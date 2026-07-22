library(shiny)
library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(lubridate)
library(expss)
library(ranger)
library(h2o)
library(formattable)
library(plotly)

# devtools::install_github("duhi23/Funciones-Auxiliares") # Descomentar si no está instalada
library(FunAuxiliares)

# Carga de funciones auxiliares propias (AQUÍ YA ESTÁN INCLUIDAS TUS FUNCIONES)
source("funciones_auxiliares.R")
options(shiny.maxRequestSize = 500 * 1024^2)

# Inicializar H2O localmente
h2o.init(ip = "localhost", nthreads = -1, max_mem_size = "4G")

# ==============================================================================
# 1. UI (INTERFAZ DE USUARIO)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Evaluación de Credit Scoring"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo_datos", "Carga de Datos (.csv o .xlsx):", 
                accept = c(".csv", ".xlsx")),
      
      selectInput("modelo_seleccionado", "Selección de Modelo:",
                  choices = c("Seleccione un modelo...",
                              "Regresión Logística", 
                              "Random Forest", 
                              "Gradient Boosting Machine (GBM)", 
                              "Ensamble (Modelo Combinado)")),
      
      # Parámetros del modelo
      tags$h4("Parámetros de Evaluación"),
      numericInput("cutoff", "Cutoff Óptimo (Probabilidad):", value = 0.5, min = 0.01, max = 0.99, step = 0.01),
      numericInput("lgd_input", "Loss Given Default (LGD):", value = 1, min = 0.01, max = 1, step = 0.01),
      
      actionButton("ejecutar_evaluacion", "Ejecutar Evaluación", class = "btn-primary", width = "100%"),
      
      tags$hr(),
      
      downloadButton("descargar_resultados", "Descargar Resultados en Excel", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados por Registro", 
                 br(),
                 DT::dataTableOutput("tabla_resultados")),
        tabPanel("Tabla Performance", 
                 br(),
                 DT::dataTableOutput("tabla_performance_ui")),
        tabPanel("Pérdida Esperada (PE)", 
                 br(),
                 uiOutput("panel_metricas_ui"),
                 br(),
                 DT::dataTableOutput("tabla_pe")),
        # --- NUEVA PESTAÑA DE GRÁFICOS INTERACTIVOS ---
        tabPanel("Gráficos Interactivos",
                 br(),
                 fluidRow(
                   # Dividimos la pantalla en dos mitades (6 y 6)
                   column(6, plotlyOutput("pie_rangos", height = "500px")),
                   column(6, plotlyOutput("hist_exposicion", height = "500px"))
                 ))
      )
    )
  )
)

# ==============================================================================
# 2. SERVER (LÓGICA DEL SERVIDOR)
# ==============================================================================
server <- function(input, output, session) {
  
  # Variables reactivas para almacenar resultados y permitir su descarga/visualización
  resultados_base <- reactiveVal(NULL)
  resultados_perf <- reactiveVal(NULL)
  resultados_pe <- reactiveVal(NULL)
  metricas_negocio <- reactiveVal(NULL)
  
  observeEvent(input$ejecutar_evaluacion, {
    req(input$archivo_datos)
    req(input$modelo_seleccionado != "Seleccione un modelo...")
    
    id_notificacion <- showNotification("Procesando datos y evaluando modelo...", type = "message", duration = NULL)
    
    tryCatch({
      message("--- PASO 1: Leyendo datos ---")
      ext <- tools::file_ext(input$archivo_datos$name)
      if (ext == "csv") {
        datos_originales <- read.csv(input$archivo_datos$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        datos_originales <- read.xlsx(input$archivo_datos$datapath)
      }
      
      datos <- copy(datos_originales)
      
      message("--- PASO 2: Ejecutando fun_acum ---")
      datos <- fun_acum(datos)
      
      message("--- PASO 3: Ejecutando fun_ratios ---")
      datos <- fun_ratios(datos)
      
      message("--- PASO 4: Ejecutando fun_comb ---")
      datos <- fun_comb(datos)
      
      # Variable principal donde todos los modelos deben depositar sus predicciones
      predicciones_h2o <- NULL
      
      message(paste("--- PASO 5: Iniciando modelo seleccionado:", input$modelo_seleccionado, "---"))
      
      # --- INICIALIZACIÓN CONDICIONAL PARA MODELOS H2O ---
      if (input$modelo_seleccionado %in% c("Random Forest", "Gradient Boosting Machine (GBM)", "Ensamble (Modelo Combinado)")) {
        
        vars <- c("VarDepF", "MAX_DVEN_SC_OP_12M", "NENT_VEN_SBS_OP_6M", "r_DEUDA_TOTAL_SCE_12a24M", "NOPE_APERT_SCE_24M",
                  "PROM_VEN_SCE_6M", "MAX_DVEN_SC_OP_3M", "MVAL_CASTIGO_SCE_OP_36M", "PROM_VEN_SBS_24M", "PROM_VEN_SBS_6M",
                  "r_NOPE_VENC_181A360_OP_12s36M")
        
        # Si explota aquí, el CSV no tiene las variables de H2O
        message("--- PASO 5.1: Creando matriz H2O ---") 
        mod_em <- as.h2o(x = setDT(datos)[, vars, with=FALSE])
        y_em <- "VarDepF"
        x_em <- setdiff(names(mod_em), y_em)
        mod_em[[y_em]] <- as.factor(mod_em[[y_em]])
        nfolds <- 5
      }
      
      # --- EJECUCIÓN DE MODELOS ---
      if (input$modelo_seleccionado == "Random Forest") {
        message("--- PASO 6: Corriendo Random Forest ---")
        my_rf <- h2o.randomForest(x = x_em, y = y_em, model_id = "RF", training_frame = mod_em,
                                  ntrees = 200, min_rows = 800, mtries = 3, nfolds = nfolds,
                                  fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        predicciones_h2o <- h2o.predict(my_rf, newdata = mod_em)
        
      } else if (input$modelo_seleccionado == "Regresión Logística") {
        message("--- PASO 6: Corriendo Regresión Logística ---")
        
        setDT(datos)
        
        # Transformación Logarítmica
        datos[, ln_CUOTA_EST_OP      := log(CUOTA_EST_OP + 1)]
        datos[, ln_EXPOSICION        := log(EXPOSICION + 1)]
        
        datos[, prbm_NOPE_VENC_OP_3M := fcase(
          NOPE_VENC_OP_3M <= 0,        0.15431,   # Nodo 8: Sin operaciones vencidas recientes (Perfil Sano)
          NOPE_VENC_OP_3M > 0,         0.68425,   # Nodo 9: Con al menos una operación vencida (Alto Riesgo)
          
          default = 0.39384                       # Media global de esta muestra (Nodo 0)
        )]
        
        datos[, prbm_ANTIGUEDAD_OP_SBS := fcase(
          ANTIGUEDAD_OP_SBS <= 0,                                   0.47179,  # Nodo 173: Sin antigüedad registrada (Perfil "Thin File" / Mayor riesgo)
          ANTIGUEDAD_OP_SBS > 0    & ANTIGUEDAD_OP_SBS <= 26.9,   0.34938,  # Nodo 174: Antigüedad incipiente (Menos de 2 años en el sistema)
          ANTIGUEDAD_OP_SBS > 26.9  & ANTIGUEDAD_OP_SBS <= 63.4,   0.33090,  # Nodo 175: Antigüedad intermedia (De 2 a 5 años de historial)
          ANTIGUEDAD_OP_SBS > 63.4  & ANTIGUEDAD_OP_SBS <= 79.2,   0.30889,  # Nodo 176: Antigüedad madura (De 5 a 6.5 años de estabilidad)
          ANTIGUEDAD_OP_SBS > 79.2,                                      0.25180,  # Nodo 177: Clientes bancarizados de larga data (Riesgo mínimo)
          
          default = 0.39384                                                           # Media global de esta muestra (Nodo 0)
        )]
        
        datos[, prbm_r_NOPE_VENC_OP_6s12M := fcase(
          r_NOPE_VENC_OP_6s12M <= 0,                                   0.14591,  # Nodo 59: Sin operaciones vencidas registradas (Perfil Muy Sano)
          r_NOPE_VENC_OP_6s12M > 0    & r_NOPE_VENC_OP_6s12M <= 0.917,   0.63762,  # Nodo 60: Presencia moderada de morosidad (Riesgo Alto)
          r_NOPE_VENC_OP_6s12M > 0.917,                                0.67146,  # Nodo 61: Frecuencia crítica de operaciones vencidas (Máximo Riesgo)
          
          default = 0.39384                                                     # Media global de esta muestra (Nodo 0)
        )]
        
        
        datos[, prbm_NENT_VEN_SCE_24M := fcase(
          NENT_VEN_SCE_24M <= 0,                                  0.10355,  # Nodo 148: Sin entidades con saldo vencido a 24M (Perfil Muy Sano)
          NENT_VEN_SCE_24M > 0    & NENT_VEN_SCE_24M <= 1,        0.62820,  # Nodo 149: Alerta de incumplimiento con 1 entidad (Pico de riesgo)
          NENT_VEN_SCE_24M > 1    & NENT_VEN_SCE_24M <= 2,        0.62411,  # Nodo 150: Incumplimiento sostenido con 2 entidades
          NENT_VEN_SCE_24M > 2    & NENT_VEN_SCE_24M <= 3,        0.53560,  # Nodo 151: Morosidad extendida en múltiples entidades
          NENT_VEN_SCE_24M > 3,                                   0.34205,  # Nodo 152: Concentración masiva de deudas multi-entidad antiguas
          
          default = 0.39384                                                 # Media global de esta muestra (Nodo 0)
        )]
        
        
        datos[, prbm_PROM_VEN_SCE_12M := fcase(
          PROM_VEN_SCE_12M <= 0,                                     0.11641,  # Nodo 142: Sin saldo vencido promedio a 12M (Perfil Impecable)
          PROM_VEN_SCE_12M > 0       & PROM_VEN_SCE_12M <= 20,       0.60281,  # Nodo 143: Alerta temprana de incumplimiento
          PROM_VEN_SCE_12M > 20      & PROM_VEN_SCE_12M <= 212.25,   0.80356,  # Nodo 144: Saldo vencido promedio activo (Pico de riesgo destructivo)
          PROM_VEN_SCE_12M > 212.25  & PROM_VEN_SCE_12M <= 627.18,   0.71580,  # Nodo 145: Incumplimiento crítico a mediano plazo
          PROM_VEN_SCE_12M > 627.18  & PROM_VEN_SCE_12M <= 2093.16,  0.51140,  # Nodo 146: Historial de vencimiento crónico envejecido
          PROM_VEN_SCE_12M > 2093.16,                                0.23320,  # Nodo 147: Cuentas severamente antiguas (Efecto regularización / recuperación)
          
          default = 0.39384                                                    # Media global de esta muestra (Nodo 0)
        )]
        
        
        datos[, prbm_MAX_DVEN_SCE_12M := fcase(
          MAX_DVEN_SCE_12M <= 0,                                     0.07069,  # Nodo 167: Sin morosidad en el último año (Excelente perfil)
          MAX_DVEN_SCE_12M > 0    & MAX_DVEN_SCE_12M <= 26,          0.15908,  # Nodo 168: Mora menor a un mes (Alerta leve)
          MAX_DVEN_SCE_12M > 26   & MAX_DVEN_SCE_12M <= 165,         0.47040,  # Nodo 169: Mora en desarrollo (Riesgo alto)
          MAX_DVEN_SCE_12M > 165  & MAX_DVEN_SCE_12M <= 360,         0.75696,  # Nodo 170: Mora crítica activa (Pico de riesgo destructivo)
          MAX_DVEN_SCE_12M > 360  & MAX_DVEN_SCE_12M <= 720,         0.58869,  # Nodo 171: Cuenta castigada reciente (Entrando a recuperación)
          MAX_DVEN_SCE_12M > 720  & MAX_DVEN_SCE_12M <= 1358,        0.64962,  # Nodo 172: Morosidad crónica de larga duración
          MAX_DVEN_SCE_12M > 1358 & MAX_DVEN_SCE_12M <= 2448,        0.52079,  # Nodo 173: Cartera severamente envejecida
          MAX_DVEN_SCE_12M > 2448,                                   0.48250,  # Nodo 174: Historial prehistórico (Efecto supervivencia/castigo total)
          
          default = 0.39384                                                    # Media global de esta muestra (Nodo 0)
        )]
        
        
        datos[, prbm_TOT_CUPO := fcase(
          TOT_CUPO <= 0,                               0.48519,  # Nodo 175: Sin cupo asignado (Máximo riesgo / Perfil desbancarizado)
          TOT_CUPO > 0       & TOT_CUPO <= 1193.49,    0.37116,  # Nodo 176: Cupo bajo / Capacidad limitada
          TOT_CUPO > 1193.49 & TOT_CUPO <= 3172,       0.26811,  # Nodo 177: Cupo intermedio
          TOT_CUPO > 3172    & TOT_CUPO <= 8395,       0.20896,  # Nodo 178: Cupo alto / Cliente consolidado
          TOT_CUPO > 8395,                             0.17716,  # Nodo 179: Cupo preferencial / Clientes Premium (Riesgo mínimo)
          
          default = 0.39384                                      # Media global de esta muestra (Nodo 0)
        )]
        
        ### Carga y Ajuste del Modelo ------
        
        modelo <- readRDS("modelo_logistico.RDS")
        datos[, PROB_LOGISTICO := predict(modelo, newdata = datos, type = "response")]
        predicciones_h2o <- data.frame(p1 = datos$PROB_LOGISTICO)
        
      } else if (input$modelo_seleccionado == "Gradient Boosting Machine (GBM)") {
        message("--- PASO 6: Corriendo GBM ---")
        my_gbm_ind <- h2o.gbm(x = x_em, y = y_em, model_id = "GBM", training_frame = mod_em,
                              ntrees = 200, max_depth = 3, min_rows = 800, learn_rate = 0.02, nfolds = nfolds,
                              fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        predicciones_h2o <- h2o.predict(my_gbm_ind, newdata = mod_em)
      }
      
      # --- PROCESAMIENTO GENERAL DE RESULTADOS ---
      if (!is.null(predicciones_h2o)) {
        message("--- PASO 7: Procesando tablas finales (res_fun) ---")
        df_pred <- as.data.frame(predicciones_h2o)
        base_resultado <- copy(datos_originales)
        
        if (!"EXPOSICION" %in% names(base_resultado)) base_resultado$EXPOSICION <- NA
        if (!"IDENTIFICACION" %in% names(base_resultado)) base_resultado$IDENTIFICACION <- NA
        
        base_resultado$Probabilidad_Default_PD <- round(df_pred$p1, 4)
        base_resultado$Clasificacion_Final <- ifelse(base_resultado$Probabilidad_Default_PD > input$cutoff, "Aprobado", "Rechazado")
        base_resultado$Perdida_Esperada <- base_resultado$Probabilidad_Default_PD * input$lgd_input * base_resultado$EXPOSICION
        
        Score_calculado <- 1000 - ceiling(1000 * df_pred$p1)
        
        # Protegemos la variable objetivo en caso de que subas un archivo de nuevos clientes sin la variable VarDepF
        Var_real <- if ("VarDepF" %in% names(datos)) datos$VarDepF else rep(NA, nrow(datos))
        
        mod_e2m <- data.table(
          Var = Var_real,
          Score = Score_calculado
        )
        
        # Función interna para calcular los deciles de forma segura (ignora los NA si los hay)
        rango_score_seguro <- function(vector, nrangos=10){
          aux <- seq_along(vector)
          res <- data.frame(id=aux, val=vector)
          res <- res[order(res$val, decreasing = TRUE, na.last = TRUE),]
          res$aux <- as.numeric(cut(aux, breaks = round(seq(0, length(vector), length.out = (nrangos+1)), 0), labels = seq(1, nrangos)))
          res <- res[order(res$id),]
          return(res$aux)
        }
        
        mod_e2m[, Rango := rango_score_seguro(Score)]
        
        base_resultado$Score <- mod_e2m$Score
        base_resultado$Rango <- mod_e2m$Rango
        
        message("--- PASO 8: Calculando métricas ---")
        
        # --- CÁLCULO DE MÉTRICAS DE NEGOCIO ---
        total_clientes <- nrow(base_resultado)
        aprobados <- base_resultado %>% filter(Clasificacion_Final == "Aprobado")
        rechazados <- base_resultado %>% filter(Clasificacion_Final == "Rechazado")
        
        pct_aprobados <- (nrow(aprobados) / total_clientes) * 100
        pct_rechazados <- (nrow(rechazados) / total_clientes) * 100
        
        monto_aprobados <- sum(aprobados$EXPOSICION, na.rm = TRUE)
        monto_rechazados <- sum(rechazados$EXPOSICION, na.rm = TRUE)
        
        provisiones_necesarias <- sum(aprobados$Perdida_Esperada, na.rm = TRUE)
        
        metricas_negocio(list(
          pct_aprobados = pct_aprobados,
          pct_rechazados = pct_rechazados,
          monto_aprobados = monto_aprobados,
          monto_rechazados = monto_rechazados,
          provisiones_necesarias = provisiones_necesarias
        ))
        
        # 3. FILTRAR COLUMNAS
        message("--- PASO 9: Filtrando columnas finales ---")
        columnas_deseadas <- c("IDENTIFICACION", "EXPOSICION", "Probabilidad_Default_PD", 
                               "Clasificacion_Final", "Score", "Rango", "Perdida_Esperada")
        
        # ⚠️ SOLUCIÓN BLINDADA AL POSIBLE ERROR AQUÍ:
        base_resultado <- as.data.frame(base_resultado) 
        columnas_existentes <- intersect(columnas_deseadas, names(base_resultado))
        base_resultado_filtrada <- base_resultado[, columnas_existentes, drop = FALSE]
        
        message("--- PASO 10: Generando tablas Performance ---")
        # Generar tabla performance
        tabla_mod_e2m <- tabla_performance(mod_e2m)
        tabla_perf_final <- tabla_mod_e2m[[1]]
        
        # 4. Cálculo de Pérdida Esperada Global
        if ("EXPOSICION" %in% colnames(datos) && any(!is.na(datos$EXPOSICION))) {
          pe_s <- data.table(Var = mod_e2m$Var, Score = mod_e2m$Score, EXP = datos$EXPOSICION)
          r_s <- calcular_perdida_esperada(pe_s, LGD = input$lgd_input)
          resultados_pe(r_s)
        } else {
          resultados_pe(NULL)
        }
        
        resultados_base(base_resultado_filtrada)
        resultados_perf(tabla_perf_final)
        showNotification("Evaluación completada con éxito.", type = "message")
      }
      
    }, error = function(e) {
      showNotification(paste("Error en la ejecución:", e$message), type = "error")
    }, finally = {
      removeNotification(id_notificacion)
    })
  })
  
  # --- RENDERIZADO DE TABLAS EN LA UI ---
  output$tabla_resultados <- DT::renderDataTable({
    req(resultados_base())
    dt_data <- resultados_base()
    
    # Format Perdida_Esperada and EXPOSICION
    if("Perdida_Esperada" %in% colnames(dt_data)) {
      dt_data$Perdida_Esperada <- round(dt_data$Perdida_Esperada, 2)
    }
    if("EXPOSICION" %in% colnames(dt_data)) {
      dt_data$EXPOSICION <- round(as.numeric(dt_data$EXPOSICION), 4)
    }
    
    DT::datatable(dt_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- TABLA PERFORMANCE ACTUALIZADA (NUEVO SEMÁFORO VERDE-AMARILLO-ROJO) ---
  output$tabla_performance_ui <- DT::renderDataTable({
    req(resultados_perf())
    dt_perf <- as.data.frame(resultados_perf())
    
    # Redondear todas las columnas numéricas a 4 decimales
    cols_num <- sapply(dt_perf, is.numeric)
    dt_perf[cols_num] <- lapply(dt_perf[cols_num], round, 4)
    
    # Aseguramos que RazonMalo se convierta a número real para que styleInterval funcione
    dt_perf$RazonMalo_Num <- as.numeric(gsub("%", "", as.character(dt_perf$RazonMalo))) / 100
    
    # Índice de la última columna agregada (RazonMalo_Num) para ocultarla.
    hidden_col_idx <- ncol(dt_perf) - 1 
    
    DT::datatable(dt_perf, 
                  options = list(
                    pageLength = 10, 
                    scrollX = TRUE,
                    # Ocultar visualmente la columna RazonMalo_Num
                    columnDefs = list(list(visible = FALSE, targets = hidden_col_idx)) 
                  ), 
                  rownames = FALSE) %>%
      DT::formatStyle(
        'RazonMalo', # Aplicar estilo a la columna visible
        valueColumns = 'RazonMalo_Num', # Usar los valores de la columna oculta
        backgroundColor = DT::styleInterval(
          c(0.10, 0.40), # Puntos de corte ajustados para verde-amarillo-rojo
          c('#c8e6c9', '#fff9c4', '#ef9a9a') # Colores: Verde claro, Amarillo claro, Rojo claro
        )
      )
  })
  
  # --- RENDERIZADO UI DE MÉTRICAS (HTML ESTILIZADO) ---
  output$panel_metricas_ui <- renderUI({
    if (is.null(resultados_pe()) || is.null(metricas_negocio())) {
      return(HTML("<div class='alert alert-warning'>La columna 'EXPOSICION' no fue encontrada en los datos de entrada o está vacía. No se puede calcular la Pérdida Esperada ni las métricas de negocio.</div>"))
    } 
    
    res <- resultados_pe()$Metricas
    mn <- metricas_negocio()
    
    HTML(paste0("
      <style>
        .metric-card { background-color: #f8f9fa; border-left: 5px solid #007bff; border-radius: 5px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .metric-card.resumen { border-color: #17a2b8; }
        .metric-card.aprobados { border-color: #28a745; }
        .metric-card.rechazados { border-color: #dc3545; }
        .metric-card.provisiones { border-color: #ffc107; background-color: #fffdf5; }
        .metric-title { font-weight: bold; color: #495057; font-size: 1.1em; margin-bottom: 5px; }
        .metric-value { font-size: 1.4em; color: #212529; font-weight: 600; }
        .metric-sub { font-size: 0.9em; color: #6c757d; }
      </style>
      
      <div class='row'>
        <div class='col-sm-12'>
          <h4 style='color: #333; border-bottom: 1px solid #ddd; padding-bottom: 5px;'>Resumen Global Histórico</h4>
        </div>
        <div class='col-sm-4'>
          <div class='metric-card resumen'>
            <div class='metric-title'>Pérdida Esperada Total</div>
            <div class='metric-value'>$", formattable::comma(round(res$PE_Total, 2)), "</div>
            <div class='metric-sub'>LGD = ", input$lgd_input, "</div>
          </div>
        </div>
        <div class='col-sm-4'>
          <div class='metric-card resumen'>
            <div class='metric-title'>Exposición Total</div>
            <div class='metric-value'>$", formattable::comma(round(res$EXP_Total, 2)), "</div>
          </div>
        </div>
        <div class='col-sm-4'>
          <div class='metric-card resumen'>
            <div class='metric-title'>Ratio PE / Exposición</div>
            <div class='metric-value'>", round(res$PE_sobre_EXP * 100, 4), "%</div>
          </div>
        </div>
      </div>
      
      <div class='row' style='margin-top: 15px;'>
        <div class='col-sm-12'>
          <h4 style='color: #333; border-bottom: 1px solid #ddd; padding-bottom: 5px;'>Métricas de Negocio (Cutoff: ", input$cutoff, ")</h4>
        </div>
        <div class='col-sm-6'>
          <div class='metric-card aprobados'>
            <div class='metric-title'>Aprobados (", round(mn$pct_aprobados, 2), "%)</div>
            <div class='metric-value'>$", formattable::comma(round(mn$monto_aprobados, 2)), "</div>
            <div class='metric-sub'>Exposición Aprobada</div>
          </div>
        </div>
        <div class='col-sm-6'>
          <div class='metric-card rechazados'>
            <div class='metric-title'>Rechazados (", round(mn$pct_rechazados, 2), "%)</div>
            <div class='metric-value'>$", formattable::comma(round(mn$monto_rechazados, 2)), "</div>
            <div class='metric-sub'>Exposición Rechazada</div>
          </div>
        </div>
        <div class='col-sm-12'>
          <div class='metric-card provisiones'>
            <div class='metric-title'>Provisiones Necesarias</div>
            <div class='metric-value'>$", formattable::comma(round(mn$provisiones_necesarias, 2)), "</div>
            <div class='metric-sub'>Pérdida Esperada generada únicamente por los créditos Aprobados</div>
          </div>
        </div>
      </div>
    "))
  })
  
  output$tabla_pe <- DT::renderDataTable({
    req(resultados_pe())
    DT::datatable(resultados_pe()$PE_por_Rango, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # --- DESCARGA EXCEL MULTI-HOJA ---
  output$descargar_resultados <- downloadHandler(
    filename = function() {
      paste0("Resultados_", gsub(" ", "_", input$modelo_seleccionado), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      hojas <- list(
        "Predicciones" = resultados_base(),
        "Performance" = resultados_perf()
      )
      
      if (!is.null(resultados_pe())) {
        hojas[["Perdida_Esperada"]] <- resultados_pe()$PE_por_Rango
      }
      
      write.xlsx(hojas, file = file, overwrite = TRUE)
    }
  )
  
  
  
  # ============================================================================
  # --- GRÁFICOS INTERACTIVOS (PLOTLY) ---
  # ============================================================================
  
  # 1. Gráfico de Pastel Interactivo (Solucionado)
  output$pie_rangos <- renderPlotly({
    req(resultados_base(), resultados_perf())
    
    df <- as.data.table(resultados_base())
    dt_perf <- as.data.table(resultados_perf())
    
    # 1. Reconstruimos el Rango
    if (!"Rango" %in% names(dt_perf)) {
      dt_perf[, Rango := 1:.N]
    }
    
    # 2. 🛡️ Calculamos los Buenos (Total - Malos) ya que tper no los exporta
    if (!"Bueno" %in% names(dt_perf)) {
      dt_perf[, Bueno := Total - Malo]
    }
    
    # 3. Calculamos la plata (exposición) agrupada por Rango
    exp_rango <- df[, .(Exposicion_Total = sum(as.numeric(EXPOSICION), na.rm = TRUE)), by = Rango]
    
    # 4. Unimos todo
    resumen_pie <- merge(dt_perf, exp_rango, by = "Rango", all.x = TRUE)
    resumen_pie <- resumen_pie[order(Rango)]
    
    # 5. Construimos el Hover (El texto al pasar el mouse)
    resumen_pie[, HoverText := paste0(
      "<b>Decil (Rango):</b> ", Rango, "<br>",
      "<b>Total Observaciones:</b> ", formattable::comma(Total, digits = 0), "<br>",
      "<b>Buenos:</b> ", formattable::comma(Bueno, digits = 0), " | <b>Malos:</b> ", formattable::comma(Malo, digits = 0), "<br>",
      "<b>Exposición:</b> $", formattable::comma(Exposicion_Total, digits = 2), "<br>",
      "<b>Rango de Score:</b> ", Min, " - ", Max
    )]
    
    # 6. Dibujamos el pastel
    plot_ly(resumen_pie, 
            labels = ~paste("Decil", Rango), 
            values = ~Total, 
            type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~HoverText,
            customdata = ~Rango, # Vital para que el clic funcione
            source = "pie_click", 
            marker = list(line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = "Distribución de Observaciones por Decil",
             showlegend = TRUE)
  })
  
  # 2. Histograma Dinámico (Despierta con el clic)
  output$hist_exposicion <- renderPlotly({
    req(resultados_base())
    
    # Capturamos el clic del usuario
    click_data <- event_data("plotly_click", source = "pie_click")
    
    # Si aún no hacen clic, mostramos el mensaje base
    if (is.null(click_data)) {
      return(plot_ly() %>% 
               layout(title = "Haz clic en un decil del pastel<br>para ver la distribución de su exposición",
                      xaxis = list(visible = FALSE), 
                      yaxis = list(visible = FALSE)))
    }
    
    # Extraemos el número del decil al que le hicieron clic
    rango_seleccionado <- click_data$customdata[[1]]
    
    # Filtramos la data original
    df <- as.data.table(resultados_base())
    df_filtrado <- df[Rango %in% rango_seleccionado]
    
    if(nrow(df_filtrado) == 0){
      return(plot_ly() %>% layout(title = "Sin datos numéricos de exposición para este decil."))
    }
    
    # Dibujamos el histograma de ese decil específico
    plot_ly(df_filtrado, x = ~as.numeric(EXPOSICION), type = "histogram", 
            marker = list(color = "#17a2b8", line = list(color = "white", width = 1)),
            opacity = 0.8) %>%
      layout(title = paste("Distribución de Exposición - Decil", rango_seleccionado),
             xaxis = list(title = "Monto de Exposición ($)", tickformat = "$,.0f"),
             yaxis = list(title = "N° de Observaciones"),
             bargap = 0.1)
  })
}

shinyApp(ui = ui, server = server)
