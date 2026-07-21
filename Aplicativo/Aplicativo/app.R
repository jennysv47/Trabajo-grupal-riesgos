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
                 DT::dataTableOutput("tabla_pe"))
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
      # --- LECTURA DE DATOS ---
      ext <- tools::file_ext(input$archivo_datos$name)
      if (ext == "csv") {
        datos_originales <- read.csv(input$archivo_datos$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        datos_originales <- read.xlsx(input$archivo_datos$datapath)
      }
      
      datos <- copy(datos_originales)
      
      # --- TRANSFORMACIONES PREVIAS ---
      datos <- fun_acum(datos)
      datos <- fun_ratios(datos)
      datos <- fun_comb(datos)
      
      # --- DEFINICIÓN DE VARIABLES PARA MODELADO ---
      vars <- c("VarDepF", "MAX_DVEN_SC_OP_12M", "NENT_VEN_SBS_OP_6M", "r_DEUDA_TOTAL_SCE_12a24M", "NOPE_APERT_SCE_24M",
                "PROM_VEN_SCE_6M", "MAX_DVEN_SC_OP_3M", "MVAL_CASTIGO_SCE_OP_36M", "PROM_VEN_SBS_24M", "PROM_VEN_SBS_6M",
                "r_NOPE_VENC_181A360_OP_12s36M")
      
      mod_em <- as.h2o(x = setDT(datos)[, vars, with=FALSE])
      y_em <- "VarDepF"
      x_em <- setdiff(names(mod_em), y_em)
      mod_em[[y_em]] <- as.factor(mod_em[[y_em]])
      nfolds <- 5
      
      # Variable principal donde todos los modelos deben depositar sus predicciones
      predicciones_h2o <- NULL
      
      # --- EJECUCIÓN DE MODELOS ---
      if (input$modelo_seleccionado == "Random Forest") {
        my_rf <- h2o.randomForest(x = x_em, y = y_em, model_id = "RF", training_frame = mod_em,
                                  ntrees = 200, min_rows = 800, mtries = 3, nfolds = nfolds,
                                  fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        predicciones_h2o <- h2o.predict(my_rf, newdata = mod_em)
        
      } else if (input$modelo_seleccionado == "Ensamble (Modelo Combinado)") {
        my_gbm <- h2o.gbm(x = x_em, y = y_em, model_id = "GBM", training_frame = mod_em, distribution = "bernoulli",
                          ntrees = 300, max_depth = 6, min_rows = 500, learn_rate = 0.02, nfolds = nfolds,
                          fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        
        my_glm <- h2o.glm(x = x_em, y = y_em, model_id = "GLM", training_frame = mod_em, alpha = 0.1,
                          remove_collinear_columns = TRUE, nfolds = nfolds, fold_assignment = "Stratified",
                          keep_cross_validation_predictions = TRUE, seed = 12345)
        
        my_nn <- h2o.deeplearning(x = x_em, y = y_em, model_id = "NeuralNetwork_CreditScoring", training_frame = mod_em, 
                                  distribution = "bernoulli", hidden = c(32, 16), activation = "RectifierWithDropout", 
                                  epochs = 50, train_samples_per_iteration = -1, l1 = 1e-5, l2 = 1e-5, 
                                  input_dropout_ratio = 0.1, hidden_dropout_ratios = c(0.2, 0.2), nfolds = nfolds,
                                  fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        
        my_ensemble <- h2o.stackedEnsemble(x = x_em, y = y_em, training_frame = mod_em, model_id = "Ensamble_Tridente",
                                           base_models = list(my_glm, my_gbm, my_nn), metalearner_algorithm = "glm" )
        predicciones_h2o <- h2o.predict(my_ensemble, newdata = mod_em)
        
      } else if (input$modelo_seleccionado == "Regresión Logística") {
        showNotification("El modelo de Regresión Logística aún no ha sido implementado.", type = "warning")
        return()
      } else if (input$modelo_seleccionado == "Gradient Boosting Machine (GBM)") {
        my_gbm_ind <- h2o.gbm(x = x_em, y = y_em, model_id = "GBM", training_frame = mod_em,
                              ntrees = 200, max_depth = 3, min_rows = 800, learn_rate = 0.02, nfolds = nfolds,
                              fold_assignment = "Stratified", keep_cross_validation_predictions = TRUE, seed = 12345)
        predicciones_h2o <- h2o.predict(my_gbm_ind, newdata = mod_em)
      }
      
      # --- PROCESAMIENTO GENERAL DE RESULTADOS ---
      if (!is.null(predicciones_h2o)) {
        
        df_pred <- as.data.frame(predicciones_h2o)
        base_resultado <- copy(datos_originales)
        
        if (!"EXPOSICION" %in% names(base_resultado)) base_resultado$EXPOSICION <- NA
        if (!"IDENTIFICACION" %in% names(base_resultado)) base_resultado$IDENTIFICACION <- NA
        
        base_resultado$Probabilidad_Default_PD <- round(df_pred$p1, 4)
        base_resultado$Clasificacion_Final <- ifelse(base_resultado$Probabilidad_Default_PD > input$cutoff, "Aprobado", "Rechazado")
        base_resultado$Perdida_Esperada <- base_resultado$Probabilidad_Default_PD * input$lgd_input * base_resultado$EXPOSICION
        
        mod_e2m <- setDT(res_fun(datos, df_pred))
        colnames(mod_e2m)[1] <- "Var"
        
        base_resultado$Score <- mod_e2m$Score
        base_resultado$Rango <- mod_e2m$Rango
        
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
        columnas_deseadas <- c("IDENTIFICACION", "EXPOSICION", "Probabilidad_Default_PD", 
                               "Clasificacion_Final", "Score", "Rango", "Perdida_Esperada")
        columnas_existentes <- intersect(columnas_deseadas, names(base_resultado))
        base_resultado_filtrada <- base_resultado[, columnas_existentes, drop = FALSE]
        
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
}

shinyApp(ui = ui, server = server)
