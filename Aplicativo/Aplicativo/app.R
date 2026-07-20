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

# Inicializar H2O localmente
h2o.init(ip = "localhost", nthreads = -1, max_mem_size = "4G")

# ==============================================================================
# 1. UI (INTERFAZ DE USUARIO)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Módulo 1: Evaluación Masiva (Batch Processing)"),
  
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
      
      numericInput("cutoff", "Cutoff Óptimo (Probabilidad):", value = 0.5, min = 0.01, max = 0.99, step = 0.01),
      
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
                 verbatimTextOutput("texto_pe"),
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
      # Estas funciones deben estar dentro de "funciones_auxiliares.R"
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
        # =====================================================================
        # 🚨 INSTRUCCIONES PARA EL COMPAÑERO (REGRESIÓN LOGÍSTICA) 🚨
        # =====================================================================
        # 1. Pega aquí el código de entrenamiento de tu modelo GLM.
        #    Usa 'x_em', 'y_em' y 'mod_em' para el entrenamiento.
        #
        # 2. OBLIGATORIO: Tu línea de predicción DEBE llamarse 'predicciones_h2o'
        #    Ejemplo: predicciones_h2o <- h2o.predict(tu_modelo_glm, newdata = mod_em)
        #
        # 3. Borra las siguientes dos líneas (showNotification y return) una vez 
        #    que pegues tu código para que el programa continúe.
        # =====================================================================
        showNotification("El modelo de Regresión Logística aún no ha sido implementado.", type = "warning")
        return()
        
      } else if (input$modelo_seleccionado == "Gradient Boosting Machine (GBM)") {
        # =====================================================================
        # 🚨 INSTRUCCIONES PARA EL COMPAÑERO (GBM INDIVIDUAL) 🚨
        # =====================================================================
        # 1. Pega aquí el código de entrenamiento de tu modelo GBM individual.
        #    Usa 'x_em', 'y_em' y 'mod_em' para el entrenamiento.
        #
        # 2. OBLIGATORIO: Tu línea de predicción DEBE llamarse 'predicciones_h2o'
        #    Ejemplo: predicciones_h2o <- h2o.predict(tu_modelo_gbm, newdata = mod_em)
        #
        # 3. Borra las siguientes dos líneas (showNotification y return) una vez 
        #    que pegues tu código para que el programa continúe.
        # =====================================================================
        showNotification("El modelo GBM individual aún no ha sido implementado.", type = "warning")
        return()
      }
      
      # --- PROCESAMIENTO GENERAL DE RESULTADOS PARA CUALQUIER MODELO ---
      # Si el compañero llenó correctamente predicciones_h2o, este bloque procesará todo automáticamente
      if (!is.null(predicciones_h2o)) {
        
        df_pred <- as.data.frame(predicciones_h2o)
        
        # 1. Base a nivel de registro (Probabilidad y Aprobación/Rechazo)
        base_resultado <- copy(datos_originales)
        base_resultado$Probabilidad_Default_PD <- round(df_pred$p1, 4)
        base_resultado$Clasificacion_Final <- ifelse(base_resultado$Probabilidad_Default_PD > input$cutoff, "Rechazado", "Aprobado")
        
        # 2. Generación del Score y Rango con la función res_fun de "funciones_auxiliares.R"
        mod_e2m <- setDT(res_fun(datos, df_pred))
        colnames(mod_e2m)[1] <- "Var"
        
        # Integrar Score y Rango a la base de salida
        base_resultado$Score <- mod_e2m$Score
        base_resultado$Rango <- mod_e2m$Rango
        
        # Generar tabla performance
        tabla_mod_e2m <- tabla_performance(mod_e2m)
        tabla_perf_final <- tabla_mod_e2m[[1]]
        
        # 3. Cálculo de Pérdida Esperada (Requiere que los datos originales tengan 'EXPOSICION')
        if ("EXPOSICION" %in% colnames(datos)) {
          pe_s <- data.table(Var = mod_e2m$Var, Score = mod_e2m$Score, EXP = datos$EXPOSICION)
          r_s <- calcular_perdida_esperada(pe_s, LGD = 1)
          resultados_pe(r_s)
        } else {
          resultados_pe(NULL) # Si no hay exposición, se queda nulo
        }
        
        # Guardar en variables reactivas para mostrarlas en la UI y descargarlas
        resultados_base(base_resultado)
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
    DT::datatable(resultados_base(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tabla_performance_ui <- DT::renderDataTable({
    req(resultados_perf())
    DT::datatable(resultados_perf(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$texto_pe <- renderText({
    if (is.null(resultados_pe())) {
      return("La columna 'EXPOSICION' no fue encontrada en los datos de entrada. No se puede calcular la Pérdida Esperada.")
    } else {
      res <- resultados_pe()$Metricas
      paste0("PE Total: $", round(res$PE_Total, 2), 
             "\nExposición Total: $", round(res$EXP_Total, 2),
             "\nPE / Exposición: ", round(res$PE_sobre_EXP * 100, 4), "%")
    }
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
      
      # Agregar la hoja de Pérdida Esperada solo si se pudo calcular
      if (!is.null(resultados_pe())) {
        hojas[["Perdida_Esperada"]] <- resultados_pe()$PE_por_Rango
      }
      
      write.xlsx(hojas, file = file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)