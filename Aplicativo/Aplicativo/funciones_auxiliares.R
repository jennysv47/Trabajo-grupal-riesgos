rango_score <- function(vector){
      index <- aux <- seq(1:length(vector))
      res <- data.frame(id=index, val=vector)
      res <- res[order(res$val, decreasing = TRUE),]
      res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
      res <- res[order(res$id),]
      return(as.numeric(res$aux))
}
res_fun <- function(valida, resultado){
      res <- data.frame(VarDep=valida$VarDep,
                        Score=1000-round(1000*as.data.frame(resultado)[,3],0),
                        Rango=rango_score(1000-round(1000*as.data.frame(resultado)[,3],0)))
      return(res)
}
tabla_performance_or<-function(data, nrangos = 10) {
      # Asegurar estructura data.table limpia
      data <- as.data.table(data)
      
      # Agrupación por VALOR del Score
      min_score <- min(data$Score, na.rm = TRUE)
      max_score <- max(data$Score, na.rm = TRUE)
      cortes_dinamicos <- seq(min_score, max_score, length.out = nrangos + 1)
      
      # Crear rangos (Score alto = Rango 1)
      data[, Rango := cut(Score, 
                          breaks = cortes_dinamicos, 
                          include.lowest = TRUE, 
                          labels = nrangos:1)]
      
      # Forzar a entero numérico puro
      data[, Rango := as.integer(as.character(Rango))]
      
      # Tabla base original
      tabla <- data[, list(Min = min(Score), Max = max(Score)), by = Rango][order(Rango)]
      tabla[nrow(tabla), 2] <- 1
      tabla[1, 3] <- 999
      
      # Conteo de casos (0 = Bueno, 1 = Malo)
      conteos_var <- data %>% 
            group_by(Rango, Var) %>% 
            summarise(Casos = n(), .groups = "drop") %>% 
            spread(key = "Var", value = "Casos", fill = 0)
      
      if (!"0" %in% colnames(conteos_var)) conteos_var$`0` <- 0
      if (!"1" %in% colnames(conteos_var)) conteos_var$`1` <- 0
      
      conteos_var <- as.data.table(conteos_var)
      
      conteos_var$Total <- conteos_var$`0` + conteos_var$`1`
      conteos_var$Z <- (conteos_var$`1`) / (conteos_var$`0` + conteos_var$`1`) 
      conteos_var$W <- (conteos_var$`0`) / (conteos_var$`0` + conteos_var$`1`) 
      conteos_var$Otros <- 0 
      conteos_var$MC <- ceiling(conteos_var$Otros * conteos_var$Z + conteos_var$`1`) 
      conteos_var$BC <- ceiling(conteos_var$Otros * conteos_var$W + conteos_var$`0`) 
      
      # Unir la tabla base con los conteos
      tabla <- merge(tabla, conteos_var, by = "Rango", all.x = TRUE)
      tabla[is.na(tabla)] <- 0
      tabla <- tabla[order(Rango)]
      
      # Re-estructuración de columnas con formattable
      tabla_final <- data.table(Rango = as.integer(tabla$Rango)) # <-- FIJA EL RANGO AQUÍ
      tabla_final$Min <- tabla$Min
      tabla_final$Max <- tabla$Max
      tabla_final$Total <- tabla$Total
      tabla_final$PTotal <- percent(tabla_final$Total / sum(tabla_final$Total), digits = 1)
      tabla_final$ATotal <- cumsum(tabla_final$PTotal)
      
      tabla_final$Malo <- tabla$MC
      tabla_final$PMalo <- percent(tabla_final$Malo / sum(tabla_final$Malo), digits = 1)
      tabla_final$AMalo <- cumsum(tabla_final$PMalo)
      tabla_final$RazonMalo <- percent(tabla_final$Malo / tabla_final$Total, digits = 1)
      
      AcumTotal <- cumsum(tabla_final$Total)
      AcumMalo  <- cumsum(tabla_final$Malo)
      tabla_final$CumMalo <- percent(AcumMalo / AcumTotal, digits = 1)
      
      tabla_final$DecumMalo <- sum(tabla_final$Malo)
      for(k in 2:nrow(tabla_final)){
            tabla_final[k, "DecumMalo"] <- sum(tabla_final$Malo) - AcumMalo[k-1]
      }
      tabla_final$PDecumMalo <- tabla_final$DecumMalo / sum(tabla_final$Malo)
      
      tabla_final$Bueno <- tabla$BC
      tabla_final$PBueno <- percent(tabla_final$Bueno / sum(tabla_final$Bueno), digits = 1)
      AcumBueno <- cumsum(tabla_final$Bueno)
      
      tabla_final$DecumBueno <- sum(tabla_final$Bueno)
      for(k in 2:nrow(tabla_final)){
            tabla_final[k, "DecumBueno"] <- sum(tabla_final$Bueno) - AcumBueno[k-1]
      }
      tabla_final$PDecumBueno <- tabla_final$DecumBueno / sum(tabla_final$Bueno)
      
      # Discriminación (KS, ROC)
      tabla_final$KS <- percent(abs(tabla_final$PDecumMalo - tabla_final$PDecumBueno), digits = 1)
      tabla_final$ROC <- percent(0, digits = 1)
      
      for(k in 1:nrow(tabla_final)){
            if(k == nrow(tabla_final)){
                  tabla_final[k, "ROC"] <- percent(tabla_final$PBueno[k] * (tabla_final$PDecumMalo[k]) / 2, digits = 1)
            } else {
                  tabla_final[k, "ROC"] <- percent(tabla_final$PBueno[k] * (tabla_final$PDecumMalo[k] + tabla_final$PDecumMalo[k+1]) / 2, digits = 1)
            }
      }
      
      # Seleccionar las 11 columnas correctas sin perder Rango
      tper <- tabla_final[, c("Rango", "Min", "Max", "Total", "PTotal", "ATotal", "Malo", "PMalo", "AMalo", "RazonMalo", "CumMalo"), with = FALSE]
      
      # Fila Total con Rango = NA_integer_ para no arruinar la clase numérica
      fila_total <- data.table(
            Rango = NA_integer_,
            Min = as.numeric(min(data$Score, na.rm = TRUE)),
            Max = as.numeric(max(data$Score, na.rm = TRUE)),
            Total = sum(tper$Total),
            PTotal = percent(1.0, digits = 1),
            ATotal = percent(1.0, digits = 1),
            Malo = sum(tper$Malo),
            PMalo = percent(1.0, digits = 1),
            AMalo = percent(1.0, digits = 1),
            RazonMalo = percent(sum(tper$Malo) / sum(tper$Total), digits = 1),
            CumMalo = percent(sum(tper$Malo) / sum(tper$Total), digits = 1)
      )
      
      tper <- rbindlist(list(tper, fila_total), use.names = TRUE)
      
      cols_num <- names(tper)[sapply(tper, is.numeric)]
      tper[, (cols_num) := lapply(.SD, round, 6), .SDcols = cols_num]
      
      KSe <- max(tabla_final$KS)
      ROCe <- sum(tabla_final$ROC)
      GINIe <- 2 * ROCe - 1
      
      res <- data.table(
            Indicador_KS = round(as.numeric(KSe), 6), 
            Indicador_ROC = round(as.numeric(ROCe), 6), 
            Indicador_GINI = round(as.numeric(GINIe), 6)
      )
      
      return(list(tper, res))
}


# ==============================================================================
# 2. FUNCIÓN: CÁLCULO DE PÉRDIDA ESPERADA (PE)
# ==============================================================================
calcular_pe <- function(dt, nrangos = 10, LGD = 1) {
      if (!all(c("Var", "Score", "EXP") %in% names(dt))) {
            stop("La data.table debe contener: 'Var', 'Score' y 'EXP'")
      }
      dt_local <- copy(dt)
      dt_local <- dt_local[Var %in% c(0, 1)]
      cat("Filas válidas:", nrow(dt_local), "\n")
      cat("Buenos (0):", dt_local[Var == 0, .N], "| Malos (1):", dt_local[Var == 1, .N], "\n\n")
      # 1. Tabla performance
      cat("Calculando performance...\n")
      perf <- tabla_performance_or(dt_local[, .(Var, Score)], nrangos)
      #perf <- tabla_performance(dt_local[, .(Var, Score)], nrangos)
      perf_tabla    <- perf[[1]]
      perf_metrics  <- perf[[2]]
      # 2. Extraer PD por Rango (quitamos fila Total con NA)
      perf_rangos <- perf_tabla[!is.na(Rango), .(Rango, RazonMalo)]
      perf_rangos[, Rango := as.integer(Rango)]
      perf_rangos[, PD := as.numeric(gsub("%", "", RazonMalo)) / 100]
      perf_rangos[, RazonMalo := NULL]
      setorder(perf_rangos, Rango)
      cat("\nPD por Rango:\n")
      print(perf_rangos)
      # 3. Cortes de Score de la tabla performance
      cortes_df <- perf_tabla[!is.na(Rango), .(Rango, Min, Max)]
      setorder(cortes_df, Rango)
      # 4. Construir breaks CORRECTOS para cut
      # Los cortes son: [Min, Max] para cada rango
      # Rango 1 = mejores scores (896-999)
      # Rango 10 = peores scores (1-99)
      # Crear breaks de menor a mayor
      breaks <- c(0)  # Empezar desde 0
      for (i in nrow(cortes_df):1) {
            # Recorrer de rango 10 a 1
            breaks <- c(breaks, cortes_df[i, Min])
      }
      breaks <- c(breaks, 1000)  # Terminar en 1000
      breaks <- unique(sort(breaks))
      cat("\nBreaks:", breaks, "\n")
      # El número de labels = número de breaks - 1
      n_labels <- length(breaks) - 1
      labels <- n_labels:1  # 10,9,8,...,1 (el intervalo más alto es rango 1)
      cat("Labels:", labels, "\n")
      # Asignar rangos
      dt_local[, Rango := as.integer(as.character(
            cut(
                  Score,
                  breaks = breaks,
                  labels = labels,
                  include.lowest = TRUE
            )
      ))]
      cat("Rangos en datos:", sort(unique(dt_local$Rango)), "\n\n")
      # Verificar que no hay rangos fuera de 1-10
            if (any(!dt_local$Rango %in% 1:10)) {
            warning("Hay rangos fuera de 1-10: ",
                    paste(setdiff(unique(dt_local$Rango), 1:10
                    ), collapse = ", "))
      }
      # 5. Merge con PD
      dt_local <- merge(dt_local, perf_rangos, by = "Rango", all.x = TRUE)
      if (any(is.na(dt_local$PD))) {
            warning("Hay NA en PD. Rangos sin PD: ",
                    paste(unique(dt_local[is.na(PD), Rango]), collapse = ", "))
            # Imputar con la PD del rango más cercano
            dt_local[is.na(PD), PD := perf_rangos[Rango == 10, PD]]
      }
      # 6. Calcular PE
      
      dt_local[, PE := EXP * LGD * PD]
      # 7. Resumen por Rango (asegurar que todas las columnas son numéricas estándar)
      resumen <- dt_local[, .(
            Casos = as.numeric(.N),
            Min_Score = as.numeric(min(Score)),
            Max_Score = as.numeric(max(Score)),
            EXP_Total = as.numeric(sum(EXP, na.rm = TRUE)),
            EXP_Promedio = as.numeric(mean(EXP, na.rm = TRUE)),
            PD_Promedio = as.numeric(mean(PD, na.rm = TRUE)),
            PE_Total = as.numeric(sum(PE, na.rm = TRUE)),
            PE_Promedio = as.numeric(mean(PE, na.rm = TRUE))
      ), by = Rango]
      setorder(resumen, Rango)
      # Agregar columnas de referencia de la performance
      resumen <- merge(resumen, cortes_df[, .(Rango, Min_Perf = Min, Max_Perf = Max)], by = "Rango", all.x = TRUE)
      # Convertir Rango a character
      resumen[, Rango := as.character(Rango)]
      # 8. Fila Total (todas las columnas como numéricas estándar)
      totales <- data.table(
            Rango = "Total",
            Casos = as.numeric(nrow(dt_local)),
            Min_Score = as.numeric(dt_local[, min(Score)]),
            Max_Score = as.numeric(dt_local[, max(Score)]),
            EXP_Total = as.numeric(dt_local[, sum(EXP, na.rm = TRUE)]),
            EXP_Promedio = as.numeric(dt_local[, mean(EXP, na.rm = TRUE)]),
            PD_Promedio = as.numeric(dt_local[, sum(Var) / .N]),
            PE_Total = as.numeric(dt_local[, sum(PE, na.rm = TRUE)]),
            PE_Promedio = as.numeric(dt_local[, mean(PE, na.rm = TRUE)]),
            Min_Perf = as.numeric(dt_local[, min(Score)]),
            Max_Perf = as.numeric(dt_local[, max(Score)])
      )
      resumen <- rbindlist(list(resumen, totales), use.names = TRUE)
      # Redondear
      cols_num <- c(
            "Casos",
            "Min_Score",
            "Max_Score",
            "Min_Perf",
            "Max_Perf",
            "EXP_Total",
            "EXP_Promedio",
            "PD_Promedio",
            "PE_Total",
            "PE_Promedio"
      )
      resumen[, (cols_num) := lapply(.SD, function(x)
            round(as.numeric(x), 4)), .SDcols = cols_num]
      # 9. Métricas globales
      metricas <- list(
            PE_Total = as.numeric(dt_local[, sum(PE, na.rm = TRUE)]),
            EXP_Total = as.numeric(dt_local[, sum(EXP, na.rm = TRUE)]),
            PE_sobre_EXP = as.numeric(dt_local[, sum(PE, na.rm = TRUE) / sum(EXP, na.rm = TRUE)]),
            PD_Global = as.numeric(dt_local[, sum(Var) / .N]),
            KS = as.numeric(perf_metrics$Indicador_KS),
            GINI = as.numeric(perf_metrics$Indicador_GINI)
      )
      cat("\n========== PÉRDIDA ESPERADA ==========\n")
      cat(sprintf("PE Total:         $%.2f\n", metricas$PE_Total))
      cat(sprintf("Exposición Total:  $%.2f\n", metricas$EXP_Total))
      cat(sprintf("PE / Exposición:   %.4f%%\n", metricas$PE_sobre_EXP *100))
      cat(sprintf("PD Global:         %.4f%%\n", metricas$PD_Global * 100))
      cat(sprintf("KS:                %.4f\n", metricas$KS))
      cat(sprintf("GINI:              %.4f\n", metricas$GINI))
      cat("=======================================\n")
      return(
            list(
                  resumen = resumen,
                  datos = dt_local,
                  tabla_performance = perf_tabla,
                  metricas = metricas
            )
      )
}
calcular_perdida_esperada <- function(pe_s, nrangos = 10, LGD = 1) {
      # 1. Hacemos una copia para no alterar tu tabla original
      data_perf <- data.table::copy(pe_s)
      
      # 2. Llamamos a TU función original. 
      # Tu script internamente modifica 'data_perf' y le agrega la columna 'Rango' perfecta.
      resultado_perf <- tabla_performance(data_perf, nrangos)
      tabla_perf <- resultado_perf[[1]]
      metricas_perf <- resultado_perf[[2]]
      
      # 3. Calcular PE usando exactamente los MISMOS Rangos que creó tu función
      pe_por_rango <- data_perf[, .(
            Min = min(Score, na.rm = TRUE),
            Max = max(Score, na.rm = TRUE),
            EXP = sum(EXP, na.rm = TRUE),       # Evita que celdas nulas dejen vacía la suma
            Malos = sum(Var == 1, na.rm = TRUE),
            Total = .N
      ), by = Rango][order(Rango)]          # order(Rango) arregla el desorden de tu primera imagen
      
      # Calculamos PD y la Pérdida Esperada
      pe_por_rango[, PD := Malos / Total]
      pe_por_rango[, PE_Rango := PD * LGD * EXP]
      
      # 4. Métricas Globales
      PE_Total_calc <- sum(pe_por_rango$PE_Rango, na.rm = TRUE)
      EXP_Total_calc <- sum(pe_por_rango$EXP, na.rm = TRUE)
      
      metricas_finales <- data.table(
            "PE_Total" = PE_Total_calc,
            "EXP_Total" = EXP_Total_calc,
            "PE_sobre_EXP" = ifelse(EXP_Total_calc > 0, PE_Total_calc / EXP_Total_calc, 0),
            "PD_Global" = mean(data_perf$Var == 1, na.rm = TRUE),
            "KS" = metricas_perf$KS,
            "GINI" = metricas_perf$GINI
      )
      
      # Limpiamos las columnas para entregar exactamente lo que necesitas
      return(list(
            "PE_por_Rango" = pe_por_rango[, .(Rango, Min, Max, EXP, Malos, PD, PE_Rango)],
            "Tabla_Performance" = tabla_perf,
            "Metricas" = metricas_finales
      ))
}



########## TABLA PERFORMANCE
library(formattable)
tabla_performance <- function(data, nrangos=10){# DataFrame con dos variables "Score" y "Var"
  rango_score <- function(vector){                         #internamente rango ordena score
    index <- aux <- seq(1:length(vector))
    res <- data.frame(id=index, val=vector)
    res <- res[order(res$val, decreasing = TRUE),]
    res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = (nrangos+1) ),0), labels = seq(1,nrangos))
    res <- res[order(res$id),]
    return(as.numeric(res$aux))
  }
  data[, Rango := rango_score(Score)] #aqui score esta escrito diferente, correguir aqui o en el otro
  # Tabla base
  tabla <- data[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
  tabla[nrow(tabla), 2] <- 1; tabla[1,3] <- 999; tabla <- tabla[,-c(1)]
  # Conteo de casos (Bueno, Malo, Indeterminados, Malo Observado y Sin desempeño)
  data <- data %>% group_by(Rango, Var) %>% summarise(Casos = n()) %>% spread(key = "Var", value = "Casos", fill = 0)
  data$Total <- data$'0' + data$'1'
  data$Z <- (data$'1')/(data$'0' + data$'1') # Tasa de malos ponderada
  data$W <- (data$'0')/(data$'0' + data$'1') # Tasa de buenos ponderada
  data$Otros <- 0 # Malos Observados + Sin desempeño
  data$MC <- ceiling(data$Otros*data$Z + data$'1') # Malos corregidos
  data$BC <- ceiling(data$Otros*data$W + data$'0') # Buenos corregidos
  # Columnas faltantes
  tabla$Total <- data$Total
  tabla$PTotal <- percent(tabla$Total/sum(tabla$Total), digits = 1)
  tabla$ATotal <- cumsum(tabla$PTotal)
  tabla$Malo <- data$MC
  tabla$PMalo <- percent(tabla$Malo/sum(tabla$Malo), digits = 1)
  tabla$AMalo <- cumsum(tabla$PMalo)
  tabla$RazonMalo <- percent(tabla$Malo/tabla$Total, digits = 1)
  tabla$AcumTotal <- cumsum(tabla$Total)
  tabla$AcumMalo <- cumsum(tabla$Malo)
  tabla$CumMalo <- percent(tabla$AcumMalo/tabla$AcumTotal, digits = 1)
  tabla$DecumMalo <- sum(tabla$Malo)
  for(k in 2:nrow(tabla)){
    tabla[k,"DecumMalo"] <- sum(tabla$Malo) - tabla[k-1,"AcumMalo"]
  }
  tabla$PDecumMalo <- tabla$DecumMalo/sum(tabla$Malo)
  tabla$Bueno <- data$BC
  tabla$PBueno <- percent(tabla$Bueno/sum(tabla$Bueno), digits = 1)
  tabla$AcumBueno <- cumsum(tabla$Bueno)
  tabla$DecumBueno <- sum(tabla$Bueno)
  for(k in 2:nrow(tabla)){
    tabla[k,"DecumBueno"] <- sum(tabla$Bueno) - tabla[k-1,"AcumBueno"]
  }
  tabla$PDecumBueno <- tabla$DecumBueno/sum(tabla$Bueno)
  tabla$KS <- percent(abs(tabla$PDecumMalo - tabla$PDecumBueno), digits = 1)
  tabla$ROC <- percent(0, digits = 1)
  for(k in 1:nrow(tabla)){
    if(k == nrow(tabla)){
      tabla[k, "ROC"] <- percent(tabla[k, "PBueno"]*(tabla[k, "PDecumMalo"])/2, digits = 1)
    } else {
      tabla[k, "ROC"] <- percent(tabla[k, "PBueno"]*(tabla[k, "PDecumMalo"] + tabla[k+1, "PDecumMalo"])/2, digits = 1)
    }
  }
  tper <- tabla[,c(1,2,3,4,5,6,7,8,9,12)]
  SumTotal <- 0
  for(k in 1:nrow(tper)){
    SumTotal <- SumTotal + tper[k,3]
  }
  SumMalos <- 0
  for(k in 1:nrow(tper)){
    SumMalos <- SumMalos + tper[k,6]
  }
  KSe <- max(tabla$KS)
  ROCe <- sum(tabla$ROC)
  GINIe <- 2*ROCe - 1
  res <- data.table("KS" = KSe, "ROC" = ROCe, "GINI" = GINIe)
  return(list(tper, res))
}
#PERDIDA ESPERADA SENCILLA



###### fun_comb
# Funcion que genera variables acumuladas combinadas --------------------------------------
fun_comb <- function(data){
  data <- data.table(data)
  #Variables acumuladas combinando los sistemas SBS, SC y SICOM
  data[, NOPE_APERT_SCE_3M := (NOPE_APERT_SBS_OP_3M + NOPE_APERT_SC_OP_3M + NOPE_APERT_SICOM_OP_3M), ]
  data[, NTC_APERT_SCE_3M := (NTC_APERT_SBS_TC_3M + NTC_APERT_SC_TC_3M + NTC_APERT_SICOM_TC_3M),]
  data[, NENT_VEN_SCE_OP_3M := (NENT_VEN_SBS_OP_3M + NENT_VEN_SC_OP_3M + NENT_VEN_SICOM_OP_3M),]
  
  data[, NOPE_APERT_SCE_6M := (NOPE_APERT_SBS_OP_6M + NOPE_APERT_SC_OP_6M + NOPE_APERT_SICOM_OP_6M), ]
  data[, NTC_APERT_SCE_6M := (NTC_APERT_SBS_TC_6M + NTC_APERT_SC_TC_6M + NTC_APERT_SICOM_TC_6M),]
  data[, NENT_VEN_SCE_OP_6M := (NENT_VEN_SBS_OP_6M + NENT_VEN_SC_OP_6M + NENT_VEN_SICOM_OP_6M),]
  
  data[, NOPE_APERT_SCE_12M := (NOPE_APERT_SBS_OP_12M + NOPE_APERT_SC_OP_12M + NOPE_APERT_SICOM_OP_12M), ]
  data[, NTC_APERT_SCE_12M := (NTC_APERT_SBS_TC_12M + NTC_APERT_SC_TC_12M + NTC_APERT_SICOM_TC_12M),]
  data[, NENT_VEN_SCE_OP_12M := (NENT_VEN_SBS_OP_12M + NENT_VEN_SC_OP_12M + NENT_VEN_SICOM_OP_12M),]
  
  data[, NOPE_APERT_SCE_24M := (NOPE_APERT_SBS_OP_24M + NOPE_APERT_SC_OP_24M + NOPE_APERT_SICOM_OP_24M), ]
  data[, NTC_APERT_SCE_24M := (NTC_APERT_SBS_TC_24M + NTC_APERT_SC_TC_24M + NTC_APERT_SICOM_TC_24M),]
  data[, NENT_VEN_SCE_OP_24M := (NENT_VEN_SBS_OP_24M + NENT_VEN_SC_OP_24M + NENT_VEN_SICOM_OP_24M),]
  
  data[, NOPE_APERT_SCE_36M := (NOPE_APERT_SBS_OP_36M + NOPE_APERT_SC_OP_36M + NOPE_APERT_SICOM_OP_36M), ]
  data[, NTC_APERT_SCE_36M := (NTC_APERT_SBS_TC_36M + NTC_APERT_SC_TC_36M + NTC_APERT_SICOM_TC_36M),]
  data[, NENT_VEN_SCE_OP_36M := (NENT_VEN_SBS_OP_36M + NENT_VEN_SC_OP_36M + NENT_VEN_SICOM_OP_36M),]
  
  data[, MVAL_DEMANDA_SCE_OP_3M := pmax(MVAL_DEMANDA_SBS_OP_3M, MVAL_DEMANDA_SC_OP_3M, MVAL_DEMANDA_SICOM_OP_3M), ]
  data[, MVAL_CASTIGO_SCE_OP_3M := pmax(MVAL_CASTIGO_SBS_OP_3M, MVAL_CASTIGO_SC_OP_3M, MVAL_CASTIGO_SICOM_OP_3M), ]
  data[, MVAL_DEMANDA_SCE_OP_6M := pmax(MVAL_DEMANDA_SBS_OP_6M, MVAL_DEMANDA_SC_OP_6M, MVAL_DEMANDA_SICOM_OP_6M), ]
  data[, MVAL_CASTIGO_SCE_OP_6M := pmax(MVAL_CASTIGO_SBS_OP_6M, MVAL_CASTIGO_SC_OP_6M, MVAL_CASTIGO_SICOM_OP_6M), ]
  data[, MVAL_DEMANDA_SCE_OP_12M := pmax(MVAL_DEMANDA_SBS_OP_12M, MVAL_DEMANDA_SC_OP_12M, MVAL_DEMANDA_SICOM_OP_12M), ]
  data[, MVAL_CASTIGO_SCE_OP_12M := pmax(MVAL_CASTIGO_SBS_OP_12M, MVAL_CASTIGO_SC_OP_12M, MVAL_CASTIGO_SICOM_OP_12M), ]
  data[, MVAL_DEMANDA_SCE_OP_24M := pmax(MVAL_DEMANDA_SBS_OP_24M, MVAL_DEMANDA_SC_OP_24M, MVAL_DEMANDA_SICOM_OP_24M), ]
  data[, MVAL_CASTIGO_SCE_OP_24M := pmax(MVAL_CASTIGO_SBS_OP_24M, MVAL_CASTIGO_SC_OP_24M, MVAL_CASTIGO_SICOM_OP_24M), ]
  data[, MVAL_DEMANDA_SCE_OP_36M := pmax(MVAL_DEMANDA_SBS_OP_36M, MVAL_DEMANDA_SC_OP_36M, MVAL_DEMANDA_SICOM_OP_36M), ]
  data[, MVAL_CASTIGO_SCE_OP_36M := pmax(MVAL_CASTIGO_SBS_OP_36M, MVAL_CASTIGO_SC_OP_36M, MVAL_CASTIGO_SICOM_OP_36M), ]
  
  data[, r_NOPE_APERT_SBSsSCE_OP_3M := ifelse(NOPE_APERT_SCE_3M > 0, NOPE_APERT_SBS_OP_3M/NOPE_APERT_SCE_3M, 0), ]
  data[, r_NOPE_APERT_SCsSCE_OP_3M := ifelse(NOPE_APERT_SCE_3M > 0, NOPE_APERT_SC_OP_3M/NOPE_APERT_SCE_3M, 0), ]
  data[, r_NOPE_APERT_SICOMsSCE_OP_3M := ifelse(NOPE_APERT_SCE_3M > 0, NOPE_APERT_SICOM_OP_3M/NOPE_APERT_SCE_3M, 0), ]
  
  data[, r_NOPE_APERT_SBSsSCE_OP_6M := ifelse(NOPE_APERT_SCE_6M > 0, NOPE_APERT_SBS_OP_6M/NOPE_APERT_SCE_6M, 0), ]
  data[, r_NOPE_APERT_SCsSCE_OP_6M := ifelse(NOPE_APERT_SCE_6M > 0, NOPE_APERT_SC_OP_6M/NOPE_APERT_SCE_6M, 0), ]
  data[, r_NOPE_APERT_SICOMsSCE_OP_6M := ifelse(NOPE_APERT_SCE_6M > 0, NOPE_APERT_SICOM_OP_6M/NOPE_APERT_SCE_6M, 0), ]
  
  data[, r_NOPE_APERT_SBSsSCE_OP_12M := ifelse(NOPE_APERT_SCE_12M > 0, NOPE_APERT_SBS_OP_12M/NOPE_APERT_SCE_12M, 0), ]
  data[, r_NOPE_APERT_SCsSCE_OP_12M := ifelse(NOPE_APERT_SCE_12M > 0, NOPE_APERT_SC_OP_12M/NOPE_APERT_SCE_12M, 0), ]
  data[, r_NOPE_APERT_SICOMsSCE_OP_12M := ifelse(NOPE_APERT_SCE_12M > 0, NOPE_APERT_SICOM_OP_12M/NOPE_APERT_SCE_12M, 0), ]
  
  data[, r_NOPE_APERT_SBSsSCE_OP_24M := ifelse(NOPE_APERT_SCE_24M > 0, NOPE_APERT_SBS_OP_24M/NOPE_APERT_SCE_24M, 0), ]
  data[, r_NOPE_APERT_SCsSCE_OP_24M := ifelse(NOPE_APERT_SCE_24M > 0, NOPE_APERT_SC_OP_24M/NOPE_APERT_SCE_24M, 0), ]
  data[, r_NOPE_APERT_SICOMsSCE_OP_24M := ifelse(NOPE_APERT_SCE_24M > 0, NOPE_APERT_SICOM_OP_24M/NOPE_APERT_SCE_24M, 0), ]
  
  data[, r_NOPE_APERT_SBSsSCE_OP_36M := ifelse(NOPE_APERT_SCE_36M > 0, NOPE_APERT_SBS_OP_36M/NOPE_APERT_SCE_36M, 0), ]
  data[, r_NOPE_APERT_SCsSCE_OP_36M := ifelse(NOPE_APERT_SCE_36M > 0, NOPE_APERT_SC_OP_36M/NOPE_APERT_SCE_36M, 0), ]
  data[, r_NOPE_APERT_SICOMsSCE_OP_36M := ifelse(NOPE_APERT_SCE_36M > 0, NOPE_APERT_SICOM_OP_36M/NOPE_APERT_SCE_36M, 0), ]
  
  data[, r_NOPE_APERT_SCE_3a6M := ifelse(NOPE_APERT_SCE_6M > 0, NOPE_APERT_SCE_3M/NOPE_APERT_SCE_6M, 0),]
  data[, r_NOPE_APERT_SCE_3a12M := ifelse(NOPE_APERT_SCE_12M > 0, NOPE_APERT_SCE_3M/NOPE_APERT_SCE_12M, 0),]
  data[, r_NOPE_APERT_SCE_6a12M := ifelse(NOPE_APERT_SCE_12M > 0, NOPE_APERT_SCE_6M/NOPE_APERT_SCE_12M, 0),]
  data[, r_NOPE_APERT_SCE_12a24M := ifelse(NOPE_APERT_SCE_24M > 0, NOPE_APERT_SCE_12M/NOPE_APERT_SCE_24M, 0),]
  data[, r_NOPE_APERT_SCE_12a36M := ifelse(NOPE_APERT_SCE_36M > 0, NOPE_APERT_SCE_12M/NOPE_APERT_SCE_36M, 0),]
  
  data[, r_NTC_APERT_SCE_3a6M := ifelse(NTC_APERT_SCE_6M > 0, NTC_APERT_SCE_3M/NTC_APERT_SCE_6M, 0),]
  data[, r_NTC_APERT_SCE_3a12M := ifelse(NTC_APERT_SCE_12M > 0, NTC_APERT_SCE_3M/NTC_APERT_SCE_12M, 0),]
  data[, r_NTC_APERT_SCE_6a12M := ifelse(NTC_APERT_SCE_12M > 0, NTC_APERT_SCE_6M/NTC_APERT_SCE_12M, 0),]
  data[, r_NTC_APERT_SCE_12a24M := ifelse(NTC_APERT_SCE_24M > 0, NTC_APERT_SCE_12M/NTC_APERT_SCE_24M, 0),]
  data[, r_NTC_APERT_SCE_12a36M := ifelse(NTC_APERT_SCE_36M > 0, NTC_APERT_SCE_12M/NTC_APERT_SCE_36M, 0),]
  
  data[, r_DEUDA_TOTAL_SBSsSCE_3M := ifelse(DEUDA_TOTAL_SCE_3M > 0 , (DEUDA_TOTAL_SBS_OP_3M + DEUDA_TOTAL_SBS_TC_3M)/DEUDA_TOTAL_SCE_3M, 0),]
  data[, r_DEUDA_TOTAL_SBSsSCE_6M := ifelse(DEUDA_TOTAL_SCE_6M > 0 , (DEUDA_TOTAL_SBS_OP_6M + DEUDA_TOTAL_SBS_TC_6M)/DEUDA_TOTAL_SCE_6M, 0),]
  data[, r_DEUDA_TOTAL_SBSsSCE_12M := ifelse(DEUDA_TOTAL_SCE_12M > 0 , (DEUDA_TOTAL_SBS_OP_12M + DEUDA_TOTAL_SBS_TC_12M)/DEUDA_TOTAL_SCE_12M, 0),]
  data[, r_DEUDA_TOTAL_SBSsSCE_24M := ifelse(DEUDA_TOTAL_SCE_24M > 0 , (DEUDA_TOTAL_SBS_OP_24M + DEUDA_TOTAL_SBS_TC_24M)/DEUDA_TOTAL_SCE_24M, 0),]
  
  data[, r_DEUDA_TOTAL_SCsSCE_3M := ifelse(DEUDA_TOTAL_SCE_3M > 0 , (DEUDA_TOTAL_SC_OP_3M + DEUDA_TOTAL_SC_TC_3M)/DEUDA_TOTAL_SCE_3M, 0),]
  data[, r_DEUDA_TOTAL_SCsSCE_6M := ifelse(DEUDA_TOTAL_SCE_6M > 0 , (DEUDA_TOTAL_SC_OP_6M + DEUDA_TOTAL_SC_TC_6M)/DEUDA_TOTAL_SCE_6M, 0),]
  data[, r_DEUDA_TOTAL_SCsSCE_12M := ifelse(DEUDA_TOTAL_SCE_12M > 0 , (DEUDA_TOTAL_SC_OP_12M + DEUDA_TOTAL_SC_TC_12M)/DEUDA_TOTAL_SCE_12M, 0),]
  data[, r_DEUDA_TOTAL_SCsSCE_24M := ifelse(DEUDA_TOTAL_SCE_24M > 0 , (DEUDA_TOTAL_SC_OP_24M + DEUDA_TOTAL_SC_TC_24M)/DEUDA_TOTAL_SCE_24M, 0),]
  
  data[, r_DEUDA_TOTAL_SICOMsSCE_3M := ifelse(DEUDA_TOTAL_SCE_3M > 0 , (DEUDA_TOTAL_SICOM_OP_3M + DEUDA_TOTAL_SICOM_TC_3M)/DEUDA_TOTAL_SCE_3M, 0),]
  data[, r_DEUDA_TOTAL_SICOMsSCE_6M := ifelse(DEUDA_TOTAL_SCE_6M > 0 , (DEUDA_TOTAL_SICOM_OP_6M + DEUDA_TOTAL_SICOM_TC_6M)/DEUDA_TOTAL_SCE_6M, 0),]
  data[, r_DEUDA_TOTAL_SICOMsSCE_12M := ifelse(DEUDA_TOTAL_SCE_12M > 0 , (DEUDA_TOTAL_SICOM_OP_12M + DEUDA_TOTAL_SICOM_TC_12M)/DEUDA_TOTAL_SCE_12M, 0),]
  data[, r_DEUDA_TOTAL_SICOMsSCE_24M := ifelse(DEUDA_TOTAL_SCE_24M > 0 , (DEUDA_TOTAL_SICOM_OP_24M + DEUDA_TOTAL_SICOM_TC_24M)/DEUDA_TOTAL_SCE_24M, 0),]
  
  data[, r_DEUDA_TOTAL_SCE_3a6M := ifelse(DEUDA_TOTAL_SCE_6M > 0, DEUDA_TOTAL_SCE_3M/DEUDA_TOTAL_SCE_6M, 0),]
  data[, r_DEUDA_TOTAL_SCE_3a12M := ifelse(DEUDA_TOTAL_SCE_12M > 0, DEUDA_TOTAL_SCE_3M/DEUDA_TOTAL_SCE_12M, 0),]
  data[, r_DEUDA_TOTAL_SCE_6a12M := ifelse(DEUDA_TOTAL_SCE_12M > 0, DEUDA_TOTAL_SCE_6M/DEUDA_TOTAL_SCE_12M, 0),]
  data[, r_DEUDA_TOTAL_SCE_12a24M := ifelse(DEUDA_TOTAL_SCE_24M > 0, DEUDA_TOTAL_SCE_12M/DEUDA_TOTAL_SCE_24M, 0),]
  return(data)
}

#####Generacion variables acumuladas & ratios 
# Funcion que genera variables acumuladas --------------------------------------
fun_acum <- function(data){
  data <- data.table(data)
  # Variables Acumuladas Adicionales ----------------------------------------------------
  data[, NTC_APERT_SBS_TC_3M := ifelse(DEUDA_TOTAL_SBS_TC_3M == 0, 0, NTC_APERT_SBS_TC_3M)]
  data[, NTC_APERT_SBS_TC_6M := ifelse(DEUDA_TOTAL_SBS_TC_6M == 0, 0, NTC_APERT_SBS_TC_6M)]
  data[, NTC_APERT_SBS_TC_12M := ifelse(DEUDA_TOTAL_SBS_TC_12M == 0, 0, NTC_APERT_SBS_TC_12M)]
  data[, NTC_APERT_SBS_TC_24M := ifelse(DEUDA_TOTAL_SBS_TC_24M == 0, 0, NTC_APERT_SBS_TC_24M)]
  
  data[, NTC_APERT_SC_TC_3M := ifelse(DEUDA_TOTAL_SC_TC_3M == 0, 0, NTC_APERT_SC_TC_3M)]
  data[, NTC_APERT_SC_TC_6M := ifelse(DEUDA_TOTAL_SC_TC_6M == 0, 0, NTC_APERT_SC_TC_6M)]
  data[, NTC_APERT_SC_TC_12M := ifelse(DEUDA_TOTAL_SC_TC_12M == 0, 0, NTC_APERT_SC_TC_12M)]
  data[, NTC_APERT_SC_TC_24M := ifelse(DEUDA_TOTAL_SC_TC_24M == 0, 0, NTC_APERT_SC_TC_24M)]
  
  data[, NTC_APERT_SICOM_TC_3M := ifelse(DEUDA_TOTAL_SICOM_TC_3M == 0, 0, NTC_APERT_SICOM_TC_3M)]
  data[, NTC_APERT_SICOM_TC_6M := ifelse(DEUDA_TOTAL_SICOM_TC_6M == 0, 0, NTC_APERT_SICOM_TC_6M)]
  data[, NTC_APERT_SICOM_TC_12M := ifelse(DEUDA_TOTAL_SICOM_TC_12M == 0, 0, NTC_APERT_SICOM_TC_12M)]
  data[, NTC_APERT_SICOM_TC_24M := ifelse(DEUDA_TOTAL_SICOM_TC_24M == 0, 0, NTC_APERT_SICOM_TC_24M)]
  
  data[, NTC_APERT_OTROS_TC_3M := ifelse(DEUDA_TOTAL_OTROS_TC_3M == 0, 0, NTC_APERT_OTROS_TC_3M)]
  data[, NTC_APERT_OTROS_TC_6M := ifelse(DEUDA_TOTAL_OTROS_TC_6M == 0, 0, NTC_APERT_OTROS_TC_6M)]
  data[, NTC_APERT_OTROS_TC_12M := ifelse(DEUDA_TOTAL_OTROS_TC_12M == 0, 0, NTC_APERT_OTROS_TC_12M)]
  data[, NTC_APERT_OTROS_TC_24M := ifelse(DEUDA_TOTAL_OTROS_TC_24M == 0, 0, NTC_APERT_OTROS_TC_24M)]
  
  data[, DEUDA_TOTAL_SCE_24M := sum(DEUDA_TOTAL_SBS_OP_24M, DEUDA_TOTAL_SC_OP_24M, DEUDA_TOTAL_SICOM_OP_24M,
                                    DEUDA_TOTAL_OTROS_OP_24M, DEUDA_TOTAL_SBS_TC_24M, DEUDA_TOTAL_SC_TC_24M,
                                    DEUDA_TOTAL_SICOM_TC_24M, DEUDA_TOTAL_OTROS_TC_24M), by="IDENTIFICACION"]
  data[, DEUDA_TOTAL_SCE_12M := sum(DEUDA_TOTAL_SBS_OP_12M, DEUDA_TOTAL_SC_OP_12M, DEUDA_TOTAL_SICOM_OP_12M,
                                    DEUDA_TOTAL_OTROS_OP_12M, DEUDA_TOTAL_SBS_TC_12M, DEUDA_TOTAL_SC_TC_12M,
                                    DEUDA_TOTAL_SICOM_TC_12M, DEUDA_TOTAL_OTROS_TC_12M), by="IDENTIFICACION"]
  data[, DEUDA_TOTAL_SCE_6M := sum(DEUDA_TOTAL_SBS_OP_6M, DEUDA_TOTAL_SC_OP_6M, DEUDA_TOTAL_SICOM_OP_6M,
                                   DEUDA_TOTAL_OTROS_OP_6M, DEUDA_TOTAL_SBS_TC_6M, DEUDA_TOTAL_SC_TC_6M,
                                   DEUDA_TOTAL_SICOM_TC_6M, DEUDA_TOTAL_OTROS_TC_6M), by="IDENTIFICACION"]
  data[, DEUDA_TOTAL_SCE_3M := sum(DEUDA_TOTAL_SBS_OP_3M, DEUDA_TOTAL_SC_OP_3M, DEUDA_TOTAL_SICOM_OP_3M,
                                   DEUDA_TOTAL_OTROS_OP_3M, DEUDA_TOTAL_SBS_TC_3M, DEUDA_TOTAL_SC_TC_3M,
                                   DEUDA_TOTAL_SICOM_TC_3M, DEUDA_TOTAL_OTROS_TC_3M), by="IDENTIFICACION"]
  data[, ANTIGUEDAD_SCE := pmax(ANTIGUEDAD_OP_SBS, ANTIGUEDAD_TC_SBS, ANTIGUEDAD_OP_SC, ANTIGUEDAD_TC_SC, 
                                ANTIGUEDAD_OP_SICOM, ANTIGUEDAD_TC_SICOM)]
  
  data[, DEUDA_TOTAL_SBS_SC_24M := sum(DEUDA_TOTAL_SBS_OP_24M, DEUDA_TOTAL_SC_OP_24M, DEUDA_TOTAL_SBS_TC_24M, 
                                       DEUDA_TOTAL_SC_TC_24M), by="IDENTIFICACION"]
  data[, DEUDA_TOTAL_SBS_SC_12M := sum(DEUDA_TOTAL_SBS_OP_12M, DEUDA_TOTAL_SC_OP_12M, DEUDA_TOTAL_SBS_TC_12M, 
                                       DEUDA_TOTAL_SC_TC_12M), by="IDENTIFICACION"]
  data[, ANTIGUEDAD_SBS_SC := pmax(ANTIGUEDAD_OP_SBS, ANTIGUEDAD_TC_SBS, ANTIGUEDAD_OP_SC, ANTIGUEDAD_TC_SC)]
  
  data[, PROM_VEN_SCE_36M := sum(PROM_VEN_SBS_OP_36M, PROM_DEM_SBS_OP_36M, PROM_CAS_SBS_OP_36M,
                                 PROM_VEN_SC_OP_36M, PROM_DEM_SC_OP_36M, PROM_CAS_SC_OP_36M,
                                 PROM_VEN_SICOM_OP_36M, PROM_DEM_SICOM_OP_36M, PROM_CAS_SICOM_OP_36M,
                                 PROM_VEN_OTROS_OP_36M, PROM_DEM_OTROS_OP_36M, PROM_CAS_OTROS_OP_36M), by="IDENTIFICACION"]
  # data[, PROM_VEN_SCE_24M := sum(PROM_VEN_SBS_OP_24M, PROM_DEM_SBS_OP_24M, PROM_CAS_SBS_OP_24M,
  #                                 PROM_VEN_SC_OP_24M, PROM_DEM_SC_OP_24M, PROM_CAS_SC_OP_24M,
  #                                 PROM_VEN_SICOM_OP_24M, PROM_DEM_SICOM_OP_24M, PROM_CAS_SICOM_OP_24M,
  #                                 PROM_VEN_OTROS_OP_24M, PROM_DEM_OTROS_OP_24M, PROM_CAS_OTROS_OP_24M), by="IDENTIFICACION"]
  data[, PROM_VEN_SCE_12M := sum(PROM_VEN_SBS_OP_12M, PROM_DEM_SBS_OP_12M, PROM_CAS_SBS_OP_12M,
                                 PROM_VEN_SC_OP_12M, PROM_DEM_SC_OP_12M, PROM_CAS_SC_OP_12M,
                                 PROM_VEN_SICOM_OP_12M, PROM_DEM_SICOM_OP_12M, PROM_CAS_SICOM_OP_12M,
                                 PROM_VEN_OTROS_OP_12M, PROM_DEM_OTROS_OP_12M, PROM_CAS_OTROS_OP_12M), by="IDENTIFICACION"]
  
  data[, MAX_DVEN_SCE_36M := pmax(MAX_DVEN_SBS_OP_36M, MAX_DVEN_SC_OP_36M, MAX_DVEN_SICOM_OP_36M,
                                  MAX_DVEN_OTROS_SIS_OP_36M, MAX_DVEN_SBS_TC_36M, MAX_DVEN_SC_TC_36M,
                                  MAX_DVEN_SICOM_TC_36M, MAX_DVEN_OTROS_SIS_TC_36M)]
  data[, MAX_DVEN_SCE_24M := pmax(MAX_DVEN_SBS_OP_24M, MAX_DVEN_SC_OP_24M, MAX_DVEN_SICOM_OP_24M,
                                  MAX_DVEN_OTROS_SIS_OP_24M, MAX_DVEN_SBS_TC_24M, MAX_DVEN_SC_TC_24M,
                                  MAX_DVEN_SICOM_TC_24M, MAX_DVEN_OTROS_SIS_TC_24M)]
  data[, MAX_DVEN_SCE_12M := pmax(MAX_DVEN_SBS_OP_12M, MAX_DVEN_SC_OP_12M, MAX_DVEN_SICOM_OP_12M,
                                  MAX_DVEN_OTROS_SIS_OP_12M, MAX_DVEN_SBS_TC_12M, MAX_DVEN_SC_TC_12M,
                                  MAX_DVEN_SICOM_TC_12M, MAX_DVEN_OTROS_SIS_TC_12M)]
  
  data[, NOPE_VENC_31AMAS_OP_3M := sum(NOPE_VENC_31A90_OP_3M, NOPE_VENC_91A180_OP_3M, NOPE_VENC_181A360_OP_3M,
                                       NOPE_VENC_MAYOR360_OP_3M, NOPE_DEMANDA_OP_3M, NOPE_CASTIGO_OP_3M), by="IDENTIFICACION"]
  data[, NOPE_VENC_31AMAS_OP_6M := sum(NOPE_VENC_31A90_OP_6M, NOPE_VENC_91A180_OP_6M, NOPE_VENC_181A360_OP_6M,
                                       NOPE_VENC_MAYOR360_OP_6M, NOPE_DEMANDA_OP_6M, NOPE_CASTIGO_OP_6M), by="IDENTIFICACION"]
  data[, NOPE_VENC_31AMAS_OP_12M := sum(NOPE_VENC_31A90_OP_12M, NOPE_VENC_91A180_OP_12M, NOPE_VENC_181A360_OP_12M,
                                        NOPE_VENC_MAYOR360_OP_12M, NOPE_DEMANDA_OP_12M, NOPE_CASTIGO_OP_12M), by="IDENTIFICACION"]
  data[, NOPE_VENC_31AMAS_OP_24M := sum(NOPE_VENC_31A90_OP_24M, NOPE_VENC_91A180_OP_24M, NOPE_VENC_181A360_OP_24M,
                                        NOPE_VENC_MAYOR360_OP_24M, NOPE_DEMANDA_OP_24M, NOPE_CASTIGO_OP_24M), by="IDENTIFICACION"]
  data[, NOPE_VENC_31AMAS_OP_36M := sum(NOPE_VENC_31A90_OP_36M, NOPE_VENC_91A180_OP_36M, NOPE_VENC_181A360_OP_36M,
                                        NOPE_VENC_MAYOR360_OP_36M, NOPE_DEMANDA_OP_36M, NOPE_CASTIGO_OP_36M), by="IDENTIFICACION"]
  
  data[, PROM_DEUDA_TOTAL_SBS_OP_3M := (PROM_XVEN_SBS_OP_3M + PROM_NDI_SBS_OP_3M + PROM_VEN_SBS_OP_3M + PROM_DEM_SBS_OP_3M + PROM_CAS_SBS_OP_3M), ]
  data[, PROM_DEUDA_TOTAL_SBS_OP_6M := (PROM_XVEN_SBS_OP_6M + PROM_NDI_SBS_OP_6M + PROM_VEN_SBS_OP_6M + PROM_DEM_SBS_OP_6M + PROM_CAS_SBS_OP_6M), ]
  data[, PROM_DEUDA_TOTAL_SBS_OP_12M := (PROM_XVEN_SBS_OP_12M + PROM_NDI_SBS_OP_12M + PROM_VEN_SBS_OP_12M + PROM_DEM_SBS_OP_12M + PROM_CAS_SBS_OP_12M), ]
  data[, PROM_DEUDA_TOTAL_SBS_OP_24M := (PROM_XVEN_SBS_OP_24M + PROM_NDI_SBS_OP_24M + PROM_VEN_SBS_OP_24M + PROM_DEM_SBS_OP_24M + PROM_CAS_SBS_OP_24M), ]
  data[, PROM_DEUDA_TOTAL_SBS_OP_36M := (PROM_XVEN_SBS_OP_36M + PROM_NDI_SBS_OP_36M + PROM_VEN_SBS_OP_36M + PROM_DEM_SBS_OP_36M + PROM_CAS_SBS_OP_36M), ]
  
  data[, PROM_DEUDA_TOTAL_SC_OP_3M := (PROM_XVEN_SC_OP_3M + PROM_NDI_SC_OP_3M + PROM_VEN_SC_OP_3M + PROM_DEM_SC_OP_3M + PROM_CAS_SC_OP_3M), ]
  data[, PROM_DEUDA_TOTAL_SC_OP_6M := (PROM_XVEN_SC_OP_6M + PROM_NDI_SC_OP_6M + PROM_VEN_SC_OP_6M + PROM_DEM_SC_OP_6M + PROM_CAS_SC_OP_6M), ]
  data[, PROM_DEUDA_TOTAL_SC_OP_12M := (PROM_XVEN_SC_OP_12M + PROM_NDI_SC_OP_12M + PROM_VEN_SC_OP_12M + PROM_DEM_SC_OP_12M + PROM_CAS_SC_OP_12M), ]
  data[, PROM_DEUDA_TOTAL_SC_OP_24M := (PROM_XVEN_SC_OP_24M + PROM_NDI_SC_OP_24M + PROM_VEN_SC_OP_24M + PROM_DEM_SC_OP_24M + PROM_CAS_SC_OP_24M), ]
  data[, PROM_DEUDA_TOTAL_SC_OP_36M := (PROM_XVEN_SC_OP_36M + PROM_NDI_SC_OP_36M + PROM_VEN_SC_OP_36M + PROM_DEM_SC_OP_36M + PROM_CAS_SC_OP_36M), ]
  
  data[, PROM_DEUDA_TOTAL_SICOM_OP_3M := (PROM_XVEN_SICOM_OP_3M + PROM_NDI_SICOM_OP_3M + PROM_VEN_SICOM_OP_3M + PROM_DEM_SICOM_OP_3M + PROM_CAS_SICOM_OP_3M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_OP_6M := (PROM_XVEN_SICOM_OP_6M + PROM_NDI_SICOM_OP_6M + PROM_VEN_SICOM_OP_6M + PROM_DEM_SICOM_OP_6M + PROM_CAS_SICOM_OP_6M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_OP_12M := (PROM_XVEN_SICOM_OP_12M + PROM_NDI_SICOM_OP_12M + PROM_VEN_SICOM_OP_12M + PROM_DEM_SICOM_OP_12M + PROM_CAS_SICOM_OP_12M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_OP_24M := (PROM_XVEN_SICOM_OP_24M + PROM_NDI_SICOM_OP_24M + PROM_VEN_SICOM_OP_24M + PROM_DEM_SICOM_OP_24M + PROM_CAS_SICOM_OP_24M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_OP_36M := (PROM_XVEN_SICOM_OP_36M + PROM_NDI_SICOM_OP_36M + PROM_VEN_SICOM_OP_36M + PROM_DEM_SICOM_OP_36M + PROM_CAS_SICOM_OP_36M), ]
  
  data[, PROM_DEUDA_TOTAL_OTROS_OP_3M := (PROM_XVEN_OTROS_OP_3M + PROM_NDI_OTROS_OP_3M + PROM_VEN_OTROS_OP_3M + PROM_DEM_OTROS_OP_3M + PROM_CAS_OTROS_OP_3M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_OP_6M := (PROM_XVEN_OTROS_OP_6M + PROM_NDI_OTROS_OP_6M + PROM_VEN_OTROS_OP_6M + PROM_DEM_OTROS_OP_6M + PROM_CAS_OTROS_OP_6M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_OP_12M := (PROM_XVEN_OTROS_OP_12M + PROM_NDI_OTROS_OP_12M + PROM_VEN_OTROS_OP_12M + PROM_DEM_OTROS_OP_12M + PROM_CAS_OTROS_OP_12M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_OP_24M := (PROM_XVEN_OTROS_OP_24M + PROM_NDI_OTROS_OP_24M + PROM_VEN_OTROS_OP_24M + PROM_DEM_OTROS_OP_24M + PROM_CAS_OTROS_OP_24M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_OP_36M := (PROM_XVEN_OTROS_OP_36M + PROM_NDI_OTROS_OP_36M + PROM_VEN_OTROS_OP_36M + PROM_DEM_OTROS_OP_36M + PROM_CAS_OTROS_OP_36M), ]
  
  data[, NTC_VENC_31AMAS_TC_3M := (NTC_VENC_31A90_TC_3M + NTC_VENC_91A180_TC_3M + NTC_VENC_181A360_TC_3M + NTC_VENC_MAYOR360_TC_3M + NTC_DEMANDA_TC_3M + NTC_CASTIGO_TC_3M), ]
  data[, NTC_VENC_31AMAS_TC_6M := (NTC_VENC_31A90_TC_6M + NTC_VENC_91A180_TC_6M + NTC_VENC_181A360_TC_6M + NTC_VENC_MAYOR360_TC_6M + NTC_DEMANDA_TC_6M + NTC_CASTIGO_TC_6M), ]
  data[, NTC_VENC_31AMAS_TC_12M := (NTC_VENC_31A90_TC_12M + NTC_VENC_91A180_TC_12M + NTC_VENC_181A360_TC_12M + NTC_VENC_MAYOR360_TC_12M + NTC_DEMANDA_TC_12M + NTC_CASTIGO_TC_12M), ]
  data[, NTC_VENC_31AMAS_TC_24M := (NTC_VENC_31A90_TC_24M + NTC_VENC_91A180_TC_24M + NTC_VENC_181A360_TC_24M + NTC_VENC_MAYOR360_TC_24M + NTC_DEMANDA_TC_24M + NTC_CASTIGO_TC_24M), ]
  data[, NTC_VENC_31AMAS_TC_36M := (NTC_VENC_31A90_TC_36M + NTC_VENC_91A180_TC_36M + NTC_VENC_181A360_TC_36M + NTC_VENC_MAYOR360_TC_36M + NTC_DEMANDA_TC_36M + NTC_CASTIGO_TC_36M), ]
  
  data[, PROM_DEUDA_TOTAL_SBS_TC_3M := (PROM_XVEN_SBS_TC_3M + PROM_NDI_SBS_TC_3M + PROM_VEN_SBS_TC_3M + PROM_DEM_SBS_TC_3M + PROM_CAS_SBS_TC_3M), ]
  data[, PROM_DEUDA_TOTAL_SBS_TC_6M := (PROM_XVEN_SBS_TC_6M + PROM_NDI_SBS_TC_6M + PROM_VEN_SBS_TC_6M + PROM_DEM_SBS_TC_6M + PROM_CAS_SBS_TC_6M), ]
  data[, PROM_DEUDA_TOTAL_SBS_TC_12M := (PROM_XVEN_SBS_TC_12M + PROM_NDI_SBS_TC_12M + PROM_VEN_SBS_TC_12M + PROM_DEM_SBS_TC_12M + PROM_CAS_SBS_TC_12M), ]
  data[, PROM_DEUDA_TOTAL_SBS_TC_24M := (PROM_XVEN_SBS_TC_24M + PROM_NDI_SBS_TC_24M + PROM_VEN_SBS_TC_24M + PROM_DEM_SBS_TC_24M + PROM_CAS_SBS_TC_24M), ]
  data[, PROM_DEUDA_TOTAL_SBS_TC_36M := (PROM_XVEN_SBS_TC_36M + PROM_NDI_SBS_TC_36M + PROM_VEN_SBS_TC_36M + PROM_DEM_SBS_TC_36M + PROM_CAS_SBS_TC_36M), ]
  
  data[, PROM_DEUDA_TOTAL_SC_TC_3M := (PROM_XVEN_SC_TC_3M + PROM_NDI_SC_TC_3M + PROM_VEN_SC_TC_3M + PROM_DEM_SC_TC_3M + PROM_CAS_SC_TC_3M), ]
  data[, PROM_DEUDA_TOTAL_SC_TC_6M := (PROM_XVEN_SC_TC_6M + PROM_NDI_SC_TC_6M + PROM_VEN_SC_TC_6M + PROM_DEM_SC_TC_6M + PROM_CAS_SC_TC_6M), ]
  data[, PROM_DEUDA_TOTAL_SC_TC_12M := (PROM_XVEN_SC_TC_12M + PROM_NDI_SC_TC_12M + PROM_VEN_SC_TC_12M + PROM_DEM_SC_TC_12M + PROM_CAS_SC_TC_12M), ]
  data[, PROM_DEUDA_TOTAL_SC_TC_24M := (PROM_XVEN_SC_TC_24M + PROM_NDI_SC_TC_24M + PROM_VEN_SC_TC_24M + PROM_DEM_SC_TC_24M + PROM_CAS_SC_TC_24M), ]
  data[, PROM_DEUDA_TOTAL_SC_TC_36M := (PROM_XVEN_SC_TC_36M + PROM_NDI_SC_TC_36M + PROM_VEN_SC_TC_36M + PROM_DEM_SC_TC_36M + PROM_CAS_SC_TC_36M), ]
  
  data[, PROM_DEUDA_TOTAL_SICOM_TC_3M := (PROM_XVEN_SICOM_TC_3M + PROM_NDI_SICOM_TC_3M + PROM_VEN_SICOM_TC_3M + PROM_DEM_SICOM_TC_3M + PROM_CAS_SICOM_TC_3M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_TC_6M := (PROM_XVEN_SICOM_TC_6M + PROM_NDI_SICOM_TC_6M + PROM_VEN_SICOM_TC_6M + PROM_DEM_SICOM_TC_6M + PROM_CAS_SICOM_TC_6M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_TC_12M := (PROM_XVEN_SICOM_TC_12M + PROM_NDI_SICOM_TC_12M + PROM_VEN_SICOM_TC_12M + PROM_DEM_SICOM_TC_12M + PROM_CAS_SICOM_TC_12M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_TC_24M := (PROM_XVEN_SICOM_TC_24M + PROM_NDI_SICOM_TC_24M + PROM_VEN_SICOM_TC_24M + PROM_DEM_SICOM_TC_24M + PROM_CAS_SICOM_TC_24M), ]
  data[, PROM_DEUDA_TOTAL_SICOM_TC_36M := (PROM_XVEN_SICOM_TC_36M + PROM_NDI_SICOM_TC_36M + PROM_VEN_SICOM_TC_36M + PROM_DEM_SICOM_TC_36M + PROM_CAS_SICOM_TC_36M), ]
  
  data[, PROM_DEUDA_TOTAL_OTROS_TC_3M := (PROM_XVEN_OTROS_TC_3M + PROM_NDI_OTROS_TC_3M + PROM_VEN_OTROS_TC_3M + PROM_DEM_OTROS_TC_3M + PROM_CAS_OTROS_TC_3M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_TC_6M := (PROM_XVEN_OTROS_TC_6M + PROM_NDI_OTROS_TC_6M + PROM_VEN_OTROS_TC_6M + PROM_DEM_OTROS_TC_6M + PROM_CAS_OTROS_TC_6M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_TC_12M := (PROM_XVEN_OTROS_TC_12M + PROM_NDI_OTROS_TC_12M + PROM_VEN_OTROS_TC_12M + PROM_DEM_OTROS_TC_12M + PROM_CAS_OTROS_TC_12M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_TC_24M := (PROM_XVEN_OTROS_TC_24M + PROM_NDI_OTROS_TC_24M + PROM_VEN_OTROS_TC_24M + PROM_DEM_OTROS_TC_24M + PROM_CAS_OTROS_TC_24M), ]
  data[, PROM_DEUDA_TOTAL_OTROS_TC_36M := (PROM_XVEN_OTROS_TC_36M + PROM_NDI_OTROS_TC_36M + PROM_VEN_OTROS_TC_36M + PROM_DEM_OTROS_TC_36M + PROM_CAS_OTROS_TC_36M), ]
  
  data[, NENT_VEN_SCE_3M := NENT_VEN_SBS_OP_3M + NENT_VEN_SBS_TC_3M + NENT_VEN_SC_OP_3M + NENT_VEN_SC_TC_3M + NENT_VEN_SICOM_OP_3M + NENT_VEN_SICOM_TC_3M + NENT_VEN_OTROS_OP_3M + NENT_VEN_OTROS_TC_3M]
  data[, NENT_VEN_SCE_6M := NENT_VEN_SBS_OP_6M + NENT_VEN_SBS_TC_6M + NENT_VEN_SC_OP_6M + NENT_VEN_SC_TC_6M + NENT_VEN_SICOM_OP_6M + NENT_VEN_SICOM_TC_6M + NENT_VEN_OTROS_OP_6M + NENT_VEN_OTROS_TC_6M]
  data[, NENT_VEN_SCE_12M := NENT_VEN_SBS_OP_12M + NENT_VEN_SBS_TC_12M + NENT_VEN_SC_OP_12M + NENT_VEN_SC_TC_12M + NENT_VEN_SICOM_OP_12M + NENT_VEN_SICOM_TC_12M + NENT_VEN_OTROS_OP_12M + NENT_VEN_OTROS_TC_12M]
  data[, NENT_VEN_SCE_24M := NENT_VEN_SBS_OP_24M + NENT_VEN_SBS_TC_24M + NENT_VEN_SC_OP_24M + NENT_VEN_SC_TC_24M + NENT_VEN_SICOM_OP_24M + NENT_VEN_SICOM_TC_24M + NENT_VEN_OTROS_OP_24M + NENT_VEN_OTROS_TC_24M]
  data[, NENT_VEN_SCE_36M := NENT_VEN_SBS_OP_36M + NENT_VEN_SBS_TC_36M + NENT_VEN_SC_OP_36M + NENT_VEN_SC_TC_36M + NENT_VEN_SICOM_OP_36M + NENT_VEN_SICOM_TC_36M + NENT_VEN_OTROS_OP_36M + NENT_VEN_OTROS_TC_36M]
  
  data[, NOPE_APERT_SCE_3M := NOPE_APERT_SBS_OP_3M + NOPE_APERT_SC_OP_3M + NOPE_APERT_SICOM_OP_3M + NOPE_APERT_OTROS_OP_3M]
  data[, NOPE_APERT_SCE_6M := NOPE_APERT_SBS_OP_6M + NOPE_APERT_SC_OP_6M + NOPE_APERT_SICOM_OP_6M + NOPE_APERT_OTROS_OP_6M]
  data[, NOPE_APERT_SCE_12M := NOPE_APERT_SBS_OP_12M + NOPE_APERT_SC_OP_12M + NOPE_APERT_SICOM_OP_12M + NOPE_APERT_OTROS_OP_12M]
  data[, NOPE_APERT_SCE_24M := NOPE_APERT_SBS_OP_24M + NOPE_APERT_SC_OP_24M + NOPE_APERT_SICOM_OP_24M + NOPE_APERT_OTROS_OP_24M]
  data[, NOPE_APERT_SCE_36M := NOPE_APERT_SBS_OP_36M + NOPE_APERT_SC_OP_36M + NOPE_APERT_SICOM_OP_36M + NOPE_APERT_OTROS_OP_36M]
  
  
  return(data)
}

# Funcion que genera ratios --------------------------------------
fun_ratios <- function(data){
  data <- data.table(data)
  # Ratios de Variacion OP y TC ----------------------------------------------------
  data[, r_NOPE_REFIN_OP_3s6M := ifelse(NOPE_REFIN_OP_6M > 0, NOPE_REFIN_OP_3M/NOPE_REFIN_OP_6M, 0), ]
  data[, r_NOPE_XVEN_OP_3s6M := ifelse(NOPE_XVEN_OP_6M > 0, NOPE_XVEN_OP_3M/NOPE_XVEN_OP_6M, 0), ]
  data[, r_NOPE_VENC_OP_3s6M := ifelse(NOPE_VENC_OP_6M > 0, NOPE_VENC_OP_3M/NOPE_VENC_OP_6M, 0), ]
  data[, r_NOPE_NDI_OP_3s6M := ifelse(NOPE_NDI_OP_6M > 0, NOPE_NDI_OP_3M/NOPE_NDI_OP_6M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_3s6M := ifelse(NOPE_VENC_1A30_OP_6M > 0, NOPE_VENC_1A30_OP_3M/NOPE_VENC_1A30_OP_6M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_3s6M := ifelse(NOPE_VENC_31A90_OP_6M > 0, NOPE_VENC_31A90_OP_3M/NOPE_VENC_31A90_OP_6M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_3s6M := ifelse(NOPE_VENC_91A180_OP_6M > 0, NOPE_VENC_91A180_OP_3M/NOPE_VENC_91A180_OP_6M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_3s6M := ifelse(NOPE_VENC_181A360_OP_6M > 0, NOPE_VENC_181A360_OP_3M/NOPE_VENC_181A360_OP_6M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_3s6M := ifelse(NOPE_VENC_MAYOR360_OP_6M > 0, NOPE_VENC_MAYOR360_OP_3M/NOPE_VENC_MAYOR360_OP_6M, 0), ]
  data[, r_NOPE_DEMANDA_OP_3s6M := ifelse(NOPE_DEMANDA_OP_6M > 0, NOPE_DEMANDA_OP_3M/NOPE_DEMANDA_OP_6M, 0), ]
  data[, r_NOPE_CASTIGO_OP_3s6M := ifelse(NOPE_CASTIGO_OP_6M > 0, NOPE_CASTIGO_OP_3M/NOPE_CASTIGO_OP_6M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_3s6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_APERT_SBS_OP_3M/NOPE_APERT_SBS_OP_6M, 0), ]
  data[, r_NOPE_APERT_SC_OP_3s6M := ifelse(NOPE_APERT_SC_OP_6M > 0, NOPE_APERT_SC_OP_3M/NOPE_APERT_SC_OP_6M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_3s6M := ifelse(NOPE_APERT_SICOM_OP_6M > 0, NOPE_APERT_SICOM_OP_3M/NOPE_APERT_SICOM_OP_6M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_3s6M := ifelse(NOPE_APERT_OTROS_OP_6M > 0, NOPE_APERT_OTROS_OP_3M/NOPE_APERT_OTROS_OP_6M, 0), ]
  data[, r_MVALVEN_SBS_OP_3s6M := ifelse(MVALVEN_SBS_OP_6M > 0, MVALVEN_SBS_OP_3M/MVALVEN_SBS_OP_6M, 0), ]
  data[, r_MVALVEN_SC_OP_3s6M := ifelse(MVALVEN_SC_OP_6M > 0, MVALVEN_SC_OP_3M/MVALVEN_SC_OP_6M, 0), ]
  data[, r_MVALVEN_SICOM_OP_3s6M := ifelse(MVALVEN_SICOM_OP_6M > 0, MVALVEN_SICOM_OP_3M/MVALVEN_SICOM_OP_6M, 0), ]
  data[, r_MVALVEN_OTROS_OP_3s6M := ifelse(MVALVEN_OTROS_OP_6M > 0, MVALVEN_OTROS_OP_3M/MVALVEN_OTROS_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_3s6M := ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, DEUDA_TOTAL_SBS_OP_3M/DEUDA_TOTAL_SBS_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_3s6M := ifelse(DEUDA_TOTAL_SC_OP_6M > 0, DEUDA_TOTAL_SC_OP_3M/DEUDA_TOTAL_SC_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_3s6M := ifelse(DEUDA_TOTAL_SICOM_OP_6M > 0, DEUDA_TOTAL_SICOM_OP_3M/DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_3s6M := ifelse(DEUDA_TOTAL_OTROS_OP_6M > 0, DEUDA_TOTAL_OTROS_OP_3M/DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  
  data[, r_PROM_MAX_DVEN_N_OP_3s6M := ifelse(PROM_MAX_DVEN_N_OP_6M > 0, PROM_MAX_DVEN_N_OP_3M/PROM_MAX_DVEN_N_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_3s6M := ifelse(PROM_MAX_DVEN_M_OP_6M > 0, PROM_MAX_DVEN_M_OP_3M/PROM_MAX_DVEN_M_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_3s6M := ifelse(PROM_MAX_DVEN_C_OP_6M > 0, PROM_MAX_DVEN_C_OP_3M/PROM_MAX_DVEN_C_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_3s6M := ifelse(PROM_MAX_DVEN_V_OP_6M > 0, PROM_MAX_DVEN_V_OP_3M/PROM_MAX_DVEN_V_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_3s6M := ifelse(PROM_MAX_DVEN_P_OP_6M > 0, PROM_MAX_DVEN_P_OP_3M/PROM_MAX_DVEN_P_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_3s6M := ifelse(PROM_MAX_DVEN_OTROS_OP_6M > 0, PROM_MAX_DVEN_OTROS_OP_3M/PROM_MAX_DVEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_3s6M := ifelse(PROM_MAX_DVEN_SBS_OP_6M > 0, PROM_MAX_DVEN_SBS_OP_3M/PROM_MAX_DVEN_SBS_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_3s6M := ifelse(PROM_MAX_DVEN_SC_OP_6M > 0, PROM_MAX_DVEN_SC_OP_3M/PROM_MAX_DVEN_SC_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_3s6M := ifelse(PROM_MAX_DVEN_SICOM_OP_6M > 0, PROM_MAX_DVEN_SICOM_OP_3M/PROM_MAX_DVEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_3s6M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_6M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_3M/PROM_MAX_DVEN_OTROS_SIS_OP_6M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_3s6M := ifelse(PROM_XVEN_SBS_OP_6M > 0, PROM_XVEN_SBS_OP_3M/PROM_XVEN_SBS_OP_6M, 0), ]
  data[, r_PROM_NDI_SBS_OP_3s6M := ifelse(PROM_NDI_SBS_OP_6M > 0, PROM_NDI_SBS_OP_3M/PROM_NDI_SBS_OP_6M, 0), ]
  data[, r_PROM_VEN_SBS_OP_3s6M := ifelse(PROM_VEN_SBS_OP_6M > 0, PROM_VEN_SBS_OP_3M/PROM_VEN_SBS_OP_6M, 0), ]
  data[, r_PROM_DEM_SBS_OP_3s6M := ifelse(PROM_DEM_SBS_OP_6M > 0, PROM_DEM_SBS_OP_3M/PROM_DEM_SBS_OP_6M, 0), ]
  data[, r_PROM_CAS_SBS_OP_3s6M := ifelse(PROM_CAS_SBS_OP_6M > 0, PROM_CAS_SBS_OP_3M/PROM_CAS_SBS_OP_6M, 0), ]
  data[, r_PROM_XVEN_SC_OP_3s6M := ifelse(PROM_XVEN_SC_OP_6M > 0, PROM_XVEN_SC_OP_3M/PROM_XVEN_SC_OP_6M, 0), ]
  data[, r_PROM_NDI_SC_OP_3s6M := ifelse(PROM_NDI_SC_OP_6M > 0, PROM_NDI_SC_OP_3M/PROM_NDI_SC_OP_6M, 0), ]
  data[, r_PROM_VEN_SC_OP_3s6M := ifelse(PROM_VEN_SC_OP_6M > 0, PROM_VEN_SC_OP_3M/PROM_VEN_SC_OP_6M, 0), ]
  data[, r_PROM_DEM_SC_OP_3s6M := ifelse(PROM_DEM_SC_OP_6M > 0, PROM_DEM_SC_OP_3M/PROM_DEM_SC_OP_6M, 0), ]
  data[, r_PROM_CAS_SC_OP_3s6M := ifelse(PROM_CAS_SC_OP_6M > 0, PROM_CAS_SC_OP_3M/PROM_CAS_SC_OP_6M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_3s6M := ifelse(PROM_XVEN_SICOM_OP_6M > 0, PROM_XVEN_SICOM_OP_3M/PROM_XVEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_3s6M := ifelse(PROM_NDI_SICOM_OP_6M > 0, PROM_NDI_SICOM_OP_3M/PROM_NDI_SICOM_OP_6M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_3s6M := ifelse(PROM_VEN_SICOM_OP_6M > 0, PROM_VEN_SICOM_OP_3M/PROM_VEN_SICOM_OP_6M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_3s6M := ifelse(PROM_DEM_SICOM_OP_6M > 0, PROM_DEM_SICOM_OP_3M/PROM_DEM_SICOM_OP_6M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_3s6M := ifelse(PROM_CAS_SICOM_OP_6M > 0, PROM_CAS_SICOM_OP_3M/PROM_CAS_SICOM_OP_6M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_3s6M := ifelse(PROM_XVEN_OTROS_OP_6M > 0, PROM_XVEN_OTROS_OP_3M/PROM_XVEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_3s6M := ifelse(PROM_NDI_OTROS_OP_6M > 0, PROM_NDI_OTROS_OP_3M/PROM_NDI_OTROS_OP_6M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_3s6M := ifelse(PROM_VEN_OTROS_OP_6M > 0, PROM_VEN_OTROS_OP_3M/PROM_VEN_OTROS_OP_6M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_3s6M := ifelse(PROM_DEM_OTROS_OP_6M > 0, PROM_DEM_OTROS_OP_3M/PROM_DEM_OTROS_OP_6M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_3s6M := ifelse(PROM_CAS_OTROS_OP_6M > 0, PROM_CAS_OTROS_OP_3M/PROM_CAS_OTROS_OP_6M, 0), ]
  
  data[, r_NOPE_REFIN_OP_3s12M := ifelse(NOPE_REFIN_OP_12M > 0, NOPE_REFIN_OP_3M/NOPE_REFIN_OP_12M, 0), ]
  data[, r_NOPE_XVEN_OP_3s12M := ifelse(NOPE_XVEN_OP_12M > 0, NOPE_XVEN_OP_3M/NOPE_XVEN_OP_12M, 0), ]
  data[, r_NOPE_VENC_OP_3s12M := ifelse(NOPE_VENC_OP_12M > 0, NOPE_VENC_OP_3M/NOPE_VENC_OP_12M, 0), ]
  data[, r_NOPE_NDI_OP_3s12M := ifelse(NOPE_NDI_OP_12M > 0, NOPE_NDI_OP_3M/NOPE_NDI_OP_12M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_3s12M := ifelse(NOPE_VENC_1A30_OP_12M > 0, NOPE_VENC_1A30_OP_3M/NOPE_VENC_1A30_OP_12M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_3s12M := ifelse(NOPE_VENC_31A90_OP_12M > 0, NOPE_VENC_31A90_OP_3M/NOPE_VENC_31A90_OP_12M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_3s12M := ifelse(NOPE_VENC_91A180_OP_12M > 0, NOPE_VENC_91A180_OP_3M/NOPE_VENC_91A180_OP_12M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_3s12M := ifelse(NOPE_VENC_181A360_OP_12M > 0, NOPE_VENC_181A360_OP_3M/NOPE_VENC_181A360_OP_12M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_3s12M := ifelse(NOPE_VENC_MAYOR360_OP_12M > 0, NOPE_VENC_MAYOR360_OP_3M/NOPE_VENC_MAYOR360_OP_12M, 0), ]
  data[, r_NOPE_DEMANDA_OP_3s12M := ifelse(NOPE_DEMANDA_OP_12M > 0, NOPE_DEMANDA_OP_3M/NOPE_DEMANDA_OP_12M, 0), ]
  data[, r_NOPE_CASTIGO_OP_3s12M := ifelse(NOPE_CASTIGO_OP_12M > 0, NOPE_CASTIGO_OP_3M/NOPE_CASTIGO_OP_12M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_3s12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_APERT_SBS_OP_3M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_APERT_SC_OP_3s12M := ifelse(NOPE_APERT_SC_OP_12M > 0, NOPE_APERT_SC_OP_3M/NOPE_APERT_SC_OP_12M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_3s12M := ifelse(NOPE_APERT_SICOM_OP_12M > 0, NOPE_APERT_SICOM_OP_3M/NOPE_APERT_SICOM_OP_12M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_3s12M := ifelse(NOPE_APERT_OTROS_OP_12M > 0, NOPE_APERT_OTROS_OP_3M/NOPE_APERT_OTROS_OP_12M, 0), ]
  data[, r_MVALVEN_SBS_OP_3s12M := ifelse(MVALVEN_SBS_OP_12M > 0, MVALVEN_SBS_OP_3M/MVALVEN_SBS_OP_12M, 0), ]
  data[, r_MVALVEN_SC_OP_3s12M := ifelse(MVALVEN_SC_OP_12M > 0, MVALVEN_SC_OP_3M/MVALVEN_SC_OP_12M, 0), ]
  data[, r_MVALVEN_SICOM_OP_3s12M := ifelse(MVALVEN_SICOM_OP_12M > 0, MVALVEN_SICOM_OP_3M/MVALVEN_SICOM_OP_12M, 0), ]
  data[, r_MVALVEN_OTROS_OP_3s12M := ifelse(MVALVEN_OTROS_OP_12M > 0, MVALVEN_OTROS_OP_3M/MVALVEN_OTROS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_3s12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, DEUDA_TOTAL_SBS_OP_3M/DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_3s12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 0, DEUDA_TOTAL_SC_OP_3M/DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_3s12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 0, DEUDA_TOTAL_SICOM_OP_3M/DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_3s12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 0, DEUDA_TOTAL_OTROS_OP_3M/DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_NENT_VEN_SBS_OP_3s12M := ifelse(NENT_VEN_SBS_OP_12M > 0, NENT_VEN_SBS_OP_3M/NENT_VEN_SBS_OP_12M, 0), ]
  data[, r_NENT_VEN_SC_OP_3s12M := ifelse(NENT_VEN_SC_OP_12M > 0, NENT_VEN_SC_OP_3M/NENT_VEN_SC_OP_12M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_3s12M := ifelse(NENT_VEN_SICOM_OP_12M > 0, NENT_VEN_SICOM_OP_3M/NENT_VEN_SICOM_OP_12M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_3s12M := ifelse(NENT_VEN_OTROS_OP_12M > 0, NENT_VEN_OTROS_OP_3M/NENT_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_3s12M := ifelse(PROM_MAX_DVEN_N_OP_12M > 0, PROM_MAX_DVEN_N_OP_3M/PROM_MAX_DVEN_N_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_3s12M := ifelse(PROM_MAX_DVEN_M_OP_12M > 0, PROM_MAX_DVEN_M_OP_3M/PROM_MAX_DVEN_M_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_3s12M := ifelse(PROM_MAX_DVEN_C_OP_12M > 0, PROM_MAX_DVEN_C_OP_3M/PROM_MAX_DVEN_C_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_3s12M := ifelse(PROM_MAX_DVEN_V_OP_12M > 0, PROM_MAX_DVEN_V_OP_3M/PROM_MAX_DVEN_V_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_3s12M := ifelse(PROM_MAX_DVEN_P_OP_12M > 0, PROM_MAX_DVEN_P_OP_3M/PROM_MAX_DVEN_P_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_3s12M := ifelse(PROM_MAX_DVEN_OTROS_OP_12M > 0, PROM_MAX_DVEN_OTROS_OP_3M/PROM_MAX_DVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_3s12M := ifelse(PROM_MAX_DVEN_SBS_OP_12M > 0, PROM_MAX_DVEN_SBS_OP_3M/PROM_MAX_DVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_3s12M := ifelse(PROM_MAX_DVEN_SC_OP_12M > 0, PROM_MAX_DVEN_SC_OP_3M/PROM_MAX_DVEN_SC_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_3s12M := ifelse(PROM_MAX_DVEN_SICOM_OP_12M > 0, PROM_MAX_DVEN_SICOM_OP_3M/PROM_MAX_DVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_3s12M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_12M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_3M/PROM_MAX_DVEN_OTROS_SIS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_3s12M := ifelse(PROM_XVEN_SBS_OP_12M > 0, PROM_XVEN_SBS_OP_3M/PROM_XVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_NDI_SBS_OP_3s12M := ifelse(PROM_NDI_SBS_OP_12M > 0, PROM_NDI_SBS_OP_3M/PROM_NDI_SBS_OP_12M, 0), ]
  data[, r_PROM_VEN_SBS_OP_3s12M := ifelse(PROM_VEN_SBS_OP_12M > 0, PROM_VEN_SBS_OP_3M/PROM_VEN_SBS_OP_12M, 0), ]
  data[, r_PROM_DEM_SBS_OP_3s12M := ifelse(PROM_DEM_SBS_OP_12M > 0, PROM_DEM_SBS_OP_3M/PROM_DEM_SBS_OP_12M, 0), ]
  data[, r_PROM_CAS_SBS_OP_3s12M := ifelse(PROM_CAS_SBS_OP_12M > 0, PROM_CAS_SBS_OP_3M/PROM_CAS_SBS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SC_OP_3s12M := ifelse(PROM_XVEN_SC_OP_12M > 0, PROM_XVEN_SC_OP_3M/PROM_XVEN_SC_OP_12M, 0), ]
  data[, r_PROM_NDI_SC_OP_3s12M := ifelse(PROM_NDI_SC_OP_12M > 0, PROM_NDI_SC_OP_3M/PROM_NDI_SC_OP_12M, 0), ]
  data[, r_PROM_VEN_SC_OP_3s12M := ifelse(PROM_VEN_SC_OP_12M > 0, PROM_VEN_SC_OP_3M/PROM_VEN_SC_OP_12M, 0), ]
  data[, r_PROM_DEM_SC_OP_3s12M := ifelse(PROM_DEM_SC_OP_12M > 0, PROM_DEM_SC_OP_3M/PROM_DEM_SC_OP_12M, 0), ]
  data[, r_PROM_CAS_SC_OP_3s12M := ifelse(PROM_CAS_SC_OP_12M > 0, PROM_CAS_SC_OP_3M/PROM_CAS_SC_OP_12M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_3s12M := ifelse(PROM_XVEN_SICOM_OP_12M > 0, PROM_XVEN_SICOM_OP_3M/PROM_XVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_3s12M := ifelse(PROM_NDI_SICOM_OP_12M > 0, PROM_NDI_SICOM_OP_3M/PROM_NDI_SICOM_OP_12M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_3s12M := ifelse(PROM_VEN_SICOM_OP_12M > 0, PROM_VEN_SICOM_OP_3M/PROM_VEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_3s12M := ifelse(PROM_DEM_SICOM_OP_12M > 0, PROM_DEM_SICOM_OP_3M/PROM_DEM_SICOM_OP_12M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_3s12M := ifelse(PROM_CAS_SICOM_OP_12M > 0, PROM_CAS_SICOM_OP_3M/PROM_CAS_SICOM_OP_12M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_3s12M := ifelse(PROM_XVEN_OTROS_OP_12M > 0, PROM_XVEN_OTROS_OP_3M/PROM_XVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_3s12M := ifelse(PROM_NDI_OTROS_OP_12M > 0, PROM_NDI_OTROS_OP_3M/PROM_NDI_OTROS_OP_12M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_3s12M := ifelse(PROM_VEN_OTROS_OP_12M > 0, PROM_VEN_OTROS_OP_3M/PROM_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_3s12M := ifelse(PROM_DEM_OTROS_OP_12M > 0, PROM_DEM_OTROS_OP_3M/PROM_DEM_OTROS_OP_12M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_3s12M := ifelse(PROM_CAS_OTROS_OP_12M > 0, PROM_CAS_OTROS_OP_3M/PROM_CAS_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_REFIN_OP_6s12M := ifelse(NOPE_REFIN_OP_12M > 0, NOPE_REFIN_OP_6M/NOPE_REFIN_OP_12M, 0), ]
  data[, r_NOPE_XVEN_OP_6s12M := ifelse(NOPE_XVEN_OP_12M > 0, NOPE_XVEN_OP_6M/NOPE_XVEN_OP_12M, 0), ]
  data[, r_NOPE_VENC_OP_6s12M := ifelse(NOPE_VENC_OP_12M > 0, NOPE_VENC_OP_6M/NOPE_VENC_OP_12M, 0), ]
  data[, r_NOPE_NDI_OP_6s12M := ifelse(NOPE_NDI_OP_12M > 0, NOPE_NDI_OP_6M/NOPE_NDI_OP_12M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_6s12M := ifelse(NOPE_VENC_1A30_OP_12M > 0, NOPE_VENC_1A30_OP_6M/NOPE_VENC_1A30_OP_12M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_6s12M := ifelse(NOPE_VENC_31A90_OP_12M > 0, NOPE_VENC_31A90_OP_6M/NOPE_VENC_31A90_OP_12M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_6s12M := ifelse(NOPE_VENC_91A180_OP_12M > 0, NOPE_VENC_91A180_OP_6M/NOPE_VENC_91A180_OP_12M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_6s12M := ifelse(NOPE_VENC_181A360_OP_12M > 0, NOPE_VENC_181A360_OP_6M/NOPE_VENC_181A360_OP_12M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_6s12M := ifelse(NOPE_VENC_MAYOR360_OP_12M > 0, NOPE_VENC_MAYOR360_OP_6M/NOPE_VENC_MAYOR360_OP_12M, 0), ]
  data[, r_NOPE_DEMANDA_OP_6s12M := ifelse(NOPE_DEMANDA_OP_12M > 0, NOPE_DEMANDA_OP_6M/NOPE_DEMANDA_OP_12M, 0), ]
  data[, r_NOPE_CASTIGO_OP_6s12M := ifelse(NOPE_CASTIGO_OP_12M > 0, NOPE_CASTIGO_OP_6M/NOPE_CASTIGO_OP_12M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_6s12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_APERT_SBS_OP_6M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_APERT_SC_OP_6s12M := ifelse(NOPE_APERT_SC_OP_12M > 0, NOPE_APERT_SC_OP_6M/NOPE_APERT_SC_OP_12M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_6s12M := ifelse(NOPE_APERT_SICOM_OP_12M > 0, NOPE_APERT_SICOM_OP_6M/NOPE_APERT_SICOM_OP_12M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_6s12M := ifelse(NOPE_APERT_OTROS_OP_12M > 0, NOPE_APERT_OTROS_OP_6M/NOPE_APERT_OTROS_OP_12M, 0), ]
  data[, r_MVALVEN_SBS_OP_6s12M := ifelse(MVALVEN_SBS_OP_12M > 0, MVALVEN_SBS_OP_6M/MVALVEN_SBS_OP_12M, 0), ]
  data[, r_MVALVEN_SC_OP_6s12M := ifelse(MVALVEN_SC_OP_12M > 0, MVALVEN_SC_OP_6M/MVALVEN_SC_OP_12M, 0), ]
  data[, r_MVALVEN_SICOM_OP_6s12M := ifelse(MVALVEN_SICOM_OP_12M > 0, MVALVEN_SICOM_OP_6M/MVALVEN_SICOM_OP_12M, 0), ]
  data[, r_MVALVEN_OTROS_OP_6s12M := ifelse(MVALVEN_OTROS_OP_12M > 0, MVALVEN_OTROS_OP_6M/MVALVEN_OTROS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_6s12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, DEUDA_TOTAL_SBS_OP_6M/DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_6s12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 0, DEUDA_TOTAL_SC_OP_6M/DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_6s12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 0, DEUDA_TOTAL_SICOM_OP_6M/DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_6s12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 0, DEUDA_TOTAL_OTROS_OP_6M/DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_NENT_VEN_SBS_OP_6s12M := ifelse(NENT_VEN_SBS_OP_12M > 0, NENT_VEN_SBS_OP_6M/NENT_VEN_SBS_OP_12M, 0), ]
  data[, r_NENT_VEN_SC_OP_6s12M := ifelse(NENT_VEN_SC_OP_12M > 0, NENT_VEN_SC_OP_6M/NENT_VEN_SC_OP_12M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_6s12M := ifelse(NENT_VEN_SICOM_OP_12M > 0, NENT_VEN_SICOM_OP_6M/NENT_VEN_SICOM_OP_12M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_6s12M := ifelse(NENT_VEN_OTROS_OP_12M > 0, NENT_VEN_OTROS_OP_6M/NENT_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_6s12M := ifelse(PROM_MAX_DVEN_N_OP_12M > 0, PROM_MAX_DVEN_N_OP_6M/PROM_MAX_DVEN_N_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_6s12M := ifelse(PROM_MAX_DVEN_M_OP_12M > 0, PROM_MAX_DVEN_M_OP_6M/PROM_MAX_DVEN_M_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_6s12M := ifelse(PROM_MAX_DVEN_C_OP_12M > 0, PROM_MAX_DVEN_C_OP_6M/PROM_MAX_DVEN_C_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_6s12M := ifelse(PROM_MAX_DVEN_V_OP_12M > 0, PROM_MAX_DVEN_V_OP_6M/PROM_MAX_DVEN_V_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_6s12M := ifelse(PROM_MAX_DVEN_P_OP_12M > 0, PROM_MAX_DVEN_P_OP_6M/PROM_MAX_DVEN_P_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_6s12M := ifelse(PROM_MAX_DVEN_OTROS_OP_12M > 0, PROM_MAX_DVEN_OTROS_OP_6M/PROM_MAX_DVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_6s12M := ifelse(PROM_MAX_DVEN_SBS_OP_12M > 0, PROM_MAX_DVEN_SBS_OP_6M/PROM_MAX_DVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_6s12M := ifelse(PROM_MAX_DVEN_SC_OP_12M > 0, PROM_MAX_DVEN_SC_OP_6M/PROM_MAX_DVEN_SC_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_6s12M := ifelse(PROM_MAX_DVEN_SICOM_OP_12M > 0, PROM_MAX_DVEN_SICOM_OP_6M/PROM_MAX_DVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_6s12M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_12M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_6M/PROM_MAX_DVEN_OTROS_SIS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_6s12M := ifelse(PROM_XVEN_SBS_OP_12M > 0, PROM_XVEN_SBS_OP_6M/PROM_XVEN_SBS_OP_12M, 0), ]
  data[, r_PROM_NDI_SBS_OP_6s12M := ifelse(PROM_NDI_SBS_OP_12M > 0, PROM_NDI_SBS_OP_6M/PROM_NDI_SBS_OP_12M, 0), ]
  data[, r_PROM_VEN_SBS_OP_6s12M := ifelse(PROM_VEN_SBS_OP_12M > 0, PROM_VEN_SBS_OP_6M/PROM_VEN_SBS_OP_12M, 0), ]
  data[, r_PROM_DEM_SBS_OP_6s12M := ifelse(PROM_DEM_SBS_OP_12M > 0, PROM_DEM_SBS_OP_6M/PROM_DEM_SBS_OP_12M, 0), ]
  data[, r_PROM_CAS_SBS_OP_6s12M := ifelse(PROM_CAS_SBS_OP_12M > 0, PROM_CAS_SBS_OP_6M/PROM_CAS_SBS_OP_12M, 0), ]
  data[, r_PROM_XVEN_SC_OP_6s12M := ifelse(PROM_XVEN_SC_OP_12M > 0, PROM_XVEN_SC_OP_6M/PROM_XVEN_SC_OP_12M, 0), ]
  data[, r_PROM_NDI_SC_OP_6s12M := ifelse(PROM_NDI_SC_OP_12M > 0, PROM_NDI_SC_OP_6M/PROM_NDI_SC_OP_12M, 0), ]
  data[, r_PROM_VEN_SC_OP_6s12M := ifelse(PROM_VEN_SC_OP_12M > 0, PROM_VEN_SC_OP_6M/PROM_VEN_SC_OP_12M, 0), ]
  data[, r_PROM_DEM_SC_OP_6s12M := ifelse(PROM_DEM_SC_OP_12M > 0, PROM_DEM_SC_OP_6M/PROM_DEM_SC_OP_12M, 0), ]
  data[, r_PROM_CAS_SC_OP_6s12M := ifelse(PROM_CAS_SC_OP_12M > 0, PROM_CAS_SC_OP_6M/PROM_CAS_SC_OP_12M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_6s12M := ifelse(PROM_XVEN_SICOM_OP_12M > 0, PROM_XVEN_SICOM_OP_6M/PROM_XVEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_6s12M := ifelse(PROM_NDI_SICOM_OP_12M > 0, PROM_NDI_SICOM_OP_6M/PROM_NDI_SICOM_OP_12M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_6s12M := ifelse(PROM_VEN_SICOM_OP_12M > 0, PROM_VEN_SICOM_OP_6M/PROM_VEN_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_6s12M := ifelse(PROM_DEM_SICOM_OP_12M > 0, PROM_DEM_SICOM_OP_6M/PROM_DEM_SICOM_OP_12M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_6s12M := ifelse(PROM_CAS_SICOM_OP_12M > 0, PROM_CAS_SICOM_OP_6M/PROM_CAS_SICOM_OP_12M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_6s12M := ifelse(PROM_XVEN_OTROS_OP_12M > 0, PROM_XVEN_OTROS_OP_6M/PROM_XVEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_6s12M := ifelse(PROM_NDI_OTROS_OP_12M > 0, PROM_NDI_OTROS_OP_6M/PROM_NDI_OTROS_OP_12M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_6s12M := ifelse(PROM_VEN_OTROS_OP_12M > 0, PROM_VEN_OTROS_OP_6M/PROM_VEN_OTROS_OP_12M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_6s12M := ifelse(PROM_DEM_OTROS_OP_12M > 0, PROM_DEM_OTROS_OP_6M/PROM_DEM_OTROS_OP_12M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_6s12M := ifelse(PROM_CAS_OTROS_OP_12M > 0, PROM_CAS_OTROS_OP_6M/PROM_CAS_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_REFIN_OP_6s24M := ifelse(NOPE_REFIN_OP_24M > 0, NOPE_REFIN_OP_6M/NOPE_REFIN_OP_24M, 0), ]
  data[, r_NOPE_XVEN_OP_6s24M := ifelse(NOPE_XVEN_OP_24M > 0, NOPE_XVEN_OP_6M/NOPE_XVEN_OP_24M, 0), ]
  data[, r_NOPE_VENC_OP_6s24M := ifelse(NOPE_VENC_OP_24M > 0, NOPE_VENC_OP_6M/NOPE_VENC_OP_24M, 0), ]
  data[, r_NOPE_NDI_OP_6s24M := ifelse(NOPE_NDI_OP_24M > 0, NOPE_NDI_OP_6M/NOPE_NDI_OP_24M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_6s24M := ifelse(NOPE_VENC_1A30_OP_24M > 0, NOPE_VENC_1A30_OP_6M/NOPE_VENC_1A30_OP_24M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_6s24M := ifelse(NOPE_VENC_31A90_OP_24M > 0, NOPE_VENC_31A90_OP_6M/NOPE_VENC_31A90_OP_24M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_6s24M := ifelse(NOPE_VENC_91A180_OP_24M > 0, NOPE_VENC_91A180_OP_6M/NOPE_VENC_91A180_OP_24M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_6s24M := ifelse(NOPE_VENC_181A360_OP_24M > 0, NOPE_VENC_181A360_OP_6M/NOPE_VENC_181A360_OP_24M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_6s24M := ifelse(NOPE_VENC_MAYOR360_OP_24M > 0, NOPE_VENC_MAYOR360_OP_6M/NOPE_VENC_MAYOR360_OP_24M, 0), ]
  data[, r_NOPE_DEMANDA_OP_6s24M := ifelse(NOPE_DEMANDA_OP_24M > 0, NOPE_DEMANDA_OP_6M/NOPE_DEMANDA_OP_24M, 0), ]
  data[, r_NOPE_CASTIGO_OP_6s24M := ifelse(NOPE_CASTIGO_OP_24M > 0, NOPE_CASTIGO_OP_6M/NOPE_CASTIGO_OP_24M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_6s24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_APERT_SBS_OP_6M/NOPE_APERT_SBS_OP_24M, 0), ]
  data[, r_NOPE_APERT_SC_OP_6s24M := ifelse(NOPE_APERT_SC_OP_24M > 0, NOPE_APERT_SC_OP_6M/NOPE_APERT_SC_OP_24M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_6s24M := ifelse(NOPE_APERT_SICOM_OP_24M > 0, NOPE_APERT_SICOM_OP_6M/NOPE_APERT_SICOM_OP_24M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_6s24M := ifelse(NOPE_APERT_OTROS_OP_24M > 0, NOPE_APERT_OTROS_OP_6M/NOPE_APERT_OTROS_OP_24M, 0), ]
  data[, r_MVALVEN_SBS_OP_6s24M := ifelse(MVALVEN_SBS_OP_24M > 0, MVALVEN_SBS_OP_6M/MVALVEN_SBS_OP_24M, 0), ]
  data[, r_MVALVEN_SC_OP_6s24M := ifelse(MVALVEN_SC_OP_24M > 0, MVALVEN_SC_OP_6M/MVALVEN_SC_OP_24M, 0), ]
  data[, r_MVALVEN_SICOM_OP_6s24M := ifelse(MVALVEN_SICOM_OP_24M > 0, MVALVEN_SICOM_OP_6M/MVALVEN_SICOM_OP_24M, 0), ]
  data[, r_MVALVEN_OTROS_OP_6s24M := ifelse(MVALVEN_OTROS_OP_24M > 0, MVALVEN_OTROS_OP_6M/MVALVEN_OTROS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_6s24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, DEUDA_TOTAL_SBS_OP_6M/DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_6s24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 0, DEUDA_TOTAL_SC_OP_6M/DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_6s24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 0, DEUDA_TOTAL_SICOM_OP_6M/DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_6s24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 0, DEUDA_TOTAL_OTROS_OP_6M/DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_NENT_VEN_SBS_OP_6s24M := ifelse(NENT_VEN_SBS_OP_24M > 0, NENT_VEN_SBS_OP_6M/NENT_VEN_SBS_OP_24M, 0), ]
  data[, r_NENT_VEN_SC_OP_6s24M := ifelse(NENT_VEN_SC_OP_24M > 0, NENT_VEN_SC_OP_6M/NENT_VEN_SC_OP_24M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_6s24M := ifelse(NENT_VEN_SICOM_OP_24M > 0, NENT_VEN_SICOM_OP_6M/NENT_VEN_SICOM_OP_24M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_6s24M := ifelse(NENT_VEN_OTROS_OP_24M > 0, NENT_VEN_OTROS_OP_6M/NENT_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_6s24M := ifelse(PROM_MAX_DVEN_N_OP_24M > 0, PROM_MAX_DVEN_N_OP_6M/PROM_MAX_DVEN_N_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_6s24M := ifelse(PROM_MAX_DVEN_M_OP_24M > 0, PROM_MAX_DVEN_M_OP_6M/PROM_MAX_DVEN_M_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_6s24M := ifelse(PROM_MAX_DVEN_C_OP_24M > 0, PROM_MAX_DVEN_C_OP_6M/PROM_MAX_DVEN_C_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_6s24M := ifelse(PROM_MAX_DVEN_V_OP_24M > 0, PROM_MAX_DVEN_V_OP_6M/PROM_MAX_DVEN_V_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_6s24M := ifelse(PROM_MAX_DVEN_P_OP_24M > 0, PROM_MAX_DVEN_P_OP_6M/PROM_MAX_DVEN_P_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_6s24M := ifelse(PROM_MAX_DVEN_OTROS_OP_24M > 0, PROM_MAX_DVEN_OTROS_OP_6M/PROM_MAX_DVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_6s24M := ifelse(PROM_MAX_DVEN_SBS_OP_24M > 0, PROM_MAX_DVEN_SBS_OP_6M/PROM_MAX_DVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_6s24M := ifelse(PROM_MAX_DVEN_SC_OP_24M > 0, PROM_MAX_DVEN_SC_OP_6M/PROM_MAX_DVEN_SC_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_6s24M := ifelse(PROM_MAX_DVEN_SICOM_OP_24M > 0, PROM_MAX_DVEN_SICOM_OP_6M/PROM_MAX_DVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_6s24M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_24M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_6M/PROM_MAX_DVEN_OTROS_SIS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_6s24M := ifelse(PROM_XVEN_SBS_OP_24M > 0, PROM_XVEN_SBS_OP_6M/PROM_XVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_NDI_SBS_OP_6s24M := ifelse(PROM_NDI_SBS_OP_24M > 0, PROM_NDI_SBS_OP_6M/PROM_NDI_SBS_OP_24M, 0), ]
  data[, r_PROM_VEN_SBS_OP_6s24M := ifelse(PROM_VEN_SBS_OP_24M > 0, PROM_VEN_SBS_OP_6M/PROM_VEN_SBS_OP_24M, 0), ]
  data[, r_PROM_DEM_SBS_OP_6s24M := ifelse(PROM_DEM_SBS_OP_24M > 0, PROM_DEM_SBS_OP_6M/PROM_DEM_SBS_OP_24M, 0), ]
  data[, r_PROM_CAS_SBS_OP_6s24M := ifelse(PROM_CAS_SBS_OP_24M > 0, PROM_CAS_SBS_OP_6M/PROM_CAS_SBS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SC_OP_6s24M := ifelse(PROM_XVEN_SC_OP_24M > 0, PROM_XVEN_SC_OP_6M/PROM_XVEN_SC_OP_24M, 0), ]
  data[, r_PROM_NDI_SC_OP_6s24M := ifelse(PROM_NDI_SC_OP_24M > 0, PROM_NDI_SC_OP_6M/PROM_NDI_SC_OP_24M, 0), ]
  data[, r_PROM_VEN_SC_OP_6s24M := ifelse(PROM_VEN_SC_OP_24M > 0, PROM_VEN_SC_OP_6M/PROM_VEN_SC_OP_24M, 0), ]
  data[, r_PROM_DEM_SC_OP_6s24M := ifelse(PROM_DEM_SC_OP_24M > 0, PROM_DEM_SC_OP_6M/PROM_DEM_SC_OP_24M, 0), ]
  data[, r_PROM_CAS_SC_OP_6s24M := ifelse(PROM_CAS_SC_OP_24M > 0, PROM_CAS_SC_OP_6M/PROM_CAS_SC_OP_24M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_6s24M := ifelse(PROM_XVEN_SICOM_OP_24M > 0, PROM_XVEN_SICOM_OP_6M/PROM_XVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_6s24M := ifelse(PROM_NDI_SICOM_OP_24M > 0, PROM_NDI_SICOM_OP_6M/PROM_NDI_SICOM_OP_24M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_6s24M := ifelse(PROM_VEN_SICOM_OP_24M > 0, PROM_VEN_SICOM_OP_6M/PROM_VEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_6s24M := ifelse(PROM_DEM_SICOM_OP_24M > 0, PROM_DEM_SICOM_OP_6M/PROM_DEM_SICOM_OP_24M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_6s24M := ifelse(PROM_CAS_SICOM_OP_24M > 0, PROM_CAS_SICOM_OP_6M/PROM_CAS_SICOM_OP_24M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_6s24M := ifelse(PROM_XVEN_OTROS_OP_24M > 0, PROM_XVEN_OTROS_OP_6M/PROM_XVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_6s24M := ifelse(PROM_NDI_OTROS_OP_24M > 0, PROM_NDI_OTROS_OP_6M/PROM_NDI_OTROS_OP_24M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_6s24M := ifelse(PROM_VEN_OTROS_OP_24M > 0, PROM_VEN_OTROS_OP_6M/PROM_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_6s24M := ifelse(PROM_DEM_OTROS_OP_24M > 0, PROM_DEM_OTROS_OP_6M/PROM_DEM_OTROS_OP_24M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_6s24M := ifelse(PROM_CAS_OTROS_OP_24M > 0, PROM_CAS_OTROS_OP_6M/PROM_CAS_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_REFIN_OP_12s24M := ifelse(NOPE_REFIN_OP_24M > 0, NOPE_REFIN_OP_12M/NOPE_REFIN_OP_24M, 0), ]
  data[, r_NOPE_XVEN_OP_12s24M := ifelse(NOPE_XVEN_OP_24M > 0, NOPE_XVEN_OP_12M/NOPE_XVEN_OP_24M, 0), ]
  data[, r_NOPE_VENC_OP_12s24M := ifelse(NOPE_VENC_OP_24M > 0, NOPE_VENC_OP_12M/NOPE_VENC_OP_24M, 0), ]
  data[, r_NOPE_NDI_OP_12s24M := ifelse(NOPE_NDI_OP_24M > 0, NOPE_NDI_OP_12M/NOPE_NDI_OP_24M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_12s24M := ifelse(NOPE_VENC_1A30_OP_24M > 0, NOPE_VENC_1A30_OP_12M/NOPE_VENC_1A30_OP_24M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_12s24M := ifelse(NOPE_VENC_31A90_OP_24M > 0, NOPE_VENC_31A90_OP_12M/NOPE_VENC_31A90_OP_24M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_12s24M := ifelse(NOPE_VENC_91A180_OP_24M > 0, NOPE_VENC_91A180_OP_12M/NOPE_VENC_91A180_OP_24M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_12s24M := ifelse(NOPE_VENC_181A360_OP_24M > 0, NOPE_VENC_181A360_OP_12M/NOPE_VENC_181A360_OP_24M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_12s24M := ifelse(NOPE_VENC_MAYOR360_OP_24M > 0, NOPE_VENC_MAYOR360_OP_12M/NOPE_VENC_MAYOR360_OP_24M, 0), ]
  data[, r_NOPE_DEMANDA_OP_12s24M := ifelse(NOPE_DEMANDA_OP_24M > 0, NOPE_DEMANDA_OP_12M/NOPE_DEMANDA_OP_24M, 0), ]
  data[, r_NOPE_CASTIGO_OP_12s24M := ifelse(NOPE_CASTIGO_OP_24M > 0, NOPE_CASTIGO_OP_12M/NOPE_CASTIGO_OP_24M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_12s24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_APERT_SBS_OP_12M/NOPE_APERT_SBS_OP_24M, 0), ]
  data[, r_NOPE_APERT_SC_OP_12s24M := ifelse(NOPE_APERT_SC_OP_24M > 0, NOPE_APERT_SC_OP_12M/NOPE_APERT_SC_OP_24M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_12s24M := ifelse(NOPE_APERT_SICOM_OP_24M > 0, NOPE_APERT_SICOM_OP_12M/NOPE_APERT_SICOM_OP_24M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_12s24M := ifelse(NOPE_APERT_OTROS_OP_24M > 0, NOPE_APERT_OTROS_OP_12M/NOPE_APERT_OTROS_OP_24M, 0), ]
  data[, r_MVALVEN_SBS_OP_12s24M := ifelse(MVALVEN_SBS_OP_24M > 0, MVALVEN_SBS_OP_12M/MVALVEN_SBS_OP_24M, 0), ]
  data[, r_MVALVEN_SC_OP_12s24M := ifelse(MVALVEN_SC_OP_24M > 0, MVALVEN_SC_OP_12M/MVALVEN_SC_OP_24M, 0), ]
  data[, r_MVALVEN_SICOM_OP_12s24M := ifelse(MVALVEN_SICOM_OP_24M > 0, MVALVEN_SICOM_OP_12M/MVALVEN_SICOM_OP_24M, 0), ]
  data[, r_MVALVEN_OTROS_OP_12s24M := ifelse(MVALVEN_OTROS_OP_24M > 0, MVALVEN_OTROS_OP_12M/MVALVEN_OTROS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_OP_12s24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, DEUDA_TOTAL_SBS_OP_12M/DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SC_OP_12s24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 0, DEUDA_TOTAL_SC_OP_12M/DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_SICOM_OP_12s24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 0, DEUDA_TOTAL_SICOM_OP_12M/DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_DEUDA_TOTAL_OTROS_OP_12s24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 0, DEUDA_TOTAL_OTROS_OP_12M/DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  data[, r_NENT_VEN_SBS_OP_12s24M := ifelse(NENT_VEN_SBS_OP_24M > 0, NENT_VEN_SBS_OP_12M/NENT_VEN_SBS_OP_24M, 0), ]
  data[, r_NENT_VEN_SC_OP_12s24M := ifelse(NENT_VEN_SC_OP_24M > 0, NENT_VEN_SC_OP_12M/NENT_VEN_SC_OP_24M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_12s24M := ifelse(NENT_VEN_SICOM_OP_24M > 0, NENT_VEN_SICOM_OP_12M/NENT_VEN_SICOM_OP_24M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_12s24M := ifelse(NENT_VEN_OTROS_OP_24M > 0, NENT_VEN_OTROS_OP_12M/NENT_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_12s24M := ifelse(PROM_MAX_DVEN_N_OP_24M > 0, PROM_MAX_DVEN_N_OP_12M/PROM_MAX_DVEN_N_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_12s24M := ifelse(PROM_MAX_DVEN_M_OP_24M > 0, PROM_MAX_DVEN_M_OP_12M/PROM_MAX_DVEN_M_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_12s24M := ifelse(PROM_MAX_DVEN_C_OP_24M > 0, PROM_MAX_DVEN_C_OP_12M/PROM_MAX_DVEN_C_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_12s24M := ifelse(PROM_MAX_DVEN_V_OP_24M > 0, PROM_MAX_DVEN_V_OP_12M/PROM_MAX_DVEN_V_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_12s24M := ifelse(PROM_MAX_DVEN_P_OP_24M > 0, PROM_MAX_DVEN_P_OP_12M/PROM_MAX_DVEN_P_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_12s24M := ifelse(PROM_MAX_DVEN_OTROS_OP_24M > 0, PROM_MAX_DVEN_OTROS_OP_12M/PROM_MAX_DVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_12s24M := ifelse(PROM_MAX_DVEN_SBS_OP_24M > 0, PROM_MAX_DVEN_SBS_OP_12M/PROM_MAX_DVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_12s24M := ifelse(PROM_MAX_DVEN_SC_OP_24M > 0, PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_12s24M := ifelse(PROM_MAX_DVEN_SICOM_OP_24M > 0, PROM_MAX_DVEN_SICOM_OP_12M/PROM_MAX_DVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_12s24M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_24M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_12M/PROM_MAX_DVEN_OTROS_SIS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_12s24M := ifelse(PROM_XVEN_SBS_OP_24M > 0, PROM_XVEN_SBS_OP_12M/PROM_XVEN_SBS_OP_24M, 0), ]
  data[, r_PROM_NDI_SBS_OP_12s24M := ifelse(PROM_NDI_SBS_OP_24M > 0, PROM_NDI_SBS_OP_12M/PROM_NDI_SBS_OP_24M, 0), ]
  data[, r_PROM_VEN_SBS_OP_12s24M := ifelse(PROM_VEN_SBS_OP_24M > 0, PROM_VEN_SBS_OP_12M/PROM_VEN_SBS_OP_24M, 0), ]
  data[, r_PROM_DEM_SBS_OP_12s24M := ifelse(PROM_DEM_SBS_OP_24M > 0, PROM_DEM_SBS_OP_12M/PROM_DEM_SBS_OP_24M, 0), ]
  data[, r_PROM_CAS_SBS_OP_12s24M := ifelse(PROM_CAS_SBS_OP_24M > 0, PROM_CAS_SBS_OP_12M/PROM_CAS_SBS_OP_24M, 0), ]
  data[, r_PROM_XVEN_SC_OP_12s24M := ifelse(PROM_XVEN_SC_OP_24M > 0, PROM_XVEN_SC_OP_12M/PROM_XVEN_SC_OP_24M, 0), ]
  data[, r_PROM_NDI_SC_OP_12s24M := ifelse(PROM_NDI_SC_OP_24M > 0, PROM_NDI_SC_OP_12M/PROM_NDI_SC_OP_24M, 0), ]
  data[, r_PROM_VEN_SC_OP_12s24M := ifelse(PROM_VEN_SC_OP_24M > 0, PROM_VEN_SC_OP_12M/PROM_VEN_SC_OP_24M, 0), ]
  data[, r_PROM_DEM_SC_OP_12s24M := ifelse(PROM_DEM_SC_OP_24M > 0, PROM_DEM_SC_OP_12M/PROM_DEM_SC_OP_24M, 0), ]
  data[, r_PROM_CAS_SC_OP_12s24M := ifelse(PROM_CAS_SC_OP_24M > 0, PROM_CAS_SC_OP_12M/PROM_CAS_SC_OP_24M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_12s24M := ifelse(PROM_XVEN_SICOM_OP_24M > 0, PROM_XVEN_SICOM_OP_12M/PROM_XVEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_12s24M := ifelse(PROM_NDI_SICOM_OP_24M > 0, PROM_NDI_SICOM_OP_12M/PROM_NDI_SICOM_OP_24M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_12s24M := ifelse(PROM_VEN_SICOM_OP_24M > 0, PROM_VEN_SICOM_OP_12M/PROM_VEN_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_12s24M := ifelse(PROM_DEM_SICOM_OP_24M > 0, PROM_DEM_SICOM_OP_12M/PROM_DEM_SICOM_OP_24M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_12s24M := ifelse(PROM_CAS_SICOM_OP_24M > 0, PROM_CAS_SICOM_OP_12M/PROM_CAS_SICOM_OP_24M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_12s24M := ifelse(PROM_XVEN_OTROS_OP_24M > 0, PROM_XVEN_OTROS_OP_12M/PROM_XVEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_12s24M := ifelse(PROM_NDI_OTROS_OP_24M > 0, PROM_NDI_OTROS_OP_12M/PROM_NDI_OTROS_OP_24M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_12s24M := ifelse(PROM_VEN_OTROS_OP_24M > 0, PROM_VEN_OTROS_OP_12M/PROM_VEN_OTROS_OP_24M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_12s24M := ifelse(PROM_DEM_OTROS_OP_24M > 0, PROM_DEM_OTROS_OP_12M/PROM_DEM_OTROS_OP_24M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_12s24M := ifelse(PROM_CAS_OTROS_OP_24M > 0, PROM_CAS_OTROS_OP_12M/PROM_CAS_OTROS_OP_24M, 0), ]
  
  #data[, r_NOPE_REFIN_OP_12s36M := ifelse(NOPE_REFIN_OP_36M > 0, NOPE_REFIN_OP_12M/NOPE_REFIN_OP_36M, 0), ]
  data[, r_NOPE_XVEN_OP_12s36M := ifelse(NOPE_XVEN_OP_36M > 0, NOPE_XVEN_OP_12M/NOPE_XVEN_OP_36M, 0), ]
  data[, r_NOPE_VENC_OP_12s36M := ifelse(NOPE_VENC_OP_36M > 0, NOPE_VENC_OP_12M/NOPE_VENC_OP_36M, 0), ]
  data[, r_NOPE_NDI_OP_12s36M := ifelse(NOPE_NDI_OP_36M > 0, NOPE_NDI_OP_12M/NOPE_NDI_OP_36M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_12s36M := ifelse(NOPE_VENC_1A30_OP_36M > 0, NOPE_VENC_1A30_OP_12M/NOPE_VENC_1A30_OP_36M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_12s36M := ifelse(NOPE_VENC_31A90_OP_36M > 0, NOPE_VENC_31A90_OP_12M/NOPE_VENC_31A90_OP_36M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_12s36M := ifelse(NOPE_VENC_91A180_OP_36M > 0, NOPE_VENC_91A180_OP_12M/NOPE_VENC_91A180_OP_36M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_12s36M := ifelse(NOPE_VENC_181A360_OP_36M > 0, NOPE_VENC_181A360_OP_12M/NOPE_VENC_181A360_OP_36M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_12s36M := ifelse(NOPE_VENC_MAYOR360_OP_36M > 0, NOPE_VENC_MAYOR360_OP_12M/NOPE_VENC_MAYOR360_OP_36M, 0), ]
  data[, r_NOPE_DEMANDA_OP_12s36M := ifelse(NOPE_DEMANDA_OP_36M > 0, NOPE_DEMANDA_OP_12M/NOPE_DEMANDA_OP_36M, 0), ]
  data[, r_NOPE_CASTIGO_OP_12s36M := ifelse(NOPE_CASTIGO_OP_36M > 0, NOPE_CASTIGO_OP_12M/NOPE_CASTIGO_OP_36M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_12s36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_APERT_SBS_OP_12M/NOPE_APERT_SBS_OP_36M, 0), ]
  data[, r_NOPE_APERT_SC_OP_12s36M := ifelse(NOPE_APERT_SC_OP_36M > 0, NOPE_APERT_SC_OP_12M/NOPE_APERT_SC_OP_36M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_12s36M := ifelse(NOPE_APERT_SICOM_OP_36M > 0, NOPE_APERT_SICOM_OP_12M/NOPE_APERT_SICOM_OP_36M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_12s36M := ifelse(NOPE_APERT_OTROS_OP_36M > 0, NOPE_APERT_OTROS_OP_12M/NOPE_APERT_OTROS_OP_36M, 0), ]
  data[, r_MVALVEN_SBS_OP_12s36M := ifelse(MVALVEN_SBS_OP_36M > 0, MVALVEN_SBS_OP_12M/MVALVEN_SBS_OP_36M, 0), ]
  data[, r_MVALVEN_SC_OP_12s36M := ifelse(MVALVEN_SC_OP_36M > 0, MVALVEN_SC_OP_12M/MVALVEN_SC_OP_36M, 0), ]
  data[, r_MVALVEN_SICOM_OP_12s36M := ifelse(MVALVEN_SICOM_OP_36M > 0, MVALVEN_SICOM_OP_12M/MVALVEN_SICOM_OP_36M, 0), ]
  data[, r_MVALVEN_OTROS_OP_12s36M := ifelse(MVALVEN_OTROS_OP_36M > 0, MVALVEN_OTROS_OP_12M/MVALVEN_OTROS_OP_36M, 0), ]
  
  data[, r_NENT_VEN_SBS_OP_12s36M := ifelse(NENT_VEN_SBS_OP_36M > 0, NENT_VEN_SBS_OP_12M/NENT_VEN_SBS_OP_36M, 0), ]
  data[, r_NENT_VEN_SC_OP_12s36M := ifelse(NENT_VEN_SC_OP_36M > 0, NENT_VEN_SC_OP_12M/NENT_VEN_SC_OP_36M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_12s36M := ifelse(NENT_VEN_SICOM_OP_36M > 0, NENT_VEN_SICOM_OP_12M/NENT_VEN_SICOM_OP_36M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_12s36M := ifelse(NENT_VEN_OTROS_OP_36M > 0, NENT_VEN_OTROS_OP_12M/NENT_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_12s36M := ifelse(PROM_MAX_DVEN_N_OP_36M > 0, PROM_MAX_DVEN_N_OP_12M/PROM_MAX_DVEN_N_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_12s36M := ifelse(PROM_MAX_DVEN_M_OP_36M > 0, PROM_MAX_DVEN_M_OP_12M/PROM_MAX_DVEN_M_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_12s36M := ifelse(PROM_MAX_DVEN_C_OP_36M > 0, PROM_MAX_DVEN_C_OP_12M/PROM_MAX_DVEN_C_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_12s36M := ifelse(PROM_MAX_DVEN_V_OP_36M > 0, PROM_MAX_DVEN_V_OP_12M/PROM_MAX_DVEN_V_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_12s36M := ifelse(PROM_MAX_DVEN_P_OP_36M > 0, PROM_MAX_DVEN_P_OP_12M/PROM_MAX_DVEN_P_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_12s36M := ifelse(PROM_MAX_DVEN_OTROS_OP_36M > 0, PROM_MAX_DVEN_OTROS_OP_12M/PROM_MAX_DVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_12s36M := ifelse(PROM_MAX_DVEN_SBS_OP_36M > 0, PROM_MAX_DVEN_SBS_OP_12M/PROM_MAX_DVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_12s36M := ifelse(PROM_MAX_DVEN_SC_OP_36M > 0, PROM_MAX_DVEN_SC_OP_12M/PROM_MAX_DVEN_SC_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_12s36M := ifelse(PROM_MAX_DVEN_SICOM_OP_36M > 0, PROM_MAX_DVEN_SICOM_OP_12M/PROM_MAX_DVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_12s36M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_36M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_12M/PROM_MAX_DVEN_OTROS_SIS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_12s36M := ifelse(PROM_XVEN_SBS_OP_36M > 0, PROM_XVEN_SBS_OP_12M/PROM_XVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_NDI_SBS_OP_12s36M := ifelse(PROM_NDI_SBS_OP_36M > 0, PROM_NDI_SBS_OP_12M/PROM_NDI_SBS_OP_36M, 0), ]
  data[, r_PROM_VEN_SBS_OP_12s36M := ifelse(PROM_VEN_SBS_OP_36M > 0, PROM_VEN_SBS_OP_12M/PROM_VEN_SBS_OP_36M, 0), ]
  data[, r_PROM_DEM_SBS_OP_12s36M := ifelse(PROM_DEM_SBS_OP_36M > 0, PROM_DEM_SBS_OP_12M/PROM_DEM_SBS_OP_36M, 0), ]
  data[, r_PROM_CAS_SBS_OP_12s36M := ifelse(PROM_CAS_SBS_OP_36M > 0, PROM_CAS_SBS_OP_12M/PROM_CAS_SBS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SC_OP_12s36M := ifelse(PROM_XVEN_SC_OP_36M > 0, PROM_XVEN_SC_OP_12M/PROM_XVEN_SC_OP_36M, 0), ]
  data[, r_PROM_NDI_SC_OP_12s36M := ifelse(PROM_NDI_SC_OP_36M > 0, PROM_NDI_SC_OP_12M/PROM_NDI_SC_OP_36M, 0), ]
  data[, r_PROM_VEN_SC_OP_12s36M := ifelse(PROM_VEN_SC_OP_36M > 0, PROM_VEN_SC_OP_12M/PROM_VEN_SC_OP_36M, 0), ]
  data[, r_PROM_DEM_SC_OP_12s36M := ifelse(PROM_DEM_SC_OP_36M > 0, PROM_DEM_SC_OP_12M/PROM_DEM_SC_OP_36M, 0), ]
  data[, r_PROM_CAS_SC_OP_12s36M := ifelse(PROM_CAS_SC_OP_36M > 0, PROM_CAS_SC_OP_12M/PROM_CAS_SC_OP_36M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_12s36M := ifelse(PROM_XVEN_SICOM_OP_36M > 0, PROM_XVEN_SICOM_OP_12M/PROM_XVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_12s36M := ifelse(PROM_NDI_SICOM_OP_36M > 0, PROM_NDI_SICOM_OP_12M/PROM_NDI_SICOM_OP_36M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_12s36M := ifelse(PROM_VEN_SICOM_OP_36M > 0, PROM_VEN_SICOM_OP_12M/PROM_VEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_12s36M := ifelse(PROM_DEM_SICOM_OP_36M > 0, PROM_DEM_SICOM_OP_12M/PROM_DEM_SICOM_OP_36M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_12s36M := ifelse(PROM_CAS_SICOM_OP_36M > 0, PROM_CAS_SICOM_OP_12M/PROM_CAS_SICOM_OP_36M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_12s36M := ifelse(PROM_XVEN_OTROS_OP_36M > 0, PROM_XVEN_OTROS_OP_12M/PROM_XVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_12s36M := ifelse(PROM_NDI_OTROS_OP_36M > 0, PROM_NDI_OTROS_OP_12M/PROM_NDI_OTROS_OP_36M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_12s36M := ifelse(PROM_VEN_OTROS_OP_36M > 0, PROM_VEN_OTROS_OP_12M/PROM_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_12s36M := ifelse(PROM_DEM_OTROS_OP_36M > 0, PROM_DEM_OTROS_OP_12M/PROM_DEM_OTROS_OP_36M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_12s36M := ifelse(PROM_CAS_OTROS_OP_36M > 0, PROM_CAS_OTROS_OP_12M/PROM_CAS_OTROS_OP_36M, 0), ]
  
  #data[, r_NOPE_REFIN_OP_24s36M := ifelse(NOPE_REFIN_OP_36M > 0, NOPE_REFIN_OP_24M/NOPE_REFIN_OP_36M, 0), ]
  data[, r_NOPE_XVEN_OP_24s36M := ifelse(NOPE_XVEN_OP_36M > 0, NOPE_XVEN_OP_24M/NOPE_XVEN_OP_36M, 0), ]
  data[, r_NOPE_VENC_OP_24s36M := ifelse(NOPE_VENC_OP_36M > 0, NOPE_VENC_OP_24M/NOPE_VENC_OP_36M, 0), ]
  data[, r_NOPE_NDI_OP_24s36M := ifelse(NOPE_NDI_OP_36M > 0, NOPE_NDI_OP_24M/NOPE_NDI_OP_36M, 0), ]
  data[, r_NOPE_VENC_1A30_OP_24s36M := ifelse(NOPE_VENC_1A30_OP_36M > 0, NOPE_VENC_1A30_OP_24M/NOPE_VENC_1A30_OP_36M, 0), ]
  data[, r_NOPE_VENC_31A90_OP_24s36M := ifelse(NOPE_VENC_31A90_OP_36M > 0, NOPE_VENC_31A90_OP_24M/NOPE_VENC_31A90_OP_36M, 0), ]
  data[, r_NOPE_VENC_91A180_OP_24s36M := ifelse(NOPE_VENC_91A180_OP_36M > 0, NOPE_VENC_91A180_OP_24M/NOPE_VENC_91A180_OP_36M, 0), ]
  data[, r_NOPE_VENC_181A360_OP_24s36M := ifelse(NOPE_VENC_181A360_OP_36M > 0, NOPE_VENC_181A360_OP_24M/NOPE_VENC_181A360_OP_36M, 0), ]
  data[, r_NOPE_VENC_MAYOR360_OP_24s36M := ifelse(NOPE_VENC_MAYOR360_OP_36M > 0, NOPE_VENC_MAYOR360_OP_24M/NOPE_VENC_MAYOR360_OP_36M, 0), ]
  data[, r_NOPE_DEMANDA_OP_24s36M := ifelse(NOPE_DEMANDA_OP_36M > 0, NOPE_DEMANDA_OP_24M/NOPE_DEMANDA_OP_36M, 0), ]
  data[, r_NOPE_CASTIGO_OP_24s36M := ifelse(NOPE_CASTIGO_OP_36M > 0, NOPE_CASTIGO_OP_24M/NOPE_CASTIGO_OP_36M, 0), ]
  data[, r_NOPE_APERT_SBS_OP_24s36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_APERT_SBS_OP_24M/NOPE_APERT_SBS_OP_36M, 0), ]
  data[, r_NOPE_APERT_SC_OP_24s36M := ifelse(NOPE_APERT_SC_OP_36M > 0, NOPE_APERT_SC_OP_24M/NOPE_APERT_SC_OP_36M, 0), ]
  data[, r_NOPE_APERT_SICOM_OP_24s36M := ifelse(NOPE_APERT_SICOM_OP_36M > 0, NOPE_APERT_SICOM_OP_24M/NOPE_APERT_SICOM_OP_36M, 0), ]
  data[, r_NOPE_APERT_OTROS_OP_24s36M := ifelse(NOPE_APERT_OTROS_OP_36M > 0, NOPE_APERT_OTROS_OP_24M/NOPE_APERT_OTROS_OP_36M, 0), ]
  data[, r_MVALVEN_SBS_OP_24s36M := ifelse(MVALVEN_SBS_OP_36M > 0, MVALVEN_SBS_OP_24M/MVALVEN_SBS_OP_36M, 0), ]
  data[, r_MVALVEN_SC_OP_24s36M := ifelse(MVALVEN_SC_OP_36M > 0, MVALVEN_SC_OP_24M/MVALVEN_SC_OP_36M, 0), ]
  data[, r_MVALVEN_SICOM_OP_24s36M := ifelse(MVALVEN_SICOM_OP_36M > 0, MVALVEN_SICOM_OP_24M/MVALVEN_SICOM_OP_36M, 0), ]
  data[, r_MVALVEN_OTROS_OP_24s36M := ifelse(MVALVEN_OTROS_OP_36M > 0, MVALVEN_OTROS_OP_24M/MVALVEN_OTROS_OP_36M, 0), ]
  
  data[, r_NENT_VEN_SBS_OP_24s36M := ifelse(NENT_VEN_SBS_OP_36M > 0, NENT_VEN_SBS_OP_24M/NENT_VEN_SBS_OP_36M, 0), ]
  data[, r_NENT_VEN_SC_OP_24s36M := ifelse(NENT_VEN_SC_OP_36M > 0, NENT_VEN_SC_OP_24M/NENT_VEN_SC_OP_36M, 0), ]
  data[, r_NENT_VEN_SICOM_OP_24s36M := ifelse(NENT_VEN_SICOM_OP_36M > 0, NENT_VEN_SICOM_OP_24M/NENT_VEN_SICOM_OP_36M, 0), ]
  data[, r_NENT_VEN_OTROS_OP_24s36M := ifelse(NENT_VEN_OTROS_OP_36M > 0, NENT_VEN_OTROS_OP_24M/NENT_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_N_OP_24s36M := ifelse(PROM_MAX_DVEN_N_OP_36M > 0, PROM_MAX_DVEN_N_OP_24M/PROM_MAX_DVEN_N_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_M_OP_24s36M := ifelse(PROM_MAX_DVEN_M_OP_36M > 0, PROM_MAX_DVEN_M_OP_24M/PROM_MAX_DVEN_M_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_C_OP_24s36M := ifelse(PROM_MAX_DVEN_C_OP_36M > 0, PROM_MAX_DVEN_C_OP_24M/PROM_MAX_DVEN_C_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_V_OP_24s36M := ifelse(PROM_MAX_DVEN_V_OP_36M > 0, PROM_MAX_DVEN_V_OP_24M/PROM_MAX_DVEN_V_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_P_OP_24s36M := ifelse(PROM_MAX_DVEN_P_OP_36M > 0, PROM_MAX_DVEN_P_OP_24M/PROM_MAX_DVEN_P_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_OP_24s36M := ifelse(PROM_MAX_DVEN_OTROS_OP_36M > 0, PROM_MAX_DVEN_OTROS_OP_24M/PROM_MAX_DVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_OP_24s36M := ifelse(PROM_MAX_DVEN_SBS_OP_36M > 0, PROM_MAX_DVEN_SBS_OP_24M/PROM_MAX_DVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SC_OP_24s36M := ifelse(PROM_MAX_DVEN_SC_OP_36M > 0, PROM_MAX_DVEN_SC_OP_24M/PROM_MAX_DVEN_SC_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SICOM_OP_24s36M := ifelse(PROM_MAX_DVEN_SICOM_OP_36M > 0, PROM_MAX_DVEN_SICOM_OP_24M/PROM_MAX_DVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_MAX_DVEN_OTROS_SIS_OP_24s36M := ifelse(PROM_MAX_DVEN_OTROS_SIS_OP_36M > 0, PROM_MAX_DVEN_OTROS_SIS_OP_24M/PROM_MAX_DVEN_OTROS_SIS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SBS_OP_24s36M := ifelse(PROM_XVEN_SBS_OP_36M > 0, PROM_XVEN_SBS_OP_24M/PROM_XVEN_SBS_OP_36M, 0), ]
  data[, r_PROM_NDI_SBS_OP_24s36M := ifelse(PROM_NDI_SBS_OP_36M > 0, PROM_NDI_SBS_OP_24M/PROM_NDI_SBS_OP_36M, 0), ]
  data[, r_PROM_VEN_SBS_OP_24s36M := ifelse(PROM_VEN_SBS_OP_36M > 0, PROM_VEN_SBS_OP_24M/PROM_VEN_SBS_OP_36M, 0), ]
  data[, r_PROM_DEM_SBS_OP_24s36M := ifelse(PROM_DEM_SBS_OP_36M > 0, PROM_DEM_SBS_OP_24M/PROM_DEM_SBS_OP_36M, 0), ]
  data[, r_PROM_CAS_SBS_OP_24s36M := ifelse(PROM_CAS_SBS_OP_36M > 0, PROM_CAS_SBS_OP_24M/PROM_CAS_SBS_OP_36M, 0), ]
  data[, r_PROM_XVEN_SC_OP_24s36M := ifelse(PROM_XVEN_SC_OP_36M > 0, PROM_XVEN_SC_OP_24M/PROM_XVEN_SC_OP_36M, 0), ]
  data[, r_PROM_NDI_SC_OP_24s36M := ifelse(PROM_NDI_SC_OP_36M > 0, PROM_NDI_SC_OP_24M/PROM_NDI_SC_OP_36M, 0), ]
  data[, r_PROM_VEN_SC_OP_24s36M := ifelse(PROM_VEN_SC_OP_36M > 0, PROM_VEN_SC_OP_24M/PROM_VEN_SC_OP_36M, 0), ]
  data[, r_PROM_DEM_SC_OP_24s36M := ifelse(PROM_DEM_SC_OP_36M > 0, PROM_DEM_SC_OP_24M/PROM_DEM_SC_OP_36M, 0), ]
  data[, r_PROM_CAS_SC_OP_24s36M := ifelse(PROM_CAS_SC_OP_36M > 0, PROM_CAS_SC_OP_24M/PROM_CAS_SC_OP_36M, 0), ]
  data[, r_PROM_XVEN_SICOM_OP_24s36M := ifelse(PROM_XVEN_SICOM_OP_36M > 0, PROM_XVEN_SICOM_OP_24M/PROM_XVEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_NDI_SICOM_OP_24s36M := ifelse(PROM_NDI_SICOM_OP_36M > 0, PROM_NDI_SICOM_OP_24M/PROM_NDI_SICOM_OP_36M, 0), ]
  data[, r_PROM_VEN_SICOM_OP_24s36M := ifelse(PROM_VEN_SICOM_OP_36M > 0, PROM_VEN_SICOM_OP_24M/PROM_VEN_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEM_SICOM_OP_24s36M := ifelse(PROM_DEM_SICOM_OP_36M > 0, PROM_DEM_SICOM_OP_24M/PROM_DEM_SICOM_OP_36M, 0), ]
  data[, r_PROM_CAS_SICOM_OP_24s36M := ifelse(PROM_CAS_SICOM_OP_36M > 0, PROM_CAS_SICOM_OP_24M/PROM_CAS_SICOM_OP_36M, 0), ]
  data[, r_PROM_XVEN_OTROS_OP_24s36M := ifelse(PROM_XVEN_OTROS_OP_36M > 0, PROM_XVEN_OTROS_OP_24M/PROM_XVEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_NDI_OTROS_OP_24s36M := ifelse(PROM_NDI_OTROS_OP_36M > 0, PROM_NDI_OTROS_OP_24M/PROM_NDI_OTROS_OP_36M, 0), ]
  data[, r_PROM_VEN_OTROS_OP_24s36M := ifelse(PROM_VEN_OTROS_OP_36M > 0, PROM_VEN_OTROS_OP_24M/PROM_VEN_OTROS_OP_36M, 0), ]
  data[, r_PROM_DEM_OTROS_OP_24s36M := ifelse(PROM_DEM_OTROS_OP_36M > 0, PROM_DEM_OTROS_OP_24M/PROM_DEM_OTROS_OP_36M, 0), ]
  data[, r_PROM_CAS_OTROS_OP_24s36M := ifelse(PROM_CAS_OTROS_OP_36M > 0, PROM_CAS_OTROS_OP_24M/PROM_CAS_OTROS_OP_36M, 0), ]
  
  # data[, r_MVAL_DEMANDA_SBS_OP_6s12M := ifelse(MVAL_DEMANDA_SBS_OP_12M > 0, MVAL_DEMANDA_SBS_OP_6M/MVAL_DEMANDA_SBS_OP_12M, 0), ]
  # data[, r_MVAL_DEMANDA_SC_OP_6s12M := ifelse(MVAL_DEMANDA_SC_OP_12M > 0, MVAL_DEMANDA_SC_OP_6M/MVAL_DEMANDA_SC_OP_12M, 0), ]
  # data[, r_MVAL_DEMANDA_SICOM_OP_6s12M := ifelse(MVAL_DEMANDA_SICOM_OP_12M > 0, MVAL_DEMANDA_SICOM_OP_6M/MVAL_DEMANDA_SICOM_OP_12M, 0), ]
  # data[, r_MVAL_DEMANDA_OTROS_OP_6s12M := ifelse(MVAL_DEMANDA_OTROS_OP_12M > 0, MVAL_DEMANDA_OTROS_OP_6M/MVAL_DEMANDA_OTROS_OP_12M, 0), ]
  # data[, r_MVAL_CASTIGO_SBS_OP_6s12M := ifelse(MVAL_CASTIGO_SBS_OP_12M > 0, MVAL_CASTIGO_SBS_OP_6M/MVAL_CASTIGO_SBS_OP_12M, 0), ]
  # data[, r_MVAL_CASTIGO_SC_OP_6s12M := ifelse(MVAL_CASTIGO_SC_OP_12M > 0, MVAL_CASTIGO_SC_OP_6M/MVAL_CASTIGO_SC_OP_12M, 0), ]
  # data[, r_MVAL_CASTIGO_SICOM_OP_6s12M := ifelse(MVAL_CASTIGO_SICOM_OP_12M > 0, MVAL_CASTIGO_SICOM_OP_6M/MVAL_CASTIGO_SICOM_OP_12M, 0), ]
  # data[, r_MVAL_CASTIGO_OTROS_OP_6s12M := ifelse(MVAL_CASTIGO_OTROS_OP_12M > 0, MVAL_CASTIGO_OTROS_OP_6M/MVAL_CASTIGO_OTROS_OP_12M, 0), ]
  # 
  # data[, r_MVAL_DEMANDA_SBS_OP_6s24M := ifelse(MVAL_DEMANDA_SBS_OP_24M > 0, MVAL_DEMANDA_SBS_OP_6M/MVAL_DEMANDA_SBS_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_SC_OP_6s24M := ifelse(MVAL_DEMANDA_SC_OP_24M > 0, MVAL_DEMANDA_SC_OP_6M/MVAL_DEMANDA_SC_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_SICOM_OP_6s24M := ifelse(MVAL_DEMANDA_SICOM_OP_24M > 0, MVAL_DEMANDA_SICOM_OP_6M/MVAL_DEMANDA_SICOM_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_OTROS_OP_6s24M := ifelse(MVAL_DEMANDA_OTROS_OP_24M > 0, MVAL_DEMANDA_OTROS_OP_6M/MVAL_DEMANDA_OTROS_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SBS_OP_6s24M := ifelse(MVAL_CASTIGO_SBS_OP_24M > 0, MVAL_CASTIGO_SBS_OP_6M/MVAL_CASTIGO_SBS_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SC_OP_6s24M := ifelse(MVAL_CASTIGO_SC_OP_24M > 0, MVAL_CASTIGO_SC_OP_6M/MVAL_CASTIGO_SC_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SICOM_OP_6s24M := ifelse(MVAL_CASTIGO_SICOM_OP_24M > 0, MVAL_CASTIGO_SICOM_OP_6M/MVAL_CASTIGO_SICOM_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_OTROS_OP_6s24M := ifelse(MVAL_CASTIGO_OTROS_OP_24M > 0, MVAL_CASTIGO_OTROS_OP_6M/MVAL_CASTIGO_OTROS_OP_24M, 0), ]
  # 
  # data[, r_MVAL_DEMANDA_SBS_OP_12s24M := ifelse(MVAL_DEMANDA_SBS_OP_24M > 0, MVAL_DEMANDA_SBS_OP_12M/MVAL_DEMANDA_SBS_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_SC_OP_12s24M := ifelse(MVAL_DEMANDA_SC_OP_24M > 0, MVAL_DEMANDA_SC_OP_12M/MVAL_DEMANDA_SC_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_SICOM_OP_12s24M := ifelse(MVAL_DEMANDA_SICOM_OP_24M > 0, MVAL_DEMANDA_SICOM_OP_12M/MVAL_DEMANDA_SICOM_OP_24M, 0), ]
  # data[, r_MVAL_DEMANDA_OTROS_OP_12s24M := ifelse(MVAL_DEMANDA_OTROS_OP_24M > 0, MVAL_DEMANDA_OTROS_OP_12M/MVAL_DEMANDA_OTROS_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SBS_OP_12s24M := ifelse(MVAL_CASTIGO_SBS_OP_24M > 0, MVAL_CASTIGO_SBS_OP_12M/MVAL_CASTIGO_SBS_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SC_OP_12s24M := ifelse(MVAL_CASTIGO_SC_OP_24M > 0, MVAL_CASTIGO_SC_OP_12M/MVAL_CASTIGO_SC_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_SICOM_OP_12s24M := ifelse(MVAL_CASTIGO_SICOM_OP_24M > 0, MVAL_CASTIGO_SICOM_OP_12M/MVAL_CASTIGO_SICOM_OP_24M, 0), ]
  # data[, r_MVAL_CASTIGO_OTROS_OP_12s24M := ifelse(MVAL_CASTIGO_OTROS_OP_24M > 0, MVAL_CASTIGO_OTROS_OP_12M/MVAL_CASTIGO_OTROS_OP_24M, 0), ]
  # 
  # data[, r_MVAL_DEMANDA_SBS_OP_24s36M := ifelse(MVAL_DEMANDA_SBS_OP_36M > 0, MVAL_DEMANDA_SBS_OP_24M/MVAL_DEMANDA_SBS_OP_36M, 0), ]
  # data[, r_MVAL_DEMANDA_SC_OP_24s36M := ifelse(MVAL_DEMANDA_SC_OP_36M > 0, MVAL_DEMANDA_SC_OP_24M/MVAL_DEMANDA_SC_OP_36M, 0), ]
  # data[, r_MVAL_DEMANDA_SICOM_OP_24s36M := ifelse(MVAL_DEMANDA_SICOM_OP_36M > 0, MVAL_DEMANDA_SICOM_OP_24M/MVAL_DEMANDA_SICOM_OP_36M, 0), ]
  # data[, r_MVAL_DEMANDA_OTROS_OP_24s36M := ifelse(MVAL_DEMANDA_OTROS_OP_36M > 0, MVAL_DEMANDA_OTROS_OP_24M/MVAL_DEMANDA_OTROS_OP_36M, 0), ]
  # data[, r_MVAL_CASTIGO_SBS_OP_24s36M := ifelse(MVAL_CASTIGO_SBS_OP_36M > 0, MVAL_CASTIGO_SBS_OP_24M/MVAL_CASTIGO_SBS_OP_36M, 0), ]
  # data[, r_MVAL_CASTIGO_SC_OP_24s36M := ifelse(MVAL_CASTIGO_SC_OP_36M > 0, MVAL_CASTIGO_SC_OP_24M/MVAL_CASTIGO_SC_OP_36M, 0), ]
  # data[, r_MVAL_CASTIGO_SICOM_OP_24s36M := ifelse(MVAL_CASTIGO_SICOM_OP_36M > 0, MVAL_CASTIGO_SICOM_OP_24M/MVAL_CASTIGO_SICOM_OP_36M, 0), ]
  # data[, r_MVAL_CASTIGO_OTROS_OP_24s36M := ifelse(MVAL_CASTIGO_OTROS_OP_36M > 0, MVAL_CASTIGO_OTROS_OP_24M/MVAL_CASTIGO_OTROS_OP_36M, 0), ]
  # 
  data[, r_NOPE_VENC_31AMAS_OP_3s6M := ifelse(NOPE_VENC_31AMAS_OP_6M > 0, NOPE_VENC_31AMAS_OP_3M/NOPE_VENC_31AMAS_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_DEUDA_TOTAL_SBS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SC_OP_6M > 0, PROM_DEUDA_TOTAL_SC_OP_3M/PROM_DEUDA_TOTAL_SC_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_6M > 0, PROM_DEUDA_TOTAL_SICOM_OP_3M/PROM_DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_3s6M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_6M > 0, PROM_DEUDA_TOTAL_OTROS_OP_3M/PROM_DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_3s12M := ifelse(NOPE_VENC_31AMAS_OP_12M > 0, NOPE_VENC_31AMAS_OP_3M/NOPE_VENC_31AMAS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_DEUDA_TOTAL_SBS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 0, PROM_DEUDA_TOTAL_SC_OP_3M/PROM_DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 0, PROM_DEUDA_TOTAL_SICOM_OP_3M/PROM_DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_3s12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 0, PROM_DEUDA_TOTAL_OTROS_OP_3M/PROM_DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_6s12M := ifelse(NOPE_VENC_31AMAS_OP_12M > 0, NOPE_VENC_31AMAS_OP_6M/NOPE_VENC_31AMAS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_DEUDA_TOTAL_SBS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 0, PROM_DEUDA_TOTAL_SC_OP_6M/PROM_DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 0, PROM_DEUDA_TOTAL_SICOM_OP_6M/PROM_DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_6s12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 0, PROM_DEUDA_TOTAL_OTROS_OP_6M/PROM_DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_6s24M := ifelse(NOPE_VENC_31AMAS_OP_24M > 0, NOPE_VENC_31AMAS_OP_6M/NOPE_VENC_31AMAS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_DEUDA_TOTAL_SBS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 0, PROM_DEUDA_TOTAL_SC_OP_6M/PROM_DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 0, PROM_DEUDA_TOTAL_SICOM_OP_6M/PROM_DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_6s24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 0, PROM_DEUDA_TOTAL_OTROS_OP_6M/PROM_DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_12s24M := ifelse(NOPE_VENC_31AMAS_OP_24M > 0, NOPE_VENC_31AMAS_OP_12M/NOPE_VENC_31AMAS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_DEUDA_TOTAL_SBS_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 0, PROM_DEUDA_TOTAL_SC_OP_12M/PROM_DEUDA_TOTAL_SC_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 0, PROM_DEUDA_TOTAL_SICOM_OP_12M/PROM_DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_12s24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 0, PROM_DEUDA_TOTAL_OTROS_OP_12M/PROM_DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  
  data[, r_NOPE_VENC_31AMAS_OP_24s36M := ifelse(NOPE_VENC_31AMAS_OP_36M > 0, NOPE_VENC_31AMAS_OP_24M/NOPE_VENC_31AMAS_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_DEUDA_TOTAL_SBS_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SC_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SC_OP_36M > 0, PROM_DEUDA_TOTAL_SC_OP_24M/PROM_DEUDA_TOTAL_SC_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SICOM_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_36M > 0, PROM_DEUDA_TOTAL_SICOM_OP_24M/PROM_DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_OTROS_OP_24s36M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_36M > 0, PROM_DEUDA_TOTAL_OTROS_OP_24M/PROM_DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  
  data[, r_NTC_REFIN_TC_3s6M := ifelse(NTC_REFIN_TC_6M > 0, NTC_REFIN_TC_3M/NTC_REFIN_TC_6M, 0), ]
  data[, r_NTC_VENC_TC_3s6M := ifelse(NTC_VENC_TC_6M > 0, NTC_VENC_TC_3M/NTC_VENC_TC_6M, 0), ]
  data[, r_NTC_VENC_1A30_TC_3s6M := ifelse(NTC_VENC_1A30_TC_6M > 0, NTC_VENC_1A30_TC_3M/NTC_VENC_1A30_TC_6M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_3s6M := ifelse(NTC_VENC_31AMAS_TC_6M > 0, NTC_VENC_31AMAS_TC_3M/NTC_VENC_31AMAS_TC_6M, 0), ]
  data[, r_NTC_APERT_SBS_TC_3s6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_APERT_SBS_TC_3M/NTC_APERT_SBS_TC_6M, 0), ]
  data[, r_MVALVEN_SBS_TC_3s6M := ifelse(MVALVEN_SBS_TC_6M > 0, MVALVEN_SBS_TC_3M/MVALVEN_SBS_TC_6M, 0), ]
  #data[, r_MVAL_DEMANDA_SBS_TC_3s6M := ifelse(MVAL_DEMANDA_SBS_TC_6M > 0, MVAL_DEMANDA_SBS_TC_3M/MVAL_DEMANDA_SBS_TC_6M, 0), ]
  #data[, r_MVAL_CASTIGO_SBS_TC_3s6M := ifelse(MVAL_CASTIGO_SBS_TC_6M > 0, MVAL_CASTIGO_SBS_TC_3M/MVAL_CASTIGO_SBS_TC_6M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_TC_3s6M := ifelse(DEUDA_TOTAL_SBS_TC_6M > 0, DEUDA_TOTAL_SBS_TC_3M/DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_3s6M := ifelse(PROM_MAX_DVEN_SBS_TC_6M > 0, PROM_MAX_DVEN_SBS_TC_3M/PROM_MAX_DVEN_SBS_TC_6M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_3s6M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_6M > 0, PROM_DEUDA_TOTAL_SBS_TC_3M/PROM_DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_PROM_VEN_SBS_TC_3s6M := ifelse(PROM_VEN_SBS_TC_6M > 0, PROM_VEN_SBS_TC_3M/PROM_VEN_SBS_TC_6M, 0), ]
  
  data[, r_NTC_REFIN_TC_6s12M := ifelse(NTC_REFIN_TC_12M > 0, NTC_REFIN_TC_6M/NTC_REFIN_TC_12M, 0), ]
  data[, r_NTC_VENC_TC_6s12M := ifelse(NTC_VENC_TC_12M > 0, NTC_VENC_TC_6M/NTC_VENC_TC_12M, 0), ]
  data[, r_NTC_VENC_1A30_TC_6s12M := ifelse(NTC_VENC_1A30_TC_12M > 0, NTC_VENC_1A30_TC_6M/NTC_VENC_1A30_TC_12M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_6s12M := ifelse(NTC_VENC_31AMAS_TC_12M > 0, NTC_VENC_31AMAS_TC_6M/NTC_VENC_31AMAS_TC_12M, 0), ]
  data[, r_NTC_APERT_SBS_TC_6s12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_APERT_SBS_TC_6M/NTC_APERT_SBS_TC_12M, 0), ]
  data[, r_MVALVEN_SBS_TC_6s12M := ifelse(MVALVEN_SBS_TC_12M > 0, MVALVEN_SBS_TC_6M/MVALVEN_SBS_TC_12M, 0), ]
  data[, r_MVAL_DEMANDA_SBS_TC_6s12M := ifelse(MVAL_DEMANDA_SBS_TC_12M > 0, MVAL_DEMANDA_SBS_TC_6M/MVAL_DEMANDA_SBS_TC_12M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_TC_6s12M := ifelse(MVAL_CASTIGO_SBS_TC_12M > 0, MVAL_CASTIGO_SBS_TC_6M/MVAL_CASTIGO_SBS_TC_12M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_TC_6s12M := ifelse(DEUDA_TOTAL_SBS_TC_12M > 0, DEUDA_TOTAL_SBS_TC_6M/DEUDA_TOTAL_SBS_TC_12M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_6s12M := ifelse(PROM_MAX_DVEN_SBS_TC_12M > 0, PROM_MAX_DVEN_SBS_TC_6M/PROM_MAX_DVEN_SBS_TC_12M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_6s12M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_12M > 0, PROM_DEUDA_TOTAL_SBS_TC_6M/PROM_DEUDA_TOTAL_SBS_TC_12M, 0), ]
  data[, r_PROM_VEN_SBS_TC_6s12M := ifelse(PROM_VEN_SBS_TC_12M > 0, PROM_VEN_SBS_TC_6M/PROM_VEN_SBS_TC_12M, 0), ]
  
  data[, r_NTC_REFIN_TC_6s24M := ifelse(NTC_REFIN_TC_24M > 0, NTC_REFIN_TC_6M/NTC_REFIN_TC_24M, 0), ]
  data[, r_NTC_VENC_TC_6s24M := ifelse(NTC_VENC_TC_24M > 0, NTC_VENC_TC_6M/NTC_VENC_TC_24M, 0), ]
  data[, r_NTC_VENC_1A30_TC_6s24M := ifelse(NTC_VENC_1A30_TC_24M > 0, NTC_VENC_1A30_TC_6M/NTC_VENC_1A30_TC_24M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_6s24M := ifelse(NTC_VENC_31AMAS_TC_24M > 0, NTC_VENC_31AMAS_TC_6M/NTC_VENC_31AMAS_TC_24M, 0), ]
  data[, r_NTC_APERT_SBS_TC_6s24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_APERT_SBS_TC_6M/NTC_APERT_SBS_TC_24M, 0), ]
  data[, r_MVALVEN_SBS_TC_6s24M := ifelse(MVALVEN_SBS_TC_24M > 0, MVALVEN_SBS_TC_6M/MVALVEN_SBS_TC_24M, 0), ]
  data[, r_MVAL_DEMANDA_SBS_TC_6s24M := ifelse(MVAL_DEMANDA_SBS_TC_24M > 0, MVAL_DEMANDA_SBS_TC_6M/MVAL_DEMANDA_SBS_TC_24M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_TC_6s24M := ifelse(MVAL_CASTIGO_SBS_TC_24M > 0, MVAL_CASTIGO_SBS_TC_6M/MVAL_CASTIGO_SBS_TC_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_TC_6s24M := ifelse(DEUDA_TOTAL_SBS_TC_24M > 0, DEUDA_TOTAL_SBS_TC_6M/DEUDA_TOTAL_SBS_TC_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_6s24M := ifelse(PROM_MAX_DVEN_SBS_TC_24M > 0, PROM_MAX_DVEN_SBS_TC_6M/PROM_MAX_DVEN_SBS_TC_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_6s24M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_24M > 0, PROM_DEUDA_TOTAL_SBS_TC_6M/PROM_DEUDA_TOTAL_SBS_TC_24M, 0), ]
  data[, r_PROM_VEN_SBS_TC_6s24M := ifelse(PROM_VEN_SBS_TC_24M > 0, PROM_VEN_SBS_TC_6M/PROM_VEN_SBS_TC_24M, 0), ]
  
  data[, r_NTC_REFIN_TC_12s24M := ifelse(NTC_REFIN_TC_24M > 0, NTC_REFIN_TC_12M/NTC_REFIN_TC_24M, 0), ]
  data[, r_NTC_VENC_TC_12s24M := ifelse(NTC_VENC_TC_24M > 0, NTC_VENC_TC_12M/NTC_VENC_TC_24M, 0), ]
  data[, r_NTC_VENC_1A30_TC_12s24M := ifelse(NTC_VENC_1A30_TC_24M > 0, NTC_VENC_1A30_TC_12M/NTC_VENC_1A30_TC_24M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_12s24M := ifelse(NTC_VENC_31AMAS_TC_24M > 0, NTC_VENC_31AMAS_TC_12M/NTC_VENC_31AMAS_TC_24M, 0), ]
  data[, r_NTC_APERT_SBS_TC_12s24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_APERT_SBS_TC_12M/NTC_APERT_SBS_TC_24M, 0), ]
  data[, r_MVALVEN_SBS_TC_12s24M := ifelse(MVALVEN_SBS_TC_24M > 0, MVALVEN_SBS_TC_12M/MVALVEN_SBS_TC_24M, 0), ]
  data[, r_MVAL_DEMANDA_SBS_TC_12s24M := ifelse(MVAL_DEMANDA_SBS_TC_24M > 0, MVAL_DEMANDA_SBS_TC_12M/MVAL_DEMANDA_SBS_TC_24M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_TC_12s24M := ifelse(MVAL_CASTIGO_SBS_TC_24M > 0, MVAL_CASTIGO_SBS_TC_12M/MVAL_CASTIGO_SBS_TC_24M, 0), ]
  data[, r_DEUDA_TOTAL_SBS_TC_12s24M := ifelse(DEUDA_TOTAL_SBS_TC_24M > 0, DEUDA_TOTAL_SBS_TC_12M/DEUDA_TOTAL_SBS_TC_24M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_12s24M := ifelse(PROM_MAX_DVEN_SBS_TC_24M > 0, PROM_MAX_DVEN_SBS_TC_12M/PROM_MAX_DVEN_SBS_TC_24M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_12s24M := ifelse(NTC_VENC_31AMAS_TC_24M > 0, NTC_VENC_31AMAS_TC_12M/NTC_VENC_31AMAS_TC_24M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_12s24M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_24M > 0, PROM_DEUDA_TOTAL_SBS_TC_12M/PROM_DEUDA_TOTAL_SBS_TC_24M, 0), ]
  data[, r_PROM_VEN_SBS_TC_12s24M := ifelse(PROM_VEN_SBS_TC_24M > 0, PROM_VEN_SBS_TC_12M/PROM_VEN_SBS_TC_24M, 0), ]
  
  data[, r_NTC_REFIN_TC_24s36M := ifelse(VAL_REFIN_TC_12M > 0, NTC_REFIN_TC_24M/VAL_REFIN_TC_12M, 0), ]
  data[, r_NTC_VENC_TC_24s36M := ifelse(NTC_VENC_TC_36M > 0, NTC_VENC_TC_24M/NTC_VENC_TC_36M, 0), ]
  data[, r_NTC_VENC_1A30_TC_24s36M := ifelse(NTC_VENC_1A30_TC_36M > 0, NTC_VENC_1A30_TC_24M/NTC_VENC_1A30_TC_36M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_24s36M := ifelse(NTC_VENC_31AMAS_TC_36M > 0, NTC_VENC_31AMAS_TC_24M/NTC_VENC_31AMAS_TC_36M, 0), ]
  data[, r_NTC_APERT_SBS_TC_24s36M := ifelse(NTC_APERT_SBS_TC_36M > 0, NTC_APERT_SBS_TC_24M/NTC_APERT_SBS_TC_36M, 0), ]
  data[, r_MVALVEN_SBS_TC_24s36M := ifelse(MVALVEN_SBS_TC_36M > 0, MVALVEN_SBS_TC_24M/MVALVEN_SBS_TC_36M, 0), ]
  data[, r_MVAL_DEMANDA_SBS_TC_24s36M := ifelse(MVAL_DEMANDA_SBS_TC_36M > 0, MVAL_DEMANDA_SBS_TC_24M/MVAL_DEMANDA_SBS_TC_36M, 0), ]
  data[, r_MVAL_CASTIGO_SBS_TC_24s36M := ifelse(MVAL_CASTIGO_SBS_TC_36M > 0, MVAL_CASTIGO_SBS_TC_24M/MVAL_CASTIGO_SBS_TC_36M, 0), ]
  #data[, r_DEUDA_TOTAL_SBS_TC_24s36M := ifelse(DEUDA_TOTAL_SBS_TC_36M > 0, DEUDA_TOTAL_SBS_TC_24M/DEUDA_TOTAL_SBS_TC_36M, 0), ]
  data[, r_PROM_MAX_DVEN_SBS_TC_24s36M := ifelse(PROM_MAX_DVEN_SBS_TC_36M > 0, PROM_MAX_DVEN_SBS_TC_24M/PROM_MAX_DVEN_SBS_TC_36M, 0), ]
  data[, r_NTC_VENC_31AMAS_TC_24s36M := ifelse(NTC_VENC_31AMAS_TC_36M > 0, NTC_VENC_31AMAS_TC_24M/NTC_VENC_31AMAS_TC_36M, 0), ]
  data[, r_PROM_DEUDA_TOTAL_SBS_TC_24s36M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_36M > 0, PROM_DEUDA_TOTAL_SBS_TC_24M/PROM_DEUDA_TOTAL_SBS_TC_36M, 0), ]
  data[, r_PROM_VEN_SBS_TC_24s36M := ifelse(PROM_VEN_SBS_TC_36M > 0, PROM_VEN_SBS_TC_24M/PROM_VEN_SBS_TC_36M, 0), ]
  
  # Ratios de Representatividad OP y TC ----------------------------------------------------
  data[, r_NOPE_REFINsNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_REFIN_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  data[, r_NOPE_REFINsNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_REFIN_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  data[, r_NOPE_REFINsNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_REFIN_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_REFINsNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_REFIN_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  #data[, r_NOPE_REFINsNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_REFIN_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  data[, r_NOPE_VENCsNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  data[, r_NOPE_VENCsNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  data[, r_NOPE_VENCsNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  data[, r_NOPE_VENCsNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  data[, r_NOPE_VENCsNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_1A30sNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_1A30_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_1A30sNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_1A30_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_1A30sNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_1A30_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_1A30sNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_1A30_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_1A30sNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_1A30_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_31A90sNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_31A90_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_31A90sNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_31A90_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_31A90sNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_31A90_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_31A90sNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_31A90_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_31A90sNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_31A90_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_91A180sNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_91A180_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_91A180sNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_91A180_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_91A180sNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_91A180_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_91A180sNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_91A180_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_91A180sNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_91A180_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_181A360sNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_181A360_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_181A360sNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_181A360_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_181A360sNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_181A360_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_181A360sNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_181A360_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_181A360sNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_181A360_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_MAYOR360sNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_MAYOR360_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_MAYOR360sNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_MAYOR360_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_MAYOR360sNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_MAYOR360_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_MAYOR360sNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_MAYOR360_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_MAYOR360sNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_MAYOR360_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # data[, r_NOPE_VENC_31AMASsNOPE_APERT_SBS_3M := ifelse(NOPE_APERT_SBS_OP_3M > 0, NOPE_VENC_31AMAS_OP_3M/NOPE_APERT_SBS_OP_3M, 0), ]
  # data[, r_NOPE_VENC_31AMASsNOPE_APERT_SBS_6M := ifelse(NOPE_APERT_SBS_OP_6M > 0, NOPE_VENC_31AMAS_OP_6M/NOPE_APERT_SBS_OP_6M, 0), ]
  # data[, r_NOPE_VENC_31AMASsNOPE_APERT_SBS_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, NOPE_VENC_31AMAS_OP_12M/NOPE_APERT_SBS_OP_12M, 0), ]
  # data[, r_NOPE_VENC_31AMASsNOPE_APERT_SBS_24M := ifelse(NOPE_APERT_SBS_OP_24M > 0, NOPE_VENC_31AMAS_OP_24M/NOPE_APERT_SBS_OP_24M, 0), ]
  # data[, r_NOPE_VENC_31AMASsNOPE_APERT_SBS_36M := ifelse(NOPE_APERT_SBS_OP_36M > 0, NOPE_VENC_31AMAS_OP_36M/NOPE_APERT_SBS_OP_36M, 0), ]
  # 
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_3M := ifelse(DEUDA_TOTAL_SBS_OP_3M > 0, MVALVEN_SBS_OP_3M/DEUDA_TOTAL_SBS_OP_3M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_6M := ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, MVALVEN_SBS_OP_6M/DEUDA_TOTAL_SBS_OP_6M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, MVALVEN_SBS_OP_12M/DEUDA_TOTAL_SBS_OP_12M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, MVALVEN_SBS_OP_24M/DEUDA_TOTAL_SBS_OP_24M, 0), ]
  #data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_36M := ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, MVALVEN_SBS_OP_36M/DEUDA_TOTAL_SBS_OP_36M, 0), ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SC_3M := ifelse(DEUDA_TOTAL_SC_OP_3M > 0, MVALVEN_SC_OP_3M/DEUDA_TOTAL_SC_OP_3M, 0), ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SC_6M := ifelse(DEUDA_TOTAL_SC_OP_6M > 0, MVALVEN_SC_OP_6M/DEUDA_TOTAL_SC_OP_6M, 0), ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SC_12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 0, MVALVEN_SC_OP_12M/DEUDA_TOTAL_SC_OP_12M, 0), ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SC_24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 0, MVALVEN_SC_OP_24M/DEUDA_TOTAL_SC_OP_24M, 0), ]
  #data[, r_MVALVEN_SCsDEUDA_TOTAL_SC_36M := ifelse(DEUDA_TOTAL_SC_OP_36M > 0, MVALVEN_SC_OP_36M/DEUDA_TOTAL_SC_OP_36M, 0), ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SICOM_3M := ifelse(DEUDA_TOTAL_SICOM_OP_3M > 0, MVALVEN_SICOM_OP_3M/DEUDA_TOTAL_SICOM_OP_3M, 0), ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SICOM_6M := ifelse(DEUDA_TOTAL_SICOM_OP_6M > 0, MVALVEN_SICOM_OP_6M/DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SICOM_12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 0, MVALVEN_SICOM_OP_12M/DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SICOM_24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 0, MVALVEN_SICOM_OP_24M/DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  #data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SICOM_36M := ifelse(DEUDA_TOTAL_SICOM_OP_36M > 0, MVALVEN_SICOM_OP_36M/DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_OTROS_3M := ifelse(DEUDA_TOTAL_OTROS_OP_3M > 0, MVALVEN_OTROS_OP_3M/DEUDA_TOTAL_OTROS_OP_3M, 0), ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_OTROS_6M := ifelse(DEUDA_TOTAL_OTROS_OP_6M > 0, MVALVEN_OTROS_OP_6M/DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_OTROS_12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 0, MVALVEN_OTROS_OP_12M/DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_OTROS_24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 0, MVALVEN_OTROS_OP_24M/DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  #data[, r_MVALVEN_OTROSsDEUDA_TOTAL_OTROS_36M := ifelse(DEUDA_TOTAL_OTROS_OP_36M > 0, MVALVEN_OTROS_OP_36M/DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  
  # data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_3M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 0, PROM_VEN_SBS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_3M, 0), ]
  # data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_6M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_VEN_SBS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0), ]
  # data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_VEN_SBS_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0), ]
  # data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_VEN_SBS_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0), ]
  # data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_36M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_VEN_SBS_OP_36M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0), ]
  # 
  # data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SC_3M := ifelse(PROM_DEUDA_TOTAL_SC_OP_3M > 0, PROM_VEN_SC_OP_3M/PROM_DEUDA_TOTAL_SC_OP_3M, 0), ]
  # data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SC_6M := ifelse(PROM_DEUDA_TOTAL_SC_OP_6M > 0, PROM_VEN_SC_OP_6M/PROM_DEUDA_TOTAL_SC_OP_6M, 0), ]
  # data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SC_12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 0, PROM_VEN_SC_OP_12M/PROM_DEUDA_TOTAL_SC_OP_12M, 0), ]
  # data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SC_24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 0, PROM_VEN_SC_OP_24M/PROM_DEUDA_TOTAL_SC_OP_24M, 0), ]
  # data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SC_36M := ifelse(PROM_DEUDA_TOTAL_SC_OP_36M > 0, PROM_VEN_SC_OP_36M/PROM_DEUDA_TOTAL_SC_OP_36M, 0), ]
  # 
  # data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SICOM_3M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_3M > 0, PROM_VEN_SICOM_OP_3M/PROM_DEUDA_TOTAL_SICOM_OP_3M, 0), ]
  # data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SICOM_6M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_6M > 0, PROM_VEN_SICOM_OP_6M/PROM_DEUDA_TOTAL_SICOM_OP_6M, 0), ]
  # data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SICOM_12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 0, PROM_VEN_SICOM_OP_12M/PROM_DEUDA_TOTAL_SICOM_OP_12M, 0), ]
  # data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SICOM_24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 0, PROM_VEN_SICOM_OP_24M/PROM_DEUDA_TOTAL_SICOM_OP_24M, 0), ]
  # data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SICOM_36M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_36M > 0, PROM_VEN_SICOM_OP_36M/PROM_DEUDA_TOTAL_SICOM_OP_36M, 0), ]
  # 
  # data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_OTROS_3M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_3M > 0, PROM_VEN_OTROS_OP_3M/PROM_DEUDA_TOTAL_OTROS_OP_3M, 0), ]
  # data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_OTROS_6M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_6M > 0, PROM_VEN_OTROS_OP_6M/PROM_DEUDA_TOTAL_OTROS_OP_6M, 0), ]
  # data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_OTROS_12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 0, PROM_VEN_OTROS_OP_12M/PROM_DEUDA_TOTAL_OTROS_OP_12M, 0), ]
  # data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_OTROS_24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 0, PROM_VEN_OTROS_OP_24M/PROM_DEUDA_TOTAL_OTROS_OP_24M, 0), ]
  # data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_OTROS_36M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_36M > 0, PROM_VEN_OTROS_OP_36M/PROM_DEUDA_TOTAL_OTROS_OP_36M, 0), ]
  # 
  
  # data[, r_NTC_REFINsNTC_APERT_SBS_TC_3M := ifelse(NTC_APERT_SBS_TC_3M > 0, NTC_REFIN_TC_3M/NTC_APERT_SBS_TC_3M, 0), ]
  # data[, r_NTC_REFINsNTC_APERT_SBS_TC_6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_REFIN_TC_6M/NTC_APERT_SBS_TC_6M, 0), ]
  # data[, r_NTC_REFINsNTC_APERT_SBS_TC_12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_REFIN_TC_12M/NTC_APERT_SBS_TC_12M, 0), ]
  # data[, r_NTC_REFINsNTC_APERT_SBS_TC_24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_REFIN_TC_24M/NTC_APERT_SBS_TC_24M, 0), ]
  # data[, r_VAL_REFINsNTC_APERT_SBS_TC_36M := ifelse(NTC_APERT_SBS_TC_36M > 0, VAL_REFIN_TC_12M/NTC_APERT_SBS_TC_36M, 0), ]
  # data[, r_NTC_VENCsNTC_APERT_SBS_TC_3M := ifelse(NTC_APERT_SBS_TC_3M > 0, NTC_VENC_TC_3M/NTC_APERT_SBS_TC_3M, 0), ]
  # data[, r_NTC_VENCsNTC_APERT_SBS_TC_6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_VENC_TC_6M/NTC_APERT_SBS_TC_6M, 0), ]
  # data[, r_NTC_VENCsNTC_APERT_SBS_TC_12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_VENC_TC_12M/NTC_APERT_SBS_TC_12M, 0), ]
  data[, r_NTC_VENCsNTC_APERT_SBS_TC_24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_VENC_TC_24M/NTC_APERT_SBS_TC_24M, 0), ]
  # data[, r_NTC_VENCsNTC_APERT_SBS_TC_36M := ifelse(NTC_APERT_SBS_TC_36M > 0, NTC_VENC_TC_36M/NTC_APERT_SBS_TC_36M, 0), ]
  # data[, r_NTC_VENC_1A30sNTC_APERT_SBS_TC_3M := ifelse(NTC_APERT_SBS_TC_3M > 0, NTC_VENC_1A30_TC_3M/NTC_APERT_SBS_TC_3M, 0), ]
  # data[, r_NTC_VENC_1A30sNTC_APERT_SBS_TC_6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_VENC_1A30_TC_6M/NTC_APERT_SBS_TC_6M, 0), ]
  # data[, r_NTC_VENC_1A30sNTC_APERT_SBS_TC_12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_VENC_1A30_TC_12M/NTC_APERT_SBS_TC_12M, 0), ]
  # data[, r_NTC_VENC_1A30sNTC_APERT_SBS_TC_24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_VENC_1A30_TC_24M/NTC_APERT_SBS_TC_24M, 0), ]
  # data[, r_NTC_VENC_1A30sNTC_APERT_SBS_TC_36M := ifelse(NTC_APERT_SBS_TC_36M > 0, NTC_VENC_1A30_TC_36M/NTC_APERT_SBS_TC_36M, 0), ]
  # data[, r_NTC_VENC_31AMASsNTC_APERT_SBS_TC_3M := ifelse(NTC_APERT_SBS_TC_3M > 0, NTC_VENC_31AMAS_TC_3M/NTC_APERT_SBS_TC_3M, 0), ]
  # data[, r_NTC_VENC_31AMASsNTC_APERT_SBS_TC_6M := ifelse(NTC_APERT_SBS_TC_6M > 0, NTC_VENC_31AMAS_TC_6M/NTC_APERT_SBS_TC_6M, 0), ]
  # data[, r_NTC_VENC_31AMASsNTC_APERT_SBS_TC_12M := ifelse(NTC_APERT_SBS_TC_12M > 0, NTC_VENC_31AMAS_TC_12M/NTC_APERT_SBS_TC_12M, 0), ]
  # data[, r_NTC_VENC_31AMASsNTC_APERT_SBS_TC_24M := ifelse(NTC_APERT_SBS_TC_24M > 0, NTC_VENC_31AMAS_TC_24M/NTC_APERT_SBS_TC_24M, 0), ]
  # data[, r_NTC_VENC_31AMASsNTC_APERT_SBS_TC_36M := ifelse(NTC_APERT_SBS_TC_36M > 0, NTC_VENC_31AMAS_TC_36M/NTC_APERT_SBS_TC_36M, 0), ]
  # 
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_TC_3M := ifelse(DEUDA_TOTAL_SBS_TC_3M > 0, MVALVEN_SBS_TC_3M/DEUDA_TOTAL_SBS_TC_3M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_TC_6M := ifelse(DEUDA_TOTAL_SBS_TC_6M > 0, MVALVEN_SBS_TC_6M/DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_TC_12M := ifelse(DEUDA_TOTAL_SBS_TC_12M > 0, MVALVEN_SBS_TC_12M/DEUDA_TOTAL_SBS_TC_12M, 0), ]
  data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_TC_24M := ifelse(DEUDA_TOTAL_SBS_TC_24M > 0, MVALVEN_SBS_TC_24M/DEUDA_TOTAL_SBS_TC_24M, 0), ]
  #data[, r_MVALVEN_SBSsDEUDA_TOTAL_SBS_TC_36M := ifelse(DEUDA_TOTAL_SBS_TC_36M > 0, MVALVEN_SBS_TC_36M/DEUDA_TOTAL_SBS_TC_36M, 0), ]
  
  data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_TC_3M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_3M > 0, PROM_VEN_SBS_TC_3M/PROM_DEUDA_TOTAL_SBS_TC_3M, 0), ]
  data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_TC_6M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_6M > 0, PROM_VEN_SBS_TC_6M/PROM_DEUDA_TOTAL_SBS_TC_6M, 0), ]
  data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_TC_12M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_12M > 0, PROM_VEN_SBS_TC_12M/PROM_DEUDA_TOTAL_SBS_TC_12M, 0), ]
  data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_TC_24M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_24M > 0, PROM_VEN_SBS_TC_24M/PROM_DEUDA_TOTAL_SBS_TC_24M, 0), ]
  data[, r_PROM_VEN_SBSsPROM_DEUDA_TOTAL_SBS_TC_36M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_36M > 0, PROM_VEN_SBS_TC_36M/PROM_DEUDA_TOTAL_SBS_TC_36M, 0), ]
  
  # Ratios de Entre Sistemas OP y TC ----------------------------------------------------
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SBS_3M := ifelse(MVALVEN_SC_OP_3M == 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(MVALVEN_SC_OP_3M > 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_3M > 0, MVALVEN_SC_OP_3M/DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SBS_6M := ifelse(MVALVEN_SC_OP_6M == 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(MVALVEN_SC_OP_6M > 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, MVALVEN_SC_OP_6M/DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SBS_12M := ifelse(MVALVEN_SC_OP_12M == 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(MVALVEN_SC_OP_12M > 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, MVALVEN_SC_OP_12M/DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_MVALVEN_SCsDEUDA_TOTAL_SBS_24M := ifelse(MVALVEN_SC_OP_24M == 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(MVALVEN_SC_OP_24M > 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, MVALVEN_SC_OP_24M/DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  #data[, r_MVALVEN_SCsDEUDA_TOTAL_SBS_36M := ifelse(MVALVEN_SC_OP_36M == 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(MVALVEN_SC_OP_36M > 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, MVALVEN_SC_OP_36M/DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SBS_3M := ifelse(MVALVEN_SICOM_OP_3M == 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(MVALVEN_SICOM_OP_3M > 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_3M > 0, MVALVEN_SICOM_OP_3M/DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SBS_6M := ifelse(MVALVEN_SICOM_OP_6M == 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(MVALVEN_SICOM_OP_6M > 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, MVALVEN_SICOM_OP_6M/DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SBS_12M := ifelse(MVALVEN_SICOM_OP_12M == 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(MVALVEN_SICOM_OP_12M > 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, MVALVEN_SICOM_OP_12M/DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SBS_24M := ifelse(MVALVEN_SICOM_OP_24M == 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(MVALVEN_SICOM_OP_24M > 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, MVALVEN_SICOM_OP_24M/DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  #data[, r_MVALVEN_SICOMsDEUDA_TOTAL_SBS_36M := ifelse(MVALVEN_SICOM_OP_36M == 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(MVALVEN_SICOM_OP_36M > 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, MVALVEN_SICOM_OP_36M/DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_SBS_3M := ifelse(MVALVEN_OTROS_OP_3M == 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(MVALVEN_OTROS_OP_3M > 0 & DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_3M > 0, MVALVEN_OTROS_OP_3M/DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_SBS_6M := ifelse(MVALVEN_OTROS_OP_6M == 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(MVALVEN_OTROS_OP_6M > 0 & DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_6M > 0, MVALVEN_OTROS_OP_6M/DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_SBS_12M := ifelse(MVALVEN_OTROS_OP_12M == 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(MVALVEN_OTROS_OP_12M > 0 & DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_12M > 0, MVALVEN_OTROS_OP_12M/DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_MVALVEN_OTROSsDEUDA_TOTAL_SBS_24M := ifelse(MVALVEN_OTROS_OP_24M == 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(MVALVEN_OTROS_OP_24M > 0 & DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_24M > 0, MVALVEN_OTROS_OP_24M/DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  #data[, r_MVALVEN_OTROSsDEUDA_TOTAL_SBS_36M := ifelse(MVALVEN_OTROS_OP_36M == 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(MVALVEN_OTROS_OP_36M > 0 & DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(DEUDA_TOTAL_SBS_OP_36M > 0, MVALVEN_OTROS_OP_36M/DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  
  data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SBS_3M := ifelse(PROM_VEN_SC_OP_3M == 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(PROM_VEN_SC_OP_3M > 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 0, PROM_VEN_SC_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SBS_6M := ifelse(PROM_VEN_SC_OP_6M == 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(PROM_VEN_SC_OP_6M > 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_VEN_SC_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SBS_12M := ifelse(PROM_VEN_SC_OP_12M == 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(PROM_VEN_SC_OP_12M > 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_VEN_SC_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SBS_24M := ifelse(PROM_VEN_SC_OP_24M == 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(PROM_VEN_SC_OP_24M > 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_VEN_SC_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  data[, r_PROM_VEN_SCsPROM_DEUDA_TOTAL_SBS_36M := ifelse(PROM_VEN_SC_OP_36M == 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(PROM_VEN_SC_OP_36M > 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_VEN_SC_OP_36M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  
  data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SBS_3M := ifelse(PROM_VEN_SICOM_OP_3M == 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(PROM_VEN_SICOM_OP_3M > 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 0, PROM_VEN_SICOM_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SBS_6M := ifelse(PROM_VEN_SICOM_OP_6M == 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(PROM_VEN_SICOM_OP_6M > 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_VEN_SICOM_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SBS_12M := ifelse(PROM_VEN_SICOM_OP_12M == 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(PROM_VEN_SICOM_OP_12M > 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_VEN_SICOM_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SBS_24M := ifelse(PROM_VEN_SICOM_OP_24M == 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(PROM_VEN_SICOM_OP_24M > 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_VEN_SICOM_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  data[, r_PROM_VEN_SICOMsPROM_DEUDA_TOTAL_SBS_36M := ifelse(PROM_VEN_SICOM_OP_36M == 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(PROM_VEN_SICOM_OP_36M > 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_VEN_SICOM_OP_36M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  
  data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_SBS_3M := ifelse(PROM_VEN_OTROS_OP_3M == 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(PROM_VEN_OTROS_OP_3M > 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 0, PROM_VEN_OTROS_OP_3M/PROM_DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_SBS_6M := ifelse(PROM_VEN_OTROS_OP_6M == 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(PROM_VEN_OTROS_OP_6M > 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_VEN_OTROS_OP_6M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_SBS_12M := ifelse(PROM_VEN_OTROS_OP_12M == 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(PROM_VEN_OTROS_OP_12M > 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_VEN_OTROS_OP_12M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_SBS_24M := ifelse(PROM_VEN_OTROS_OP_24M == 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(PROM_VEN_OTROS_OP_24M > 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_VEN_OTROS_OP_24M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  data[, r_PROM_VEN_OTROSsPROM_DEUDA_TOTAL_SBS_36M := ifelse(PROM_VEN_OTROS_OP_36M == 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(PROM_VEN_OTROS_OP_36M > 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_VEN_OTROS_OP_36M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  
  data[, r_PROM_VEN_SBS_TCsPROM_DEUDA_TOTAL_SBS_OP_3M := ifelse(PROM_VEN_SBS_TC_3M == 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 0, ifelse(PROM_VEN_SBS_TC_3M > 0 & PROM_DEUDA_TOTAL_SBS_OP_3M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 0, PROM_VEN_SBS_TC_3M/PROM_DEUDA_TOTAL_SBS_OP_3M, 0))) ]
  data[, r_PROM_VEN_SBS_TCsPROM_DEUDA_TOTAL_SBS_OP_6M := ifelse(PROM_VEN_SBS_TC_6M == 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 0, ifelse(PROM_VEN_SBS_TC_6M > 0 & PROM_DEUDA_TOTAL_SBS_OP_6M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 0, PROM_VEN_SBS_TC_6M/PROM_DEUDA_TOTAL_SBS_OP_6M, 0))) ]
  data[, r_PROM_VEN_SBS_TCsPROM_DEUDA_TOTAL_SBS_OP_12M := ifelse(PROM_VEN_SBS_TC_12M == 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 0, ifelse(PROM_VEN_SBS_TC_12M > 0 & PROM_DEUDA_TOTAL_SBS_OP_12M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 0, PROM_VEN_SBS_TC_12M/PROM_DEUDA_TOTAL_SBS_OP_12M, 0))) ]
  data[, r_PROM_VEN_SBS_TCsPROM_DEUDA_TOTAL_SBS_OP_24M := ifelse(PROM_VEN_SBS_TC_24M == 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 0, ifelse(PROM_VEN_SBS_TC_24M > 0 & PROM_DEUDA_TOTAL_SBS_OP_24M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 0, PROM_VEN_SBS_TC_24M/PROM_DEUDA_TOTAL_SBS_OP_24M, 0))) ]
  data[, r_PROM_VEN_SBS_TCsPROM_DEUDA_TOTAL_SBS_OP_36M := ifelse(PROM_VEN_SBS_TC_36M == 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 0, ifelse(PROM_VEN_SBS_TC_36M > 0 & PROM_DEUDA_TOTAL_SBS_OP_36M == 0, 1, ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 0, PROM_VEN_SBS_TC_36M/PROM_DEUDA_TOTAL_SBS_OP_36M, 0))) ]
  
  data[, r_MVALVEN_SBS_TCsMVALVEN_SBS_OP_3M := ifelse(MVALVEN_SBS_TC_3M == 0 & MVALVEN_SBS_OP_3M == 0, 0, ifelse(MVALVEN_SBS_TC_3M > 0 & MVALVEN_SBS_OP_3M == 0, 1, ifelse(MVALVEN_SBS_OP_3M > 0, MVALVEN_SBS_TC_3M/MVALVEN_SBS_OP_3M, 0))) ]
  data[, r_MVALVEN_SBS_TCsMVALVEN_SBS_OP_6M := ifelse(MVALVEN_SBS_TC_6M == 0 & MVALVEN_SBS_OP_6M == 0, 0, ifelse(MVALVEN_SBS_TC_6M > 0 & MVALVEN_SBS_OP_6M == 0, 1, ifelse(MVALVEN_SBS_OP_6M > 0, MVALVEN_SBS_TC_6M/MVALVEN_SBS_OP_6M, 0))) ]
  data[, r_MVALVEN_SBS_TCsMVALVEN_SBS_OP_12M := ifelse(MVALVEN_SBS_TC_12M == 0 & MVALVEN_SBS_OP_12M == 0, 0, ifelse(MVALVEN_SBS_TC_12M > 0 & MVALVEN_SBS_OP_12M == 0, 1, ifelse(MVALVEN_SBS_OP_12M > 0, MVALVEN_SBS_TC_12M/MVALVEN_SBS_OP_12M, 0))) ]
  data[, r_MVALVEN_SBS_TCsMVALVEN_SBS_OP_24M := ifelse(MVALVEN_SBS_TC_24M == 0 & MVALVEN_SBS_OP_24M == 0, 0, ifelse(MVALVEN_SBS_TC_24M > 0 & MVALVEN_SBS_OP_24M == 0, 1, ifelse(MVALVEN_SBS_OP_24M > 0, MVALVEN_SBS_TC_24M/MVALVEN_SBS_OP_24M, 0))) ]
  data[, r_MVALVEN_SBS_TCsMVALVEN_SBS_OP_36M := ifelse(MVALVEN_SBS_TC_36M == 0 & MVALVEN_SBS_OP_36M == 0, 0, ifelse(MVALVEN_SBS_TC_36M > 0 & MVALVEN_SBS_OP_36M == 0, 1, ifelse(MVALVEN_SBS_OP_36M > 0, MVALVEN_SBS_TC_36M/MVALVEN_SBS_OP_36M, 0))) ]
  
  data[, r_DEUDA_TOTAL_SCE_3a6M := ifelse(DEUDA_TOTAL_SCE_6M > 0, DEUDA_TOTAL_SCE_3M/DEUDA_TOTAL_SCE_6M, 0)]
  data[, r_DEUDA_TOTAL_SCE_6a12M := ifelse(DEUDA_TOTAL_SCE_12M > 0, DEUDA_TOTAL_SCE_6M/DEUDA_TOTAL_SCE_12M, 0)]
  data[, r_DEUDA_TOTAL_SCE_12a24M := ifelse(DEUDA_TOTAL_SCE_24M > 0, DEUDA_TOTAL_SCE_12M/DEUDA_TOTAL_SCE_24M, 0)]
  
  
  # Transformaciones logaritmicas de saldos promedios OP y TC ----------------------------------------------------
  # data[, LN_MVALVEN_SBS_OP_3M := ifelse(MVALVEN_SBS_OP_3M > 1, log(MVALVEN_SBS_OP_3M), 0), ]
  # data[, LN_MVALVEN_SBS_OP_6M := ifelse(MVALVEN_SBS_OP_6M > 1, log(MVALVEN_SBS_OP_6M), 0), ]
  # data[, LN_MVALVEN_SBS_OP_12M := ifelse(MVALVEN_SBS_OP_12M > 1, log(MVALVEN_SBS_OP_12M), 0), ]
  # data[, LN_MVALVEN_SBS_OP_24M := ifelse(MVALVEN_SBS_OP_24M > 1, log(MVALVEN_SBS_OP_24M), 0), ]
  # data[, LN_MVALVEN_SBS_OP_36M := ifelse(MVALVEN_SBS_OP_36M > 1, log(MVALVEN_SBS_OP_36M), 0), ]
  # data[, LN_MVALVEN_SC_OP_3M := ifelse(MVALVEN_SC_OP_3M > 1, log(MVALVEN_SC_OP_3M), 0), ]
  # data[, LN_MVALVEN_SC_OP_6M := ifelse(MVALVEN_SC_OP_6M > 1, log(MVALVEN_SC_OP_6M), 0), ]
  # data[, LN_MVALVEN_SC_OP_12M := ifelse(MVALVEN_SC_OP_12M > 1, log(MVALVEN_SC_OP_12M), 0), ]
  # data[, LN_MVALVEN_SC_OP_24M := ifelse(MVALVEN_SC_OP_24M > 1, log(MVALVEN_SC_OP_24M), 0), ]
  # data[, LN_MVALVEN_SC_OP_36M := ifelse(MVALVEN_SC_OP_36M > 1, log(MVALVEN_SC_OP_36M), 0), ]
  # data[, LN_MVALVEN_SICOM_OP_3M := ifelse(MVALVEN_SICOM_OP_3M > 1, log(MVALVEN_SICOM_OP_3M), 0), ]
  # data[, LN_MVALVEN_SICOM_OP_6M := ifelse(MVALVEN_SICOM_OP_6M > 1, log(MVALVEN_SICOM_OP_6M), 0), ]
  # data[, LN_MVALVEN_SICOM_OP_12M := ifelse(MVALVEN_SICOM_OP_12M > 1, log(MVALVEN_SICOM_OP_12M), 0), ]
  # data[, LN_MVALVEN_SICOM_OP_24M := ifelse(MVALVEN_SICOM_OP_24M > 1, log(MVALVEN_SICOM_OP_24M), 0), ]
  # data[, LN_MVALVEN_SICOM_OP_36M := ifelse(MVALVEN_SICOM_OP_36M > 1, log(MVALVEN_SICOM_OP_36M), 0), ]
  # data[, LN_MVALVEN_OTROS_OP_3M := ifelse(MVALVEN_OTROS_OP_3M > 1, log(MVALVEN_OTROS_OP_3M), 0), ]
  # data[, LN_MVALVEN_OTROS_OP_6M := ifelse(MVALVEN_OTROS_OP_6M > 1, log(MVALVEN_OTROS_OP_6M), 0), ]
  # data[, LN_MVALVEN_OTROS_OP_12M := ifelse(MVALVEN_OTROS_OP_12M > 1, log(MVALVEN_OTROS_OP_12M), 0), ]
  # data[, LN_MVALVEN_OTROS_OP_24M := ifelse(MVALVEN_OTROS_OP_24M > 1, log(MVALVEN_OTROS_OP_24M), 0), ]
  # data[, LN_MVALVEN_OTROS_OP_36M := ifelse(MVALVEN_OTROS_OP_36M > 1, log(MVALVEN_OTROS_OP_36M), 0), ]
  # data[, LN_DEUDA_TOTAL_SBS_OP_3M := ifelse(DEUDA_TOTAL_SBS_OP_3M > 1, log(DEUDA_TOTAL_SBS_OP_3M), 0), ]
  # data[, LN_DEUDA_TOTAL_SBS_OP_6M := ifelse(DEUDA_TOTAL_SBS_OP_6M > 1, log(DEUDA_TOTAL_SBS_OP_6M), 0), ]
  # data[, LN_DEUDA_TOTAL_SBS_OP_12M := ifelse(DEUDA_TOTAL_SBS_OP_12M > 1, log(DEUDA_TOTAL_SBS_OP_12M), 0), ]
  # data[, LN_DEUDA_TOTAL_SBS_OP_24M := ifelse(DEUDA_TOTAL_SBS_OP_24M > 1, log(DEUDA_TOTAL_SBS_OP_24M), 0), ]
  # #data[, LN_DEUDA_TOTAL_SBS_OP_36M := ifelse(DEUDA_TOTAL_SBS_OP_36M > 1, log(DEUDA_TOTAL_SBS_OP_36M), 0), ]
  # data[, LN_DEUDA_TOTAL_SC_OP_3M := ifelse(DEUDA_TOTAL_SC_OP_3M > 1, log(DEUDA_TOTAL_SC_OP_3M), 0), ]
  # data[, LN_DEUDA_TOTAL_SC_OP_6M := ifelse(DEUDA_TOTAL_SC_OP_6M > 1, log(DEUDA_TOTAL_SC_OP_6M), 0), ]
  # data[, LN_DEUDA_TOTAL_SC_OP_12M := ifelse(DEUDA_TOTAL_SC_OP_12M > 1, log(DEUDA_TOTAL_SC_OP_12M), 0), ]
  # data[, LN_DEUDA_TOTAL_SC_OP_24M := ifelse(DEUDA_TOTAL_SC_OP_24M > 1, log(DEUDA_TOTAL_SC_OP_24M), 0), ]
  # #data[, LN_DEUDA_TOTAL_SC_OP_36M := ifelse(DEUDA_TOTAL_SC_OP_36M > 1, log(DEUDA_TOTAL_SC_OP_36M), 0), ]
  # data[, LN_DEUDA_TOTAL_SICOM_OP_3M := ifelse(DEUDA_TOTAL_SICOM_OP_3M > 1, log(DEUDA_TOTAL_SICOM_OP_3M), 0), ]
  # data[, LN_DEUDA_TOTAL_SICOM_OP_6M := ifelse(DEUDA_TOTAL_SICOM_OP_6M > 1, log(DEUDA_TOTAL_SICOM_OP_6M), 0), ]
  # data[, LN_DEUDA_TOTAL_SICOM_OP_12M := ifelse(DEUDA_TOTAL_SICOM_OP_12M > 1, log(DEUDA_TOTAL_SICOM_OP_12M), 0), ]
  # data[, LN_DEUDA_TOTAL_SICOM_OP_24M := ifelse(DEUDA_TOTAL_SICOM_OP_24M > 1, log(DEUDA_TOTAL_SICOM_OP_24M), 0), ]
  # #data[, LN_DEUDA_TOTAL_SICOM_OP_36M := ifelse(DEUDA_TOTAL_SICOM_OP_36M > 1, log(DEUDA_TOTAL_SICOM_OP_36M), 0), ]
  # data[, LN_DEUDA_TOTAL_OTROS_OP_3M := ifelse(DEUDA_TOTAL_OTROS_OP_3M > 1, log(DEUDA_TOTAL_OTROS_OP_3M), 0), ]
  # data[, LN_DEUDA_TOTAL_OTROS_OP_6M := ifelse(DEUDA_TOTAL_OTROS_OP_6M > 1, log(DEUDA_TOTAL_OTROS_OP_6M), 0), ]
  # data[, LN_DEUDA_TOTAL_OTROS_OP_12M := ifelse(DEUDA_TOTAL_OTROS_OP_12M > 1, log(DEUDA_TOTAL_OTROS_OP_12M), 0), ]
  # data[, LN_DEUDA_TOTAL_OTROS_OP_24M := ifelse(DEUDA_TOTAL_OTROS_OP_24M > 1, log(DEUDA_TOTAL_OTROS_OP_24M), 0), ]
  # #data[, LN_DEUDA_TOTAL_OTROS_OP_36M := ifelse(DEUDA_TOTAL_OTROS_OP_36M > 1, log(DEUDA_TOTAL_OTROS_OP_36M), 0), ]
  # data[, LN_PROM_XVEN_SBS_OP_3M := ifelse(PROM_XVEN_SBS_OP_3M > 1, log(PROM_XVEN_SBS_OP_3M), 0), ]
  # data[, LN_PROM_XVEN_SBS_OP_6M := ifelse(PROM_XVEN_SBS_OP_6M > 1, log(PROM_XVEN_SBS_OP_6M), 0), ]
  # data[, LN_PROM_XVEN_SBS_OP_12M := ifelse(PROM_XVEN_SBS_OP_12M > 1, log(PROM_XVEN_SBS_OP_12M), 0), ]
  # data[, LN_PROM_XVEN_SBS_OP_24M := ifelse(PROM_XVEN_SBS_OP_24M > 1, log(PROM_XVEN_SBS_OP_24M), 0), ]
  # data[, LN_PROM_XVEN_SBS_OP_36M := ifelse(PROM_XVEN_SBS_OP_36M > 1, log(PROM_XVEN_SBS_OP_36M), 0), ]
  # data[, LN_PROM_NDI_SBS_OP_3M := ifelse(PROM_NDI_SBS_OP_3M > 1, log(PROM_NDI_SBS_OP_3M), 0), ]
  # data[, LN_PROM_NDI_SBS_OP_6M := ifelse(PROM_NDI_SBS_OP_6M > 1, log(PROM_NDI_SBS_OP_6M), 0), ]
  # data[, LN_PROM_NDI_SBS_OP_12M := ifelse(PROM_NDI_SBS_OP_12M > 1, log(PROM_NDI_SBS_OP_12M), 0), ]
  # data[, LN_PROM_NDI_SBS_OP_24M := ifelse(PROM_NDI_SBS_OP_24M > 1, log(PROM_NDI_SBS_OP_24M), 0), ]
  # data[, LN_PROM_NDI_SBS_OP_36M := ifelse(PROM_NDI_SBS_OP_36M > 1, log(PROM_NDI_SBS_OP_36M), 0), ]
  # data[, LN_PROM_VEN_SBS_OP_3M := ifelse(PROM_VEN_SBS_OP_3M > 1, log(PROM_VEN_SBS_OP_3M), 0), ]
  # data[, LN_PROM_VEN_SBS_OP_6M := ifelse(PROM_VEN_SBS_OP_6M > 1, log(PROM_VEN_SBS_OP_6M), 0), ]
  # data[, LN_PROM_VEN_SBS_OP_12M := ifelse(PROM_VEN_SBS_OP_12M > 1, log(PROM_VEN_SBS_OP_12M), 0), ]
  # data[, LN_PROM_VEN_SBS_OP_24M := ifelse(PROM_VEN_SBS_OP_24M > 1, log(PROM_VEN_SBS_OP_24M), 0), ]
  # data[, LN_PROM_VEN_SBS_OP_36M := ifelse(PROM_VEN_SBS_OP_36M > 1, log(PROM_VEN_SBS_OP_36M), 0), ]
  # data[, LN_PROM_VEN_SC_OP_3M := ifelse(PROM_VEN_SC_OP_3M > 1, log(PROM_VEN_SC_OP_3M), 0), ]
  # data[, LN_PROM_VEN_SC_OP_6M := ifelse(PROM_VEN_SC_OP_6M > 1, log(PROM_VEN_SC_OP_6M), 0), ]
  # data[, LN_PROM_VEN_SC_OP_12M := ifelse(PROM_VEN_SC_OP_12M > 1, log(PROM_VEN_SC_OP_12M), 0), ]
  # data[, LN_PROM_VEN_SC_OP_24M := ifelse(PROM_VEN_SC_OP_24M > 1, log(PROM_VEN_SC_OP_24M), 0), ]
  # data[, LN_PROM_VEN_SC_OP_36M := ifelse(PROM_VEN_SC_OP_36M > 1, log(PROM_VEN_SC_OP_36M), 0), ]
  # data[, LN_PROM_VEN_SICOM_OP_3M := ifelse(PROM_VEN_SICOM_OP_3M > 1, log(PROM_VEN_SICOM_OP_3M), 0), ]
  # data[, LN_PROM_VEN_SICOM_OP_6M := ifelse(PROM_VEN_SICOM_OP_6M > 1, log(PROM_VEN_SICOM_OP_6M), 0), ]
  # data[, LN_PROM_VEN_SICOM_OP_12M := ifelse(PROM_VEN_SICOM_OP_12M > 1, log(PROM_VEN_SICOM_OP_12M), 0), ]
  # data[, LN_PROM_VEN_SICOM_OP_24M := ifelse(PROM_VEN_SICOM_OP_24M > 1, log(PROM_VEN_SICOM_OP_24M), 0), ]
  # data[, LN_PROM_VEN_SICOM_OP_36M := ifelse(PROM_VEN_SICOM_OP_36M > 1, log(PROM_VEN_SICOM_OP_36M), 0), ]
  # data[, LN_PROM_VEN_OTROS_OP_3M := ifelse(PROM_VEN_OTROS_OP_3M > 1, log(PROM_VEN_OTROS_OP_3M), 0), ]
  # data[, LN_PROM_VEN_OTROS_OP_6M := ifelse(PROM_VEN_OTROS_OP_6M > 1, log(PROM_VEN_OTROS_OP_6M), 0), ]
  # data[, LN_PROM_VEN_OTROS_OP_12M := ifelse(PROM_VEN_OTROS_OP_12M > 1, log(PROM_VEN_OTROS_OP_12M), 0), ]
  # data[, LN_PROM_VEN_OTROS_OP_24M := ifelse(PROM_VEN_OTROS_OP_24M > 1, log(PROM_VEN_OTROS_OP_24M), 0), ]
  # data[, LN_PROM_VEN_OTROS_OP_36M := ifelse(PROM_VEN_OTROS_OP_36M > 1, log(PROM_VEN_OTROS_OP_36M), 0), ]
  # 
  # data[, LN_PROM_DEUDA_TOTAL_SBS_OP_3M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_3M > 1, log(PROM_DEUDA_TOTAL_SBS_OP_3M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_OP_6M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_6M > 1, log(PROM_DEUDA_TOTAL_SBS_OP_6M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_OP_12M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_12M > 1, log(PROM_DEUDA_TOTAL_SBS_OP_12M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_OP_24M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_24M > 1, log(PROM_DEUDA_TOTAL_SBS_OP_24M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_OP_36M := ifelse(PROM_DEUDA_TOTAL_SBS_OP_36M > 1, log(PROM_DEUDA_TOTAL_SBS_OP_36M), 0), ]
  # 
  # data[, LN_PROM_DEUDA_TOTAL_SC_OP_3M := ifelse(PROM_DEUDA_TOTAL_SC_OP_3M > 1, log(PROM_DEUDA_TOTAL_SC_OP_3M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SC_OP_6M := ifelse(PROM_DEUDA_TOTAL_SC_OP_6M > 1, log(PROM_DEUDA_TOTAL_SC_OP_6M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SC_OP_12M := ifelse(PROM_DEUDA_TOTAL_SC_OP_12M > 1, log(PROM_DEUDA_TOTAL_SC_OP_12M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SC_OP_24M := ifelse(PROM_DEUDA_TOTAL_SC_OP_24M > 1, log(PROM_DEUDA_TOTAL_SC_OP_24M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SC_OP_36M := ifelse(PROM_DEUDA_TOTAL_SC_OP_36M > 1, log(PROM_DEUDA_TOTAL_SC_OP_36M), 0), ]
  # 
  # data[, LN_PROM_DEUDA_TOTAL_SICOM_OP_3M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_3M > 1, log(PROM_DEUDA_TOTAL_SICOM_OP_3M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SICOM_OP_6M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_6M > 1, log(PROM_DEUDA_TOTAL_SICOM_OP_6M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SICOM_OP_12M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_12M > 1, log(PROM_DEUDA_TOTAL_SICOM_OP_12M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SICOM_OP_24M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_24M > 1, log(PROM_DEUDA_TOTAL_SICOM_OP_24M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SICOM_OP_36M := ifelse(PROM_DEUDA_TOTAL_SICOM_OP_36M > 1, log(PROM_DEUDA_TOTAL_SICOM_OP_36M), 0), ]
  # 
  # data[, LN_PROM_DEUDA_TOTAL_OTROS_OP_3M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_3M > 1, log(PROM_DEUDA_TOTAL_OTROS_OP_3M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_OTROS_OP_6M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_6M > 1, log(PROM_DEUDA_TOTAL_OTROS_OP_6M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_OTROS_OP_12M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_12M > 1, log(PROM_DEUDA_TOTAL_OTROS_OP_12M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_OTROS_OP_24M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_24M > 1, log(PROM_DEUDA_TOTAL_OTROS_OP_24M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_OTROS_OP_36M := ifelse(PROM_DEUDA_TOTAL_OTROS_OP_36M > 1, log(PROM_DEUDA_TOTAL_OTROS_OP_36M), 0), ]
  # 
  # data[, LN_PROM_DEUDA_TOTAL_SBS_TC_3M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_3M > 1, log(PROM_DEUDA_TOTAL_SBS_TC_3M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_TC_6M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_6M > 1, log(PROM_DEUDA_TOTAL_SBS_TC_6M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_TC_12M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_12M > 1, log(PROM_DEUDA_TOTAL_SBS_TC_12M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_TC_24M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_24M > 1, log(PROM_DEUDA_TOTAL_SBS_TC_24M), 0), ]
  # data[, LN_PROM_DEUDA_TOTAL_SBS_TC_36M := ifelse(PROM_DEUDA_TOTAL_SBS_TC_36M > 1, log(PROM_DEUDA_TOTAL_SBS_TC_36M), 0), ]
  # data[, LN_PROM_VEN_SBS_TC_3M := ifelse(PROM_VEN_SBS_TC_3M > 1, log(PROM_VEN_SBS_TC_3M), 0), ]
  # data[, LN_PROM_VEN_SBS_TC_6M := ifelse(PROM_VEN_SBS_TC_6M > 1, log(PROM_VEN_SBS_TC_6M), 0), ]
  # data[, LN_PROM_VEN_SBS_TC_12M := ifelse(PROM_VEN_SBS_TC_12M > 1, log(PROM_VEN_SBS_TC_12M), 0), ]
  # data[, LN_PROM_VEN_SBS_TC_24M := ifelse(PROM_VEN_SBS_TC_24M > 1, log(PROM_VEN_SBS_TC_24M), 0), ]
  # data[, LN_PROM_VEN_SBS_TC_36M := ifelse(PROM_VEN_SBS_TC_36M > 1, log(PROM_VEN_SBS_TC_36M), 0), ]
  # 
  data[, PROM_VEN_SBS_6M := PROM_VEN_SBS_OP_6M + PROM_VEN_SBS_TC_6M + PROM_DEM_SBS_OP_6M + PROM_CAS_SBS_OP_6M + PROM_DEM_SBS_TC_6M + PROM_CAS_SBS_TC_6M]
  data[, PROM_VEN_SBS_12M := PROM_VEN_SBS_OP_12M + PROM_VEN_SBS_TC_12M + PROM_DEM_SBS_OP_12M + PROM_CAS_SBS_OP_12M + PROM_DEM_SBS_TC_12M + PROM_CAS_SBS_TC_12M]
  data[, PROM_VEN_SBS_24M := PROM_VEN_SBS_OP_24M + PROM_VEN_SBS_TC_24M + PROM_DEM_SBS_OP_24M + PROM_CAS_SBS_OP_24M + PROM_DEM_SBS_TC_24M + PROM_CAS_SBS_TC_24M]
  
  data[, PROM_VEN_SC_6M := PROM_VEN_SC_OP_6M + PROM_VEN_SC_TC_6M + PROM_DEM_SC_OP_6M + PROM_CAS_SC_OP_6M + PROM_DEM_SC_TC_6M + PROM_CAS_SC_TC_6M]
  data[, PROM_VEN_SC_12M := PROM_VEN_SC_OP_12M + PROM_VEN_SC_TC_12M + PROM_DEM_SC_OP_12M + PROM_CAS_SC_OP_12M + PROM_DEM_SC_TC_12M + PROM_CAS_SC_TC_12M]
  data[, PROM_VEN_SC_24M := PROM_VEN_SC_OP_24M + PROM_VEN_SC_TC_24M + PROM_DEM_SC_OP_24M + PROM_CAS_SC_OP_24M + PROM_DEM_SC_TC_24M + PROM_CAS_SC_TC_24M]
  
  data[, PROM_VEN_SICOM_6M := PROM_VEN_SICOM_OP_6M + PROM_VEN_SICOM_TC_6M + PROM_DEM_SICOM_OP_6M + PROM_CAS_SICOM_OP_6M + PROM_DEM_SICOM_TC_6M + PROM_CAS_SICOM_TC_6M]
  data[, PROM_VEN_SICOM_12M := PROM_VEN_SICOM_OP_12M + PROM_VEN_SICOM_TC_12M + PROM_DEM_SICOM_OP_12M + PROM_CAS_SICOM_OP_12M + PROM_DEM_SICOM_TC_12M + PROM_CAS_SICOM_TC_12M]
  data[, PROM_VEN_SICOM_24M := PROM_VEN_SICOM_OP_24M + PROM_VEN_SICOM_TC_24M + PROM_DEM_SICOM_OP_24M + PROM_CAS_SICOM_OP_24M + PROM_DEM_SICOM_TC_24M + PROM_CAS_SICOM_TC_24M]
  
  data[, PROM_VEN_OTROS_6M := PROM_VEN_OTROS_OP_6M + PROM_VEN_OTROS_TC_6M + PROM_DEM_OTROS_OP_6M + PROM_CAS_OTROS_OP_6M + PROM_DEM_OTROS_TC_6M + PROM_CAS_OTROS_TC_6M]
  data[, PROM_VEN_OTROS_12M := PROM_VEN_OTROS_OP_12M + PROM_VEN_OTROS_TC_12M + PROM_DEM_OTROS_OP_12M + PROM_CAS_OTROS_OP_12M + PROM_DEM_OTROS_TC_12M + PROM_CAS_OTROS_TC_12M]
  data[, PROM_VEN_OTROS_24M := PROM_VEN_OTROS_OP_24M + PROM_VEN_OTROS_TC_24M + PROM_DEM_OTROS_OP_24M + PROM_CAS_OTROS_OP_24M + PROM_DEM_OTROS_TC_24M + PROM_CAS_OTROS_TC_24M]
  
  data[, PROM_VEN_SCE_6M := PROM_VEN_SBS_6M + PROM_VEN_SC_6M + PROM_VEN_SICOM_6M + PROM_VEN_OTROS_6M]
  data[, PROM_VEN_SCE_12M := PROM_VEN_SBS_12M + PROM_VEN_SC_12M + PROM_VEN_SICOM_12M + PROM_VEN_OTROS_12M]
  data[, PROM_VEN_SCE_24M := PROM_VEN_SBS_24M + PROM_VEN_SC_24M + PROM_VEN_SICOM_24M + PROM_VEN_OTROS_24M]
  
  data[, LN_DEUDA_TOTAL_SCE_3M := ifelse(DEUDA_TOTAL_SCE_3M > 1, log(DEUDA_TOTAL_SCE_3M), 0)]
  data[, LN_DEUDA_TOTAL_SCE_6M := ifelse(DEUDA_TOTAL_SCE_6M > 1, log(DEUDA_TOTAL_SCE_6M), 0)]
  data[, LN_DEUDA_TOTAL_SCE_12M := ifelse(DEUDA_TOTAL_SCE_12M > 1, log(DEUDA_TOTAL_SCE_12M), 0)]
  data[, LN_DEUDA_TOTAL_SCE_24M := ifelse(DEUDA_TOTAL_SCE_24M > 1, log(DEUDA_TOTAL_SCE_24M), 0)]
  
  return(data)
}


