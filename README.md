# 📊 Aplicativo Web para Modelos de Riesgo de Crédito

**Trabajo Grupal Nº 2 - Modelización y Gestión de Riesgo**

### Integrantes:

* Alomoto Rivera Martin Sebastian
* Lara Del Salto Daniel Sebastian
* Simbaña Valencia Jennifer Pamela

### Enlace para Descarga de Datos:

- Versión ligera (5000 Registros): 	https://drive.google.com/file/d/1ees9Fi9Xgqgu8nWdRtnslPjQaqondE5-/view?usp=sharing

- Version estandar (100000 Registros):  https://drive.google.com/file/d/1QcXx5dyJHMRvjVNulwujIG_tQRwFABUH/view?usp=sharing

## 📖 Introducción
En el entorno financiero actual, la gestión cuantitativa del riesgo de crédito es un pilar fundamental para garantizar la estabilidad y solvencia de las instituciones. El advenimiento de técnicas avanzadas de *machine learning* ha transformado los enfoques tradicionales, permitiendo modelizar la probabilidad de incumplimiento con un nivel de precisión sin precedentes. Sin embargo, el verdadero valor de estos modelos radica en su operatividad en herramientas de toma de decisiones en tiempo real.

Este proyecto tiene como propósito cerrar la brecha entre la formulación teórica teórica y la implementación tecnológica, mediante el despliegue de un aplicativo interactivo en **R Shiny** que centraliza y automatiza la evaluación en lote (*batch processing*) de algoritmos de riesgo crediticio previamente entrenados.

---

## 🎯 Objetivo General
Desarrollar e implementar un aplicativo Shiny que integre los modelos predictivos de riesgo de crédito previamente entrenados, permitiendo la evaluación masiva de clientes y la estimación de métricas de pérdida esperada y provisiones a nivel de cartera.

## 📌 Objetivos Específicos
1. **Integración de Modelos:** Integrar de forma funcional y operativa modelos de Regresión Logística, Random Forest, Gradient Boosting Machine (GBM) y Ensambles en la arquitectura de un servidor Shiny.
2. **Procesamiento Masivo:** Implementar un módulo de carga de datos masiva (*batch*) mediante archivos CSV/Excel que evalúe nuevos perfiles de solicitantes de manera simultánea.
3. **Cálculo de Parámetros de Riesgo:** Aplicar fórmulas matemáticas para estimar la Probabilidad de Default (PD), la Pérdida Dado el Incumplimiento (LGD) y la Exposición al Momento del Incumplimiento (EAD) a nivel individual.
4. **Dashboard Financiero:** Consolidar los resultados individuales en un cuadro de mando financiero que calcule el fondo de provisiones requeridas y evalúe la performance del modelo mediante semaforización de deciles.

---

## 🚀 Características del Aplicativo (Features)
* **Carga Dinámica de Datos:** Soporte para archivos `.csv` y `.xlsx` con limpieza y preprocesamiento de datos en segundo plano (transformaciones logarítmicas y discretización discreta).
* **Selección de Motor Predictivo:** Interfaz gráfica para seleccionar entre modelos tradicionales (GLM) y avanzados basados en el motor de **H2O** (Random Forest, GBM, Stacked Ensembles con Deep Learning).
* **Parámetros Personalizables:** Inputs dinámicos para definir el Cutoff óptimo de aprobación y la tasa LGD esperada.
* **Métricas de Negocio:** Tablas de resultados por registro, métricas de desempeño (Performance) categorizadas por deciles de riesgo, y cálculo de la Pérdida Esperada total.
* **Gráficos Interactivos (Plotly):** Visualización reactiva que vincula un gráfico de distribución de créditos por decil con un histograma dinámico de exposición monetaria.

## 🛠️ Tecnologías Utilizadas
* **Lenguaje:** R
* **Framework Web:** Shiny
* **Machine Learning:** `h2o`, `ranger`
* **Manipulación de Datos:** `data.table`, `dplyr`, `tidyverse`
* **Visualización:** `plotly`, `DT` (DataTables)
