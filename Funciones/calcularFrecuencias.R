calcularFrecuencias <- function(merged_data) {
  tabla_contingencia <- table(merged_data$cluster_prescripciones, merged_data$cluster_cosecha)
  frecuencias_por_ambiente <- prop.table(tabla_contingencia, margin = 1) * 100
  return(frecuencias_por_ambiente)
}
