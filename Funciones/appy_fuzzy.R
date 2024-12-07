apply_fuzzy_cmeans <- function(data, column_name, centers = 3) {
  if (nrow(data) < centers) {
    return(list("labels" = rep(NA, nrow(data)), "indices" = NULL))
  }
  
  # Ejecutar Fuzzy C-Means
  cmeans_result <- e1071::cmeans(
    x = as.matrix(data[[column_name]]), 
    centers = centers, 
    method = "cmeans", 
    iter.max = 100, 
    m = 2
  )
  
  # Asignar etiquetas basadas en el máximo grado de pertenencia
  cluster_memberships <- cmeans_result$membership
  cluster_labels <- apply(cluster_memberships, 1, which.max)
  
  # Ordenar etiquetas por promedio de valores en cada cluster
  cluster_means <- tapply(data[[column_name]], cluster_labels, mean, na.rm = TRUE)
  ordered_labels <- order(cluster_means)
  label_names <- c("Baja", "Media", "Alta")
  
  final_labels <- label_names[match(cluster_labels, ordered_labels)]
  
  # Calcular índices de validación con verificación
  indices <- tryCatch({
    fclustIndex(cmeans_result, x = as.matrix(data[[column_name]]), index = "all")
  }, error = function(e) {
    print("Error al calcular índices de validación:")
    print(e)
    return(NULL)
  })
  
  return(list("labels" = final_labels, "indices" = indices))
}
