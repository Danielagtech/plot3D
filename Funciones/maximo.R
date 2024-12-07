maximo <- function(xname, yname, x_unidad_medida, y_unidad_medida, b0, b1_x, b2_y, b3_xx, b4_yy, b5_x_y, x, y, precio_tha, costoX, costoY, n_cortes) {
  valorespred <- expand.grid(Densidad = x, Fertilizante = y)
  valorespred <- transform(valorespred, Rendimiento = (
    b0 + b1_x * Densidad + b2_y * Fertilizante + b3_xx * Densidad^2 + b4_yy * Fertilizante^2 + b5_x_y * Densidad * Fertilizante))
  valorespred$Ingreso <- valorespred$Rendimiento * precio_tha
  valorespred$Costo <- valorespred$Fertilizante * costoY + valorespred$Densidad * costoX
  valorespred$MB <- valorespred$Ingreso - valorespred$Costo
  
  resultados <- list(
    "DOA" = valorespred[which.max(valorespred$Rendimiento),],
    "DOE" = valorespred[which.max(valorespred$MB),]
  )
  
  resultados_tabla <- do.call(rbind, resultados)
  
  myPlotRend <- contourplot(Rendimiento ~ Densidad * Fertilizante, data = valorespred, region = TRUE,
                            xlab = paste(xname, x_unidad_medida), ylab = paste(yname, y_unidad_medida),
                            main = "Rendimiento",
                            col.regions = viridis(n_cortes),  
                            labels = list(labels = round(seq(min(valorespred$Rendimiento), max(valorespred$Rendimiento), length.out = n_cortes), 1), cex = 1),
                            label.style = 'align', scales = list(cex = 1.2))
  
  myPlotMB <- contourplot(MB ~ Densidad * Fertilizante, data = valorespred, region = TRUE,
                          xlab = paste(xname, x_unidad_medida), ylab = paste(yname, y_unidad_medida),
                          main = "Margen Bruto USD/ha",
                          col.regions = viridis(n_cortes),  
                          labels = list(labels = round(seq(min(valorespred$MB), max(valorespred$MB), length.out = n_cortes), 1), cex = 1),
                          label.style = 'align', scales = list(cex = 1.2))
  
  list(resultados = resultados_tabla, myPlotRend = myPlotRend, myPlotMB = myPlotMB, valorespred = valorespred)
}
