maximo_densidad <- function(xname, x_unidad_medida, b0, b1_x, b3_xx, x, precio_tha, costoX) {
  valorespred <- data.frame(Densidad = x)
  valorespred$Rendimiento <- b0 + b1_x * valorespred$Densidad + b3_xx * valorespred$Densidad^2
  valorespred$Ingreso <- valorespred$Rendimiento * precio_tha
  valorespred$Costo <- valorespred$Densidad * costoX
  valorespred$MB <- valorespred$Ingreso - valorespred$Costo
  
  print("Valores calculados:")
  print(head(valorespred))
  
  doa <- valorespred[which.max(valorespred$Rendimiento), ]
  doe <- valorespred[which.max(valorespred$MB), ]
  
  if (nrow(doa) == 0 || nrow(doe) == 0) {
    print("Error: DOA o DOE no se pudo calcular.")
    return(NULL)
  }
  
  list(resultados = list(
    DOA = list(
      Densidad = doa$Densidad,
      Rendimiento = doa$Rendimiento,
      MB = doa$MB
    ),
    DOE = list(
      Densidad = doe$Densidad,
      Rendimiento = doe$Rendimiento,
      MB = doe$MB
    )
  ), valorespred = valorespred)
}

