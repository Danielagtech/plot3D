calcula_productor <- function(densidad_prod, fertilizante_prod, precio_tha, costoX, costoY, b0, b1_x, b2_y, b3_xx, b4_yy, b5_x_y) {
  rendimiento_prod <- b0 + b1_x * densidad_prod + b2_y * fertilizante_prod + b3_xx * densidad_prod^2 + b4_yy * fertilizante_prod^2 + b5_x_y * densidad_prod * fertilizante_prod
  ingreso_prod <- rendimiento_prod * precio_tha
  costo_prod <- fertilizante_prod * costoY + densidad_prod * costoX
  mb_prod <- ingreso_prod - costo_prod
  data.frame(Rendimiento = rendimiento_prod, MB = mb_prod)
}