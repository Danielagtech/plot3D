library(shiny)
library(shinydashboard)
library(lattice)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(viridis)
library(DT)
library(ggplot2)
library(leaflet)
library(sf)
library(readr)
library(latexpdf)
library(tinytex)
library(rmarkdown)
library(shinyWidgets)
library(e1071)


source("fclustIndex.R")
source("maximo_densidad.R")
source("maximo.R")
source("appy_fuzzy.R")
source("calcularFrecuencias.R")

# data <- read.csv("foliaresDT.csv")


lotes <- c("Guayaibi", "Pertenencia")

calcula_productor <- function(densidad_prod, fertilizante_prod, precio_tha, costoX, costoY, b0, b1_x, b2_y, b3_xx, b4_yy, b5_x_y) {
  rendimiento_prod <- b0 + b1_x * densidad_prod + b2_y * fertilizante_prod + b3_xx * densidad_prod^2 + b4_yy * fertilizante_prod^2 + b5_x_y * densidad_prod * fertilizante_prod
  ingreso_prod <- rendimiento_prod * precio_tha
  costo_prod <- fertilizante_prod * costoY + densidad_prod * costoX
  mb_prod <- ingreso_prod - costo_prod
  data.frame(Rendimiento = rendimiento_prod, MB = mb_prod)
}

# Cargar coeficientes desde un CSV
coeficientes_csv <- read_csv("coeficientes.csv")
coeficientes <- coeficientes_csv %>%
  group_by(Hibrido, Region) %>%
  summarise(
    alta = list(b0 = b0_alta, b1_x = b1_x_alta, b2_y = b2_y_alta, b3_xx = b3_xx_alta, b4_yy = b4_yy_alta, b5_x_y = b5_x_y_alta),
    media = list(b0 = b0_media, b1_x = b1_x_media, b2_y = b2_y_media, b3_xx = b3_xx_media, b4_yy = b4_yy_media, b5_x_y = b5_x_y_media),
    baja = list(b0 = b0_baja, b1_x = b1_x_baja, b2_y = b2_y_baja, b3_xx = b3_xx_baja, b4_yy = b4_yy_baja, b5_x_y = b5_x_y_baja)
  ) %>%
  ungroup() %>%
  split(.$Hibrido)


ui <- dashboardPage(
  dashboardHeader(title = "Field Analytics HyH"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Experta", tabName = "tab_red_experta", icon = icon("info-circle")),
      menuItem("NxDxA", tabName = "tab_modelos", icon = icon("chart-line")),
      menuItem("Resumen NxDxA", tabName = "tab_resumen", icon = icon("list")),
      menuItem("Densidad", tabName = "tab_densidad", icon = icon("chart-area")),
      menuItem("Comparativo", tabName = "tab_comparativo", icon = icon("exchange-alt")),
      # menuItem("Comparación Global", tabName = "tab_comparacion_global", icon = icon("globe")),
      menuItem("Ambientes", tabName = "tab_ambientes", icon = icon("map"))
      # menuItem("Foliares", tabName = "tab_datos_foliares", icon = icon("leaf")),
      # menuItem("Exportar a PDF", tabName = "tab_exportar", icon = icon("file-pdf")),
      # menuItem("Configuración", tabName = "tab_configuracion", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML('
    /* Color del logo */
    .skin-blue .main-header .logo {
      background-color: #528B8B; /* Cambia este valor al color que prefieras */
    }

    /* Color del logo cuando se pasa el mouse */
    .skin-blue .main-header .logo:hover {
      background-color: #528B8B;
    }

    /* Color del encabezado (barra de navegación) */
    .skin-blue .main-header .navbar {
      background-color: #528B8B;
    }

    /* Estilos para compactar el panel */
    .compact-panel {
      padding: 3px !important;
      margin: 3px !important;
    }
    .shiny-input-container {
      margin-bottom: 3px !important;
    }
  '))
    ),
  tabItems(
    # Tab Red Experta
    tabItem(tabName = "tab_red_experta",
            fluidPage(
              titlePanel("Red Experta"),
              h3("Resultados de la Red tester"),
              p("Esta aplicación Shiny ha sido desarrollada para visualizar las curvas de respuesta y la eficiencia de diversos híbridos en diferentes regiones y ambientes. Seleccione una región para comenzar a explorar los datos."),
              p("En esta pestaña puede seleccionar la región que desea visualizar. Esto afectará los datos presentados en las pestañas 'Modelos' y 'Resumen'.")
            )
    ),
    # tabItem(tabName = "tab_exportar",
    #         fluidPage(
    #           titlePanel("Exportar Información a PDF"),
    #           selectInput("exportar_opcion", "Seleccione el contenido a exportar:",
    #                       choices = c("Modelos", "Resumen", "Comparación Global")),
    #           actionButton("exportar_pdf", "Exportar a PDF", icon = icon("file-pdf"))
    #         )
    # ),
    # Tab Modelos
    tabItem(tabName = "tab_modelos",
            fluidPage(
              titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Curvas de Respuesta probable")),
              
              # Filtro de selección de región
              fluidRow(
                column(12,
                       div(style = "padding: 10px; background-color: #f0f0f0; border-bottom: 2px solid #006064;",
                           selectInput("region", "Seleccione la Región:", 
                                       choices = c("Todas", unique(coeficientes_csv$Region)),
                                       selected = "Todas")
                       )
                )
              ),
              fluidRow(
                column(2,
                       div(style = "border-right: 1px solid #006064; height: 100%; padding: 10px;",
                           uiOutput("hibrido_selector"),
                           radioButtons("ambiente", "Seleccione el potencial de rendimiento:", 
                                        choices = c("Alta" = "alta", "Media"= "media", "Baja" = "baja")),
                           sliderInput("precio_tha", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                           sliderInput("costoX", "Costo *1000semillas (USD):", min = 1.5, max = 2.7, value = 2.29),
                           sliderInput("costoY", "Costo kg Ferti (USD):", min = 0.4, max = 1.5, value = 0.8)
                       )
                ),
                column(6,
                       fluidRow(
                         column(6, plotOutput("plotRend", height = "300px")),
                         column(6, plotOutput("plotMB", height = "300px"))
                       ),
                       fluidRow(
                         plotlyOutput("plot3D", height = "300px")
                       )
                ),
                column(3,
                       div(style = "border-left: 1px solid #006064; height: 100%; padding: 10px;",
                           h3(style = "border-bottom: 1px solid #006064; padding-bottom: 5px;", "Resultados del ensayo"),
                           div(style = "border: 1px solid #26897E; background-color: #26897E; color: white; padding: 20px; border-radius: 8px; margin-bottom: 10px;",
                               HTML("<center><b>Dosis optima agronómica</b></center>"),
                               htmlOutput("doa_text")),
                           div(style = "border: 1px solid #BD0A36; background-color: #BD0A36; color: white; padding: 20px; border-radius: 8px; margin-bottom: 10px;",
                               HTML("<center><b>Dosis optima económica</b></center>"),
                               htmlOutput("doe_text")),
                           h3("Testigo de uso actual (TUA)"),
                           numericInput("densidad_prod", "Densidad (1000 semillas/ha):", value = 89, min = 30, max = 100),
                           numericInput("fertilizante_prod", "Fertilizante (kg/ha):", value = 348, min = 0, max = 500),
                           div(style = "border: 1px solid #26897E; background-color: #26897E; color: white; padding: 20px; border-radius: 8px; margin-top: 20px;",
                               HTML("<center><b>Diferencia TUA/DOE</b></center>"),
                               htmlOutput("diff_text"))
                       )
                )
              )
            )
    ),tabItem(tabName = "tab_densidad",
              fluidPage(
                titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Curvas de respuesta probable")),
                
                # Primera fila: Selectores y gráfico
                fluidRow(
                  column(3,
                         div(style = "border-right: 1px solid #006064; height: 100%; padding: 10px;",
                             uiOutput("hibrido_selector_densidad"),
                             sliderInput("precio_tha_densidad", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                             sliderInput("costoX_densidad", "Costo *1000 semillas (USD):", min = 1.5, max = 2.7, value = 2.29)
                         )
                  ),
                  column(9,
                         div(style = "border-left: 1px solid #006064; height: 100%; padding: 10px;",
                             plotlyOutput("plotDensidadAmbientes", height = "500px")
                         )
                  )
                ),
                
                # Segunda fila: Tabla de resultados
                fluidRow(
                  column(9,
                         div(style = "margin-top: 20px; max-width: 800px; display: flex; justify-content: center;",
                             DTOutput("tabla_resumen_densidad")
                         )
                  )
                )
              )
    ),
    tabItem(
      tabName = "tab_comparativo",
      fluidPage(
        titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Comparativo de Curvas de Densidad")),
        
        # Filtros y opciones
        fluidRow(
          column(3,
                 div(style = "padding: 10px; border-right: 1px solid #006064;",
                     uiOutput("hibrido_1"),        # Selector dinámico para Híbrido 1
                     uiOutput("hibrido_2"),        # Selector dinámico para Híbrido 2
                     selectInput("ambiente_comparativo", "Seleccione el Ambiente:", 
                                 choices = c("Alta", "Media", "Baja")),
                     sliderInput("precio_tha_comparativo", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                     sliderInput("costoX_comparativo", "Costo *1000 semillas (USD):", min = 1.5, max = 2.7, value = 2.29)
                 )
          ),
          column(9,
                 div(style = "padding: 10px; border-left: 1px solid #006064;",
                     plotlyOutput("plotComparativoDensidad", height = "500px")
                 )
          )
        )
      )
    ),
    # Tab Resumen
    tabItem(tabName = "tab_resumen",
            fluidPage(
              titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Resultados de híbridos por ambientes")),
              
              fluidRow(
                column(2,
                       div(style = "border-right: 1px solid #006064; height: 100%; padding: 10px;",
                           sliderInput("precio_tha_resumen", "Precio Tn (USD):", min = 100, max = 300, value = 180),
                           sliderInput("costoX_resumen", "Costo *1000semillas (USD):", min = 1.5, max = 2.7, value = 2.29),
                           sliderInput("costoY_resumen", "Costo kg Ferti (USD):", min = 0.4, max = 1.5, value = 0.8)
                       )
                ),
                column(10,
                       div(style = "width: 1000px; margin: 0 auto;",
                           DTOutput("tabla_resumen")
                       ),
                       div(style = "width: 1000px; margin: 0 auto;",
                           plotOutput("plot_euf", height = "600px")
                       )
                )
              )
            )
    ), 
    # Tab Comparación Global
    # tabItem(tabName = "tab_comparacion_global",
    #         fluidPage(
    #           titlePanel(div(style = "background-color: #006064; color: white; padding: 15px; border-radius: 8px;", "Comparación Global de Híbridos")),
    #           fluidRow(
    #             column(12,
    #                    plotOutput("comparacion_global", height = "600px")
    #             )
    #           )
    #         )
    # ),
    # tabItem(tabName = "tab_datos_foliares",
    #         fluidPage(
    #           titlePanel("Visualización de Datos Foliares"),
    #           sidebarLayout(
    #             sidebarPanel(
    #               
    #               checkboxInput("todas_regiones", "Seleccionar Todas las Regiones", value = FALSE),
    #               pickerInput("regiones", "Seleccionar Región:", choices = c("Todas", unique(data$Regiones)), selected = "Huinca", multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE)),
    #               pickerInput("cultivo", "Seleccionar Cultivo:", choices = unique(data$Cultivo), selected = unique(data$Cultivo), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))
    #             ),
    #             mainPanel(
    #               plotOutput("barPlot"),
    #               dataTableOutput("summaryTable")
    #             )
    #           )
    #         )
    # ),
    tabItem(tabName = "tab_ambientes",
            fluidPage(
              tags$head(
                tags$style(HTML("
        .map-title { font-size: 15px; } 
        .first-row { margin-bottom: 25px; } 
        #grafico_contingencia { width: 600px; } 
        .banner { background-color: #006064; color: white; padding: 10px; text-align: center; font-size: 24px; font-weight: bold; } 
        .input-panel { border-right: 1px solid #006064; padding: 2px; width: 10%; } 
        .compact-slider .shiny-input-container { margin-bottom: 4px; }
        .compact-slider .shiny-input-slider { height: 10px; } 
        .compact-slider .control-label { font-size: 12px; } 
        .compact-slider input { font-size: 12px; padding: 2px; }
      "))
              ),
      div(class = "banner", "Evaluación de Ambientes"),
      titlePanel("Análisis geoespacial de lotes"),
      sidebarLayout(
        sidebarPanel(
          div(class = "compact-panel", # Añadimos la clase compact-slider para aplicar el estilo compacto
              style = "background-color: #f8f9fa; border: 1px solid #ddd; border-radius: 5px;",
              selectInput("lote", "Seleccione el lote:", choices = lotes),
              sliderInput("precio_maiz", "Precio Maíz (USD/tn):", value = 200, min = 100, max = 500, step = 10),
              sliderInput("costo_semilla", "Semilla (*1000 semillas):", value = 2.6, min = 0, max = 5, step = 0.1),
              sliderInput("costo_ferti", "Fertilizante (USD/Kg):", value = 0.8, min = 0, max = 3, step = 0.5),
              numericInput("costo_arrancador", "Arrancador (USD/ha):", value = 70, min = 0, max = 200, step = 0.5),
              numericInput("costo_labores", "Labores (USD/ha):", value = 180, min = 0, max = 200, step = 0.5),
              numericInput("costo_fito", "Fitosanitarios (USD/ha):", value = 160, min = 0, max = 200, step = 0.5),
              numericInput("costo_arrendamiento", "Costo Arrendamiento (USD/ha):", value = 400, min = 0, max = 1000, step = 10),
              plotlyOutput("indice_ajuste")
          )
        ),
        mainPanel(
          fluidRow(class = "first-row",
                   column(3,
                          div(
                            tags$h2("Ambientes Prescripción", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_prescripciones"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   column(3,
                          div(
                            tags$h2("Ambientes Cosecha", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_cosecha"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   
                   column(3,
                          div(
                            tags$h2("Comparación", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_comparacion"),
                            style = "width: 100%; height: 400px;"
                          )
                   ),
                   column(3,
                          div(
                            tags$h2("Mapa de Margen Neto", class = "map-title", style = "font-weight: bold;"),
                            leafletOutput("mapa_margen_neto"),
                            style = "width: 100%; height: 400px;"
                          )
                   )
          ),
          fluidRow(
            column(6,
                   plotOutput("p_medias")
            ),
            column(6,
                   plotOutput("grafico_contingencia")
            )
          )
        )
      )
            )
    )
  )
  )
)


server <- function(input, output, session) {
  # Reactivo para filtrar los coeficientes por región
  coef_filtrados <- reactive({
    if (input$region == "Todas") {
      return(coeficientes_csv)
    } else {
      return(coeficientes_csv %>% filter(Region == input$region))
    }
  })
  
  
  filtrar_coeficientes <- function(hibrido, ambiente, modelo) {
    data <- coef_filtrados()
    
    # Seleccionar las columnas correctas para el modelo y ambiente
    columnas <- c(
      b0 = paste0("b0_", ambiente),
      b1_x = paste0("b1_x_", ambiente),
      b2_y = if (modelo == "NxDxA") paste0("b2_y_", ambiente) else NULL,
      b3_xx = paste0("b3_xx_", ambiente),
      b4_yy = if (modelo == "NxDxA") paste0("b4_yy_", ambiente) else NULL,
      b5_x_y = if (modelo == "NxDxA") paste0("b5_x_y_", ambiente) else NULL
    )
    
    # Filtrar y extraer los coeficientes
    coefs <- data %>%
      filter(Hibrido == hibrido) %>%
      select(all_of(columnas)) %>%
      unlist(use.names = FALSE)
    
    # Validar que los coeficientes sean completos
    if (length(coefs) != length(columnas) || any(is.na(coefs))) {
      warning("Coeficientes incompletos o no encontrados para el híbrido, ambiente y modelo seleccionados.")
      return(NULL)
    }
    
    return(coefs)
  }
  
  
  
  # Actualizar los híbridos disponibles según la región seleccionada
  output$hibrido_selector <- renderUI({
    # Filtrar los híbridos que tienen el modelo "NxDxA"
    hibridos_validos <- coef_filtrados() %>%
      filter(Modelo == "NxDxA") %>%  # Filtrar por modelo NxDxA
      pull(Hibrido)  # Extraer nombres de híbridos
    
    # Si no hay híbridos válidos, mostramos un mensaje informativo
    if (length(hibridos_validos) == 0) {
      hibridos_validos <- "No hay híbridos disponibles para este modelo"
    }
    
    # Renderizamos el selector con los híbridos válidos
    selectInput("hibrido", "Seleccione el híbrido:", choices = hibridos_validos)
  })
  
  
  # Modificar las funciones que dependen de los coeficientes para usar `coef_filtrados()`
  valores_predichos <- reactive({
    req(input$hibrido, input$ambiente)
    
    coefs <- filtrar_coeficientes(hibrido = input$hibrido, ambiente = input$ambiente, modelo = "NxDxA")
    
    if (length(coefs) != 6 || any(is.na(coefs))) return(NULL)
    
    maximo(
      xname = "Densidad",
      yname = "Fertilizante",
      x_unidad_medida = "( * 1000 semillas / ha)",
      y_unidad_medida = "(kg / ha)",
      b0 = coefs[1],
      b1_x = coefs[2],
      b2_y = coefs[3],
      b3_xx = coefs[4],
      b4_yy = coefs[5],
      b5_x_y = coefs[6],
      x = seq(45, 95, 1),
      y = seq(0, 300, 1),
      precio_tha = input$precio_tha,
      costoX = input$costoX,
      costoY = input$costoY,
      n_cortes = 5
    )
  })
  
  
  
  output$plotRend <- renderPlot({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    ggplot(pred$valorespred, aes(x = Densidad, y = Fertilizante, z = Rendimiento)) +
      geom_contour_filled() +
      labs(title = "Rendimiento",
           x = "Densidad ( * 1000 semillas / ha)",
           y = "Fertilizante (kg / ha)",
           fill = "Rendimiento") +
      theme_minimal()
  })
  
  output$plotMB <- renderPlot({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    ggplot(pred$valorespred, aes(x = Densidad, y = Fertilizante, z = MB)) +
      geom_contour_filled() +
      labs(title = "Margen Bruto USD/ha",
           x = "Densidad ( * 1000 semillas / ha)",
           y = "Fertilizante (kg / ha)",
           fill = "Margen Bruto") +
      theme_minimal()
  })
  
  output$plot3D <- renderPlotly({
    pred <- valores_predichos()
    if (is.null(pred)) {
      plot_ly() %>%
        layout(title = "Sin datos para este ambiente")
      return()
    }
    
    coefs <- coef_filtrados() %>% filter(Hibrido == input$hibrido) %>%
      select(b0 = paste0("b0_", input$ambiente),
             b1_x = paste0("b1_x_", input$ambiente),
             b2_y = paste0("b2_y_", input$ambiente),
             b3_xx = paste0("b3_xx_", input$ambiente),
             b4_yy = paste0("b4_yy_", input$ambiente),
             b5_x_y = paste0("b5_x_y_", input$ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (length(coefs) != 6 || any(is.na(coefs))) {
      plot_ly() %>%
        layout(title = "Sin datos para este ambiente")
      return()
    }
    
    densidad <- seq(45, 95, length.out = 300)
    fertilizante <- seq(0, 300, length.out = 300)
    
    D <- outer(densidad, fertilizante, FUN = function(d, f) d)
    F <- outer(densidad, fertilizante, FUN = function(d, f) f)
    
    Rendimiento <- with(list(b0 = coefs[1], b1_x = coefs[2], b2_y = coefs[3], b3_xx = coefs[4], b4_yy = coefs[5], b5_x_y = coefs[6]),
                        b0 + b1_x * D + b2_y * F + b3_xx * D^2 + b4_yy * F^2 + b5_x_y * D * F)
    
    plot_ly() %>%
      add_surface(x = ~densidad, y = ~fertilizante, z = ~Rendimiento, colorscale = 'Viridis') %>%
      layout(scene = list(xaxis = list(title = 'Densidad'),
                          yaxis = list(title = 'Ferti (kg/ha)'),
                          zaxis = list(title = 'Rinde (tn/ha)')))
  })
  
  output$doa_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    doa <- pred$resultados["DOA", ]
    HTML(paste0(
      "Rinde = <b>", round(doa$Rendimiento, 1), " tn/ha</b><br>",
      "Fertilizante = <b>", round(doa$Fertilizante, 0), " N/ha</b><br>",
      "Densidad = <b>", round(doa$Densidad, 0), " semillas/ha</b>"
    ))
  })
  
  output$doe_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    doe <- pred$resultados["DOE", ]
    HTML(paste0(
      "Rinde = <b>", round(doe$Rendimiento, 1), " tn/ha</b><br>",
      "Fertilizante = <b>", round(doe$Fertilizante, 0), " N/ha</b><br>",
      "Densidad = <b>", round(doe$Densidad, 0), " semillas/ha</b>"
    ))
  })
  
  output$diff_text <- renderUI({
    pred <- valores_predichos()
    if (is.null(pred)) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    
    valores_maximos <- pred$resultados
    coefs <- coef_filtrados() %>% filter(Hibrido == input$hibrido) %>%
      select(b0 = paste0("b0_", input$ambiente),
             b1_x = paste0("b1_x_", input$ambiente),
             b2_y = paste0("b2_y_", input$ambiente),
             b3_xx = paste0("b3_xx_", input$ambiente),
             b4_yy = paste0("b4_yy_", input$ambiente),
             b5_x_y = paste0("b5_x_y_", input$ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (length(coefs) != 6 || any(is.na(coefs))) {
      HTML("<b>Sin datos para este ambiente</b>")
      return()
    }
    
    prod_values <- calcula_productor(
      densidad_prod = input$densidad_prod,
      fertilizante_prod = input$fertilizante_prod,
      precio_tha = input$precio_tha,
      costoX = input$costoX,
      costoY = input$costoY,
      b0 = coefs[1],
      b1_x = coefs[2],
      b2_y = coefs[3],
      b3_xx = coefs[4],
      b4_yy = coefs[5],
      b5_x_y = coefs[6]
    )
    
    diferencia_rendimiento <- prod_values$Rendimiento - valores_maximos["DOE", "Rendimiento"]
    diferencia_mb <- prod_values$MB - valores_maximos["DOE", "MB"]
    
    HTML(paste0(
      "TUA = <b>", round(prod_values$Rendimiento, 1), " tn/ha</b><br>",
      "Dif_Rinde = <b>", round(diferencia_rendimiento, 2), " (tn/ha)</b><br>",
      "Dif_MB = <b>", round(diferencia_mb, 2), "</b>"
    ))
  })
  
  # Filtro por región en valores_resumen
  valores_resumen <- reactive({
    coefs_filtrados <- coef_filtrados()
    hibridos <- unique(coefs_filtrados$Hibrido)
    ambientes <- c("alta", "media", "baja")
    
    resultados <- data.frame(
      Field = numeric(),
      Fertilizante = character(),
      Hibrido = character(),
      Region = character(),
      Ambiente = character(),
      Rendimiento_DOA = numeric(),
      Ferti_DOA = numeric(),
      Densidad_DOA = numeric(),
      Rendimiento_DOE = numeric(),
      Ferti_DOE = numeric(),
      Densidad_DOE = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (hibrido in hibridos) {
      for (ambiente in ambientes) {
        # Filtrar coeficientes y seleccionar Field y Fertilizante
        coefs <- coefs_filtrados %>% filter(Hibrido == hibrido)
        
        if (nrow(coefs) == 0) {
          next
        }
        
        region <- unique(coefs$Region)
        field <- unique(coefs$Field)             # Extrae Field
        fertilizante <- unique(coefs$Fertilizante) # Extrae Fertilizante
        
        coefs_ambiente <- coefs %>%
          select(b0 = paste0("b0_", ambiente),
                 b1_x = paste0("b1_x_", ambiente),
                 b2_y = paste0("b2_y_", ambiente),
                 b3_xx = paste0("b3_xx_", ambiente),
                 b4_yy = paste0("b4_yy_", ambiente),
                 b5_x_y = paste0("b5_x_y_", ambiente)) %>%
          unlist(use.names = FALSE)
        
        if (length(coefs_ambiente) != 6 || any(is.na(coefs_ambiente))) {
          next
        }
        
        pred <- maximo(
          xname = "Densidad",
          yname = "Fertilizante",
          x_unidad_medida = "( * 1000 semillas / ha)",
          y_unidad_medida = "(kg / ha)",
          b0 = coefs_ambiente[1],
          b1_x = coefs_ambiente[2],
          b2_y = coefs_ambiente[3],
          b3_xx = coefs_ambiente[4],
          b4_yy = coefs_ambiente[5],
          b5_x_y = coefs_ambiente[6],
          x = seq(45, 95, 1),
          y = seq(0, 300, 1),
          precio_tha = input$precio_tha_resumen,
          costoX = input$costoX_resumen,
          costoY = input$costoY_resumen,
          n_cortes = 5
        )
        if (is.null(pred) || any(is.na(pred$resultados))) {
          next
        }
        doa <- pred$resultados["DOA", ]
        doe <- pred$resultados["DOE", ]
        
        resultados <- rbind(resultados, data.frame(
          Field = field,                   # Agrega Field aquí
          Fertilizante = fertilizante,     # Agrega Fertilizante aquí
          Hibrido = hibrido,
          Region = region,
          Ambiente = ambiente,
          Rendimiento_DOA = doa$Rendimiento,
          Ferti_DOA = doa$Fertilizante,
          Densidad_DOA = doa$Densidad,
          Rendimiento_DOE = doe$Rendimiento,
          Ferti_DOE = doe$Fertilizante,
          Densidad_DOE = doe$Densidad
        ))
      }
    }
    
    resultados
  })
  
  output$tabla_resumen <- renderDT({
    datatable(valores_resumen(), selection = 'single', options = list(
      dom = 't',
      paging = FALSE,
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ), rownames = FALSE)
  })
  
  output$plot_euf <- renderPlot({
    req(input$tabla_resumen_rows_selected)
    seleccion <- input$tabla_resumen_rows_selected
    hibrido <- as.character(valores_resumen()[seleccion, "Hibrido"])
    ambiente <- as.character(valores_resumen()[seleccion, "Ambiente"])
    
    coefs <- coef_filtrados() %>% filter(Hibrido == hibrido) %>%
      select(b0 = paste0("b0_", ambiente),
             b1_x = paste0("b1_x_", ambiente),
             b2_y = paste0("b2_y_", ambiente),
             b3_xx = paste0("b3_xx_", ambiente),
             b4_yy = paste0("b4_yy_", ambiente),
             b5_x_y = paste0("b5_x_y_", ambiente)) %>%
      unlist(use.names = FALSE)
    
    if (is.null(coefs) || length(coefs) != 6 || any(is.na(coefs))) {
      plot.new()
      title("Sin datos para este ambiente")
      return()
    }
    
    pred <- maximo(
      xname = "Densidad",
      yname = "Fertilizante",
      x_unidad_medida = "( * 1000 semillas / ha)",
      y_unidad_medida = "(kg / ha)",
      b0 = coefs[1],
      b1_x = coefs[2],
      b2_y = coefs[3],
      b3_xx = coefs[4],
      b4_yy = coefs[5],
      b5_x_y = coefs[6],
      x = seq(45, 90, 5),  
      y = seq(0, 300, 100),  
      precio_tha = input$precio_tha_resumen,
      costoX = input$costoX_resumen,
      costoY = input$costoY_resumen,
      n_cortes = 15
    )
    
    valorespred <- pred$valorespred
    
    euf_data <- expand.grid(Fertilizante = seq(100, 300, 100), Densidad = seq(45, 90, 10))
    euf_data$EUF <- NA
    
    for (i in 1:nrow(euf_data)) {
      densidad <- euf_data$Densidad[i]
      ferti <- euf_data$Fertilizante[i]
      rto_f <- with(valorespred, Rendimiento[Densidad == densidad & Fertilizante == ferti])
      rto_0 <- with(valorespred, Rendimiento[Densidad == densidad & Fertilizante == 0])
      if (length(rto_f) > 0 & length(rto_0) > 0) {
        euf_data$EUF[i] <- (rto_f*1000 - rto_0*1000) / ferti
      }
    }
    
    ggplot(euf_data, aes(x = Fertilizante, y = Densidad, fill = EUF, label = round(EUF, 2))) +
      geom_tile() +
      geom_text(color = "white", size = 10) +
      scale_fill_viridis_c() +
      labs(title = paste("Eficiencia de Uso de Fertilizante para", hibrido, "en ambiente", ambiente),
           x = "Dosis de Fertilizante (kg/ha)",
           y = "Densidad (1000 semillas/ha)",
           fill = "EUF") +
      theme_bw() +
      theme(
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(size = 16),     
        axis.title.y = element_text(size = 16),    
        axis.text.x = element_text(size = 16),       
        axis.text.y = element_text(size = 16)       
      )
    
  })
  
  
  output$comparacion_global <- renderPlot({
    resumen <- valores_resumen()
    
    resumen <- resumen %>%
      mutate(Ferti_DOA_group = cut(Ferti_DOA, 
                                   breaks = c(-Inf, 100, 200, 300, 400, Inf), 
                                   labels = c("0-100", "101-200", "201-300", "301-400", "401+")))
    
    ggplot(resumen, aes(x = Densidad_DOA, y = Rendimiento_DOA, label = Hibrido)) +
      geom_point(aes(color = Ambiente, shape = Ferti_DOA_group), size = 4) +
      geom_text(vjust = -1, hjust = 0.5, size = 3) +
      scale_color_manual(values = c("alta" = "green", "media" = "yellow", "baja" = "red")) +
      scale_shape_manual(values = c("0-100" = 16, "101-200" = 17, "201-300" = 18, "301-400" = 19, "401+" = 15)) +
      geom_hline(yintercept = c(5, 10), linetype = "dotted", color = "gray") +
      scale_x_continuous(breaks = seq(65, 100, 5), labels = seq(65, 100, 5)) +
      scale_y_continuous(breaks = seq(0, 15, 1), labels = seq(0, 15, 1)) +
      labs(title = "Comparación Global de Híbridos",
           x = "Densidad (1000 semillas/ha)",
           y = "Rendimiento (tn/ha)",
           color = "Ambiente",
           shape = "Dosis Fertilizante (kg/ha)") +
      theme_minimal() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            panel.grid.major = element_line(size = 0.8),
            panel.grid.minor = element_line(size = 0.5),
            plot.margin = unit(c(1,1,1,1), "cm"))
  })
  
  #-----------------------------------------------------------------------------
  #Densidad solamente
  
  # Reactivo exclusivo para filtrar coeficientes de Densidad
  coef_densidad <- reactive({
    coeficientes_csv %>% 
      filter(Modelo == "DxA")  # Solo coeficientes para DxA
  })
  
  
  output$hibrido_selector_densidad <- renderUI({
    hibridos_validos <- coef_densidad() %>%
      pull(Hibrido)  # Extraemos los híbridos válidos para el modelo DxA
    
    if (length(hibridos_validos) == 0) {
      hibridos_validos <- "No hay híbridos disponibles para este modelo"
    }
    
    selectInput("hibrido_densidad", "Seleccione el híbrido:", choices = hibridos_validos)
  })
  
  
  calcular_valores_densidad <- function(coefs, densidades, precio_tha, costoX) {
    b0 <- coefs[1]
    b1 <- coefs[2]
    b2 <- coefs[3]
    
    rendimiento <- b0 + b1 * densidades + b2 * densidades^2
    ingreso <- rendimiento * precio_tha
    costo <- densidades * costoX
    mb <- ingreso - costo
    
    data.frame(Densidad = densidades, Rendimiento = rendimiento, MB = mb)
  }
  
  # Reactivo para obtener valores predichos para todos los ambientes (solo Densidad)
  valores_predichos_densidad <- reactive({
    req(input$hibrido_densidad)  # Usar input específico para hibrido de densidad
    
    ambientes <- c("alta", "media", "baja")
    densidades <- seq(45, 80, by = 1)
    resultados <- list()
    
    for (ambiente in ambientes) {
      # Usar coef_densidad en lugar de coef_filtrados para asegurarnos de que esté aislado
      coefs <- coef_densidad() %>%
        filter(Hibrido == input$hibrido_densidad) %>%
        select(b0 = paste0("b0_", ambiente),
               b1_x = paste0("b1_x_", ambiente),
               b3_xx = paste0("b3_xx_", ambiente)) %>%
        unlist(use.names = FALSE)
      
      if (length(coefs) == 3) {
        valores <- calcular_valores_densidad(coefs, densidades, input$precio_tha_densidad, input$costoX_densidad)
        valores$Ambiente <- ambiente
        resultados[[ambiente]] <- valores
      }
    }
    
    do.call(rbind, resultados)
  })
  
  
  # Renderizar el gráfico con curvas para cada ambiente
  
  
  output$plotDensidadAmbientes <- renderPlotly({
    pred <- valores_predichos_densidad()
    if (is.null(pred) || nrow(pred) == 0) {
      return(plotly_empty())
    }
    
    # Calcular DOA y DOE para cada ambiente
    doa_points <- pred %>% group_by(Ambiente) %>% slice_max(Rendimiento)
    doe_points <- pred %>% group_by(Ambiente) %>% slice_max(MB)
    
    plot_ly(
      data = pred,
      x = ~Densidad,
      y = ~Rendimiento,
      color = ~Ambiente,
      colors = c("green", "yellow", "red"),
      type = 'scatter',
      mode = 'lines',  
      hoverinfo = 'text',
      text = ~paste("Ambiente: ", Ambiente, "<br>Densidad: ", round(Densidad, 1), 
                    "<br>Rendimiento: ", round(Rendimiento, 2))
    ) %>%
      # Agregar puntos DOA
      add_trace(
        data = doa_points,
        x = ~Densidad,
        y = ~Rendimiento,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10, color = 'black', symbol = 'diamond'),
        name = 'DOA (Dosis Óptima Agronómica)',
        hoverinfo = 'text',
        text = ~paste("Ambiente: ", Ambiente, "<br>Densidad DOA: ", round(Densidad, 1), 
                      "<br>Rendimiento: ", round(Rendimiento, 2)),
        showlegend = FALSE
      ) %>%
      # Agregar puntos DOE
      add_trace(
        data = doe_points,
        x = ~Densidad,
        y = ~Rendimiento,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10, color = 'blue', symbol = 'circle'),
        name = 'DOE (Dosis Óptima Económica)',
        hoverinfo = 'text',
        text = ~paste("Ambiente: ", Ambiente, "<br>Densidad DOE: ", round(Densidad, 1), 
                      "<br>Margen Bruto: ", round(MB, 2)),
        showlegend = FALSE
      ) %>%
      layout(
        title = "Modelo de regresión",
        xaxis = list(title = "Densidad (miles/ha)", titlefont = list(size = 14), tickfont = list(size = 14)),
        yaxis = list(title = "Rendimiento (tn/ha)", titlefont = list(size = 14), tickfont = list(size = 14)),
        legend = list(title = list(text = "Ambiente"), font = list(size = 14))
      )
  })
  
  
  
  
  output$tabla_resumen_densidad <- renderDT({
    pred <- valores_predichos_densidad()
    if (is.null(pred) || nrow(pred) == 0) {
      return(data.frame(Resultado = "Sin datos disponibles"))
    }
    
    resumen <- pred %>%
      group_by(Ambiente) %>%
      summarise(
        `Rinde (DOA)` = round(max(Rendimiento), 2),
        `Densidad (DOA)` = round(Densidad[which.max(Rendimiento)], 2),
        `Rinde (DOE)` = round(Rendimiento[which.max(MB)], 2),
        `Densidad (DOE)` = round(Densidad[which.max(MB)], 2),
        `Margen Bruto Máximo (USD)` = round(max(MB), 2)
      )
    
    datatable(resumen,
              options = list(
                dom = 't',  # Ocultar opciones de búsqueda, paginación, etc.
                pageLength = 5,
                autoWidth = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")  # Centrar columnas
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover') %>%
      formatStyle(
        columns = colnames(resumen),
        backgroundColor = 'lightgrey',  # Fondo gris claro
        fontSize = '15px',             # Fuente más grande
        textAlign = 'center'           # Centrado del texto
      ) %>%
      formatStyle(
        columns = colnames(resumen), 
        fontWeight = 'bold',           # Negrita para encabezados
        color = 'black'                # Texto negro para encabezado
      ) %>%
      formatStyle(
        columns = colnames(resumen), 
        background = styleColorBar(resumen$`Margen Bruto Máximo (USD)`, 'lightblue') # Fondo gradual en columnas numéricas
      )
  })
  
  #------------------------------------------------------------------------------
  #Hibridos comparativo
  # Reactivo para filtrar los coeficientes del modelo DxA
  coef_comparativo <- reactive({
    coeficientes_csv %>%
      filter(Modelo == "DxA")  # Solo filtrar por el modelo DxA
  })
  
  # Renderizar el selector para Híbrido 1
  output$hibrido_1 <- renderUI({
    hibridos_validos <- coef_comparativo() %>%
      pull(Hibrido) %>%
      unique()
    
    if (length(hibridos_validos) == 0) {
      hibridos_validos <- "No hay híbridos disponibles para este modelo"
    }
    
    selectInput("hibrido_1", "Seleccione el Híbrido 1:", choices = hibridos_validos)
  })
  
  # Renderizar el selector para Híbrido 2
  output$hibrido_2 <- renderUI({
    hibridos_validos <- coef_comparativo() %>%
      pull(Hibrido) %>%
      unique()
    
    if (length(hibridos_validos) == 0) {
      hibridos_validos <- "No hay híbridos disponibles para este modelo"
    }
    
    selectInput("hibrido_2", "Seleccione el Híbrido 2:", choices = hibridos_validos)
  })
  
  # Obtener los valores para la comparación
  valores_comparativo <- reactive({
    req(input$hibrido_1, input$hibrido_2, input$ambiente_comparativo)
    
    densidades <- seq(45, 80, by = 1)
    resultados <- list()
    
    # Procesar el Híbrido 1
    coefs_1 <- coef_comparativo() %>%
      filter(Hibrido == input$hibrido_1) %>%
      select(Field, Region, 
             b0 = paste0("b0_", tolower(input$ambiente_comparativo)),
             b1_x = paste0("b1_x_", tolower(input$ambiente_comparativo)),
             b3_xx = paste0("b3_xx_", tolower(input$ambiente_comparativo)))
    
    if (nrow(coefs_1) > 0) {
      valores_1 <- calcular_valores_densidad(
        coefs = unlist(coefs_1[1, 3:5], use.names = FALSE),
        densidades = densidades,
        precio_tha = input$precio_tha_comparativo,
        costoX = input$costoX_comparativo
      )
      valores_1$Hibrido <- paste0(input$hibrido_1, " (Híbrido 1)")
      valores_1$Field <- coefs_1$Field[1]
      valores_1$Region <- coefs_1$Region[1]
      resultados[[1]] <- valores_1
    }
    
    # Procesar el Híbrido 2
    coefs_2 <- coef_comparativo() %>%
      filter(Hibrido == input$hibrido_2) %>%
      select(Field, Region,
             b0 = paste0("b0_", tolower(input$ambiente_comparativo)),
             b1_x = paste0("b1_x_", tolower(input$ambiente_comparativo)),
             b3_xx = paste0("b3_xx_", tolower(input$ambiente_comparativo)))
    
    if (nrow(coefs_2) > 0) {
      valores_2 <- calcular_valores_densidad(
        coefs = unlist(coefs_2[1, 3:5], use.names = FALSE),
        densidades = densidades,
        precio_tha = input$precio_tha_comparativo,
        costoX = input$costoX_comparativo
      )
      valores_2$Hibrido <- paste0(input$hibrido_2, " (Híbrido 2)")
      valores_2$Field <- coefs_2$Field[1]
      valores_2$Region <- coefs_2$Region[1]
      resultados[[2]] <- valores_2
    }
    
    do.call(rbind, resultados)
  })
  
  # Gráfico comparativo
  output$plotComparativoDensidad <- renderPlotly({
    pred <- valores_comparativo()
    if (is.null(pred) || nrow(pred) == 0) {
      return(plotly_empty())
    }
    
    # Calcular DOA y DOE para cada híbrido
    doa_points <- pred %>% group_by(Hibrido) %>% slice_max(Rendimiento)
    doe_points <- pred %>% group_by(Hibrido) %>% slice_max(MB)
    
    plot_ly(
      data = pred,
      x = ~Densidad,
      y = ~Rendimiento,
      color = ~Hibrido,
      type = 'scatter',
      mode = 'lines',
      hoverinfo = 'text',
      text = ~paste("Híbrido: ", Hibrido,
                    "<br>Densidad: ", round(Densidad, 1),
                    "<br>Rendimiento: ", round(Rendimiento, 2),
                    "<br>Año: ", Field,
                    "<br>Región: ", Region)
    ) %>%
      # Agregar puntos DOA
      add_trace(
        data = doa_points,
        x = ~Densidad,
        y = ~Rendimiento,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10, color = 'black', symbol = 'diamond'),
        name = 'DOA (Dosis Óptima Agronómica)',
        hoverinfo = 'text',
        text = ~paste("Híbrido: ", Hibrido,
                      "<br>Densidad DOA: ", round(Densidad, 1),
                      "<br>Rendimiento: ", round(Rendimiento, 2),
                      "<br>Año: ", Field,
                      "<br>Región: ", Region),
        showlegend = FALSE
      ) %>%
      # Agregar puntos DOE
      add_trace(
        data = doe_points,
        x = ~Densidad,
        y = ~Rendimiento,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10, color = 'blue', symbol = 'circle'),
        name = 'DOE (Dosis Óptima Económica)',
        hoverinfo = 'text',
        text = ~paste("Híbrido: ", Hibrido,
                      "<br>Densidad DOE: ", round(Densidad, 1),
                      "<br>Margen Bruto: ", round(MB, 2),
                      "<br>Año: ", Field,
                      "<br>Región: ", Region),
        showlegend = FALSE
      ) %>%
      layout(
        title = "Comparativo de Curvas de Densidad",
        xaxis = list(title = "Densidad (miles/ha)", titlefont = list(size = 14)),
        yaxis = list(title = "Rendimiento (tn/ha)", titlefont = list(size = 14)),
        legend = list(title = list(text = "Híbridos"))
      )
  })
  
  
  
  #-------------Ambientes -------------------------------
  
  observe({
    # Verifica y selecciona el lote
    lote <- if (is.null(input$lote)) {
      showNotification("Lote no seleccionado. Cargando 'Guayaibi' por defecto.", type = "warning")
      "Guayaibi"
    } else {
      input$lote
    }
    
    archivo <- switch(lote,
                      "Guayaibi" = "Guayaibi.shp",
                      "Pertenencia" = "Pertenencia.shp",
                      "Laguna" = "Laguna.shp",
                      "Huinca_Martinez" = "Huinca_Martinez.shp",
                      stop("Lote no válido"))
    
    # Cargar y procesar los datos
    datos <- st_read(archivo)
    datos <- st_make_valid(datos)
    datos <- st_collection_extract(datos, "POLYGON")
    datos_coords <- st_coordinates(st_centroid(datos, of_largest_polygon = TRUE))
    datos <- cbind(datos, datos_coords)
    
    # Aplicar Fuzzy C-Means para cosecha
    valid_cosecha <- datos %>% filter(!is.na(Rinde))
    result_cosecha <- apply_fuzzy_cmeans(valid_cosecha, "Rinde", centers = 3)
    datos$cluster_cosecha <- result_cosecha$labels
    
    # Aplicar Fuzzy C-Means para prescripciones
    valid_prescripciones <- datos %>% filter(!is.na(Semilla))
    result_prescripciones <- apply_fuzzy_cmeans(valid_prescripciones, "Semilla", centers = 3)
    datos$cluster_prescripciones <- result_prescripciones$labels
    
    # Comparar clusters
    datos$comparacion <- ifelse(datos$cluster_cosecha == datos$cluster_prescripciones, "Igual", "Diferente")
    
    # Calcular frecuencias relativas
    frecuencias_relativas <- calcularFrecuencias(datos)
    frecuencias_df <- as.data.frame(as.table(frecuencias_relativas))
    colnames(frecuencias_df) <- c("cluster_prescripciones", "cluster_cosecha", "frecuencia")
    
    # Mapa de cosecha
    output$mapa_cosecha <- renderLeaflet({
      cluster_means <- tapply(datos$Rinde, datos$cluster_cosecha, mean, na.rm = TRUE)
      ordered_clusters <- names(sort(cluster_means)) # Ordenar los clusters por valores medios
      palette <- colorFactor(palette = c("red", "yellow", "green"), levels = ordered_clusters)
      
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(
          lng = ~X, lat = ~Y, 
          color = ~palette(cluster_cosecha), 
          fillOpacity = 1, radius = 3, 
          popup = ~paste("Cluster:", cluster_cosecha)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("red", "yellow", "green"),
          labels = ordered_clusters,
          title = "Cosecha",
          opacity = 1
        ) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    # Mapa de prescripciones
    output$mapa_prescripciones <- renderLeaflet({
      cluster_means <- tapply(datos$Semilla, datos$cluster_prescripciones, mean, na.rm = TRUE)
      ordered_clusters <- names(sort(cluster_means))
      palette <- colorFactor(palette = c("red", "yellow", "green"), levels = ordered_clusters)
      
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(
          lng = ~X, lat = ~Y, 
          color = ~palette(cluster_prescripciones), 
          fillOpacity = 1, radius = 3, 
          popup = ~paste("Cluster:", cluster_prescripciones)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("red", "yellow", "green"),
          labels = ordered_clusters,
          title = "Prescripciones",
          opacity = 1
        ) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    # Mapa de comparación
    output$mapa_comparacion <- renderLeaflet({
      leaflet(datos) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addCircleMarkers(
          lng = ~X, lat = ~Y, 
          color = ~colorFactor(palette = c("red", "green"), domain = datos$comparacion)(comparacion), 
          fillOpacity = 1, radius = 3, 
          popup = ~paste("Comparación:", comparacion)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("red", "green"),
          labels = c("Diferente", "Igual"),
          title = "Comparación",
          opacity = 1
        ) %>%
        fitBounds(lng1 = min(datos$X), lat1 = min(datos$Y), lng2 = max(datos$X), lat2 = max(datos$Y))
    })
    
    # Gráfico de contingencia
    output$grafico_contingencia <- renderPlot({
      ggplot(frecuencias_df, aes(x = cluster_prescripciones, y = cluster_cosecha, fill = frecuencia)) +
        geom_tile() +
        geom_text(aes(label = paste0(round(frecuencia, 2), "%")), size = 5) +
        scale_fill_gradientn(colours = c("red", "yellow", "green")) +
        labs(title = "% de coincidencias", x = "Ambientes Prescripción", y = "Ambientes Cosecha") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size = 16), plot.title = element_text(size = 12, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 14)) +
        scale_y_discrete(expand = c(0, 0))
    })
    
    # Calcular el margen neto
    datos_margen_neto <- reactive({
      datos <- datos
      req(input$precio_maiz, input$costo_semilla, input$costo_labores, input$costo_arrendamiento)
      
      # Verificar si existe la columna `ferti` en los datos
      if (!"ferti" %in% colnames(datos)) {
        # Si no existe, ignorar el costo del fertilizante en el cálculo
        datos <- datos %>%
          mutate(
            MargenNeto = (input$precio_maiz * Rinde) - 
              ( (Semilla/1000* input$costo_semilla) + input$costo_labores + input$costo_arrendamiento + input$costo_fito + input$costo_arrancador)
          )
      } else {
        # Si existe, incluir el fertilizante en el cálculo
        datos <- datos %>%
          mutate(
            MargenNeto = (input$precio_maiz * Rinde) - 
              (input$costo_semilla + (ferti * input$costo_ferti) + input$costo_labores + input$costo_arrendamiento + input$costo_fito + input$costo_arrancador)
          )
      }
      
      return(datos)
    })
    
    # Renderizar el mapa de margen neto
    output$mapa_margen_neto <- renderLeaflet({
      valid_data <- datos_margen_neto()
      
      # Definir cortes basados en quantiles
      if (min(valid_data$MargenNeto) < 0) {
        # Si hay valores negativos, asegurarse de que 0 sea un corte
        breaks <- c(quantile(valid_data$MargenNeto, probs = seq(0, 0.8, by = 0.2)), 0, 
                    quantile(valid_data$MargenNeto, probs = seq(0.2, 1, by = 0.2)))
        breaks <- sort(unique(breaks)) # Ordenar y eliminar duplicados
      } else {
        # Si todos los valores son positivos, simplemente usar quantiles
        breaks <- quantile(valid_data$MargenNeto, probs = seq(0, 1, by = 1/6))
      }
      
      # Calcular porcentajes de área para cada rango
      total_area <- sum(st_area(valid_data))
      area_percents <- sapply(1:(length(breaks)-1), function(i) {
        subset_data <- valid_data[valid_data$MargenNeto >= breaks[i] & valid_data$MargenNeto < breaks[i+1], ]
        return(sum(st_area(subset_data)) / total_area * 100)
      })
      
      # Definir la paleta de colores
      pal <- colorBin("RdYlGn", domain = valid_data$MargenNeto, bins = breaks)
      
      # Crear un vector de colores basado en los cortes
      colors <- sapply(breaks[-length(breaks)], pal)
      
      # Construir etiquetas personalizadas
      legend_labels <- mapply(function(b1, b2, ap) {
        sprintf("%d - %d (%0.2f%%)", floor(b1), floor(b2), ap)
      }, breaks[-length(breaks)], breaks[-1], area_percents)
      
      # Crear mapa
      leaflet(valid_data) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>%  # Imagen satelital de fondo
        addPolygons(color = ~pal(MargenNeto),
                    weight = 1,
                    fillOpacity = 0.7,
                    label = ~sprintf("Margen Neto: USD %d", floor(MargenNeto)),
                    highlight = highlightOptions(weight = 3,
                                                 color = "#666",
                                                 fillOpacity = 0.9,
                                                 bringToFront = TRUE)) %>%
        addLegend(
          title = "Margen Neto",
          labels = legend_labels,
          colors = colors,
          opacity = 0.8,
          position = "bottomright",
          labFormat = labelFormat(digits = 0, suffix = " USD"),
          # Reducir el tamaño de las leyendas
          className = "info legend legend-small"
        )
    })
    
    # Ajustar el estilo CSS para la leyenda
    tags$head(
      tags$style(HTML("
    .legend-small {
      font-size: 4px; /* Reducir el tamaño de fuente */
      line-height: 4px; /* Ajustar el espaciado */
      padding: 4px; /* Reducir el padding */
    }
  "))
    )
    
    output$indice_ajuste <- renderPlotly({
      # Calcular el índice de ajuste de prescripciones
      
      # Sumar las combinaciones perfectas
      combinaciones_perfectas <- sum(frecuencias_relativas["Baja", "Baja"]) + 
        sum(frecuencias_relativas["Media", "Media"]) + 
        sum(frecuencias_relativas["Alta", "Alta"])
      
      # Limitar las transiciones a un máximo del 30%
      transiciones_media_alta <- min(frecuencias_relativas["Media", "Alta"], 30)
      transiciones_media_baja <- min(frecuencias_relativas["Media", "Baja"], 30)
      
      # Calcular las transiciones ajustadas
      transiciones <- transiciones_media_alta + transiciones_media_baja
      
      # Calcular el índice de ajuste
      indice_ajuste <- (combinaciones_perfectas + transiciones) / 3
      
      print(paste("Combinaciones perfectas:", combinaciones_perfectas))
      print(paste("Transiciones:", transiciones))
      print(paste("Índice de ajuste:", indice_ajuste))
      
      # Crear el gráfico de medidor radial o podemos mejorar ???                      
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = indice_ajuste,
        gauge = list(
          axis = list(range = list(0, 100)),
          steps = list(
            list(range = c(0, 20), color = "red"),
            list(range = c(20, 50), color = "yellow"),
            list(range = c(50, 70), color = "lightgreen"),
            list(range = c(70, 100), color = "green")
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = indice_ajuste
          )
        )
      )
      
      fig <- fig %>%
        layout(
          title = "Índice de Ajuste de Prescripciones"
        )
      
      fig
    })
    
    
    
    
  })
  
    
   
    
    ##---------------------------------------------------------------------------------------
    ##Foliares
    
    # Reactivo para filtrar los datos en Foliares por región y cultivo
    filteredDatafoliares <- reactive({
      if (input$todas_regiones || "Todas" %in% input$regiones) {
        
        data %>%
          filter(Cultivo %in% input$cultivo)
      } else {
        
        data %>%
          filter(Regiones %in% input$regiones, Cultivo %in% input$cultivo)
      }
    })
    
    
    # Calcular el promedio de rendimiento y diferencia porcentual respecto al testigo
    summaryData <- reactive({
      data_filtered <- filteredDatafoliares()
      testigo <- data_filtered %>% filter(Tratamiento == "Testigo")
      promedio_testigo <- mean(testigo$Rinde, na.rm = TRUE)
      
      data_filtered %>%
        group_by(Region = if (input$todas_regiones || "Todas" %in% input$regiones) "Global" else Regiones, Cultivo, Tratamiento) %>%
        summarise(Rinde_Promedio = mean(Rinde, na.rm = TRUE),
                  Diferencia_Porcentual = round(((Rinde_Promedio - promedio_testigo) / promedio_testigo) * 100, 1),
                  Dif = unique(Dif)) %>%
        distinct(Cultivo, Tratamiento, .keep_all = TRUE)
    })
    
    # Crear el gráfico de barras
    output$barPlot <- renderPlot({
      ggplot(summaryData(), aes(x = Tratamiento, y = Rinde_Promedio, fill = Tratamiento)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_text(aes(label = paste0(round(Diferencia_Porcentual, 1), "%")), vjust = -0.5, fontface = "bold") +
        geom_text(aes(label = Dif), vjust = 1.5, color = "black") +
        geom_text(aes(label = round(Rinde_Promedio, 1)), vjust = 15, fontface = "bold", color = "black", position = position_dodge(width = 0.9)) +
        labs(title = "Rendimiento tn/ha",
             x = "Tratamiento",
             y = "Rinde tn/ha") +
        theme_minimal() + theme(panel.background = element_rect(fill = 'transparent', color = NA))+
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14))
    })
    
    # Crear la tabla resumen
    output$summaryTable <- renderDataTable({
      datatable(summaryData(), options = list(pageLength = 10))
    })
    
    
    
    #------------------------------------ indice de ajuste 
    
   

}

shinyApp(ui = ui, server = server)

