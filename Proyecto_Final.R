#Nombre del Programador: Luis Pilaguano / David Mejía
#Fecha de Creación: 21/Oct/2024
#Descripción del programa o clase: Proyecto Final
#Análisis de la satisfacción del cliente en una empresa
#Evaluar los resultados de una encuesta de satisfacción del cliente, calcular las medias,
#medianas y modas de los puntajes y mostrar los resultados en diagramas circulares y de Pareto.
#Evaluar con un set de 200 datos aproximadamente.

# Las siguientes librerías son necesarias para poder ejecutar el proyecto
library(shiny)    # Interfaz gráfica
library(readxl)   # Leer archivos Excel
library(plotly)   # Crear gráficos interactivos (pastel 3D)
library(ggplot2)  # Crear gráficos (Pareto)
library(dplyr)    # Manipulación de datos
library(modeest)  # Cálculo de la moda

# Interfaz de usuario
ui <- fluidPage(
  # Título principal del aplicativo
  titlePanel("Análisis de Satisfacción del Cliente"),
  
  # Pantalla principal
  sidebarLayout(
    # Panel izquierdo con los objetos para la carga de archivo Excel
    sidebarPanel(
      fileInput("archivo", "Cargar archivo Excel", 
                accept = c(".xlsx", ".xls")),
      actionButton("calcular", "Calcular Estadísticas")
    ),
    
    # Panel central donde se colocará los gráficos
    mainPanel(
      tabsetPanel(
        tabPanel("Diagrama Circular", plotlyOutput("grafico_circular_3d")),
        tabPanel("Diagrama de Pareto", plotOutput("grafico_pareto")),
        tabPanel("Estadísticas", verbatimTextOutput("estadisticas"))
      )
    )
  )
)

# Lógica de servidor (Funciones, gráficos)
server <- function(input, output, session) {
  
  # Leer el archivo Excel cargado por el usuario
  # Se usa la expresión para permitir la actualización de datos automática
  # cuando se carga otro archivo. La información del archivo se guarda en
  # datos.
  datos <- reactive({
    req(input$archivo)  # Verifica que se haya cargado un archivo
    archivo <- input$archivo$datapath  # Ruta temporal del archivo
    read_excel(archivo)  # Cargar datos del archivo Excel
  })
  
  # Función para homologar los códigos de calificacón con su descripción
  # La información queda registrada en etiquetas_calificacion
  etiquetas_calificacion <- function(valor) {
    case_when(
      valor == 1 ~ "Malo",
      valor == 2 ~ "Regular",
      valor == 3 ~ "Bueno",
      valor == 4 ~ "Muy Bueno",
      valor == 5 ~ "Excelente"
    )
  }
  
  # RenderText se utiliza para crear salidas de texto dinámicas para cuando la data cambia
  output$estadisticas <- renderText({
    req(datos())  # Verifica que se hayan cargado datos
    
    calificaciones <- datos()$calificacion
    
    # Calcular media, mediana y moda
    media <- mean(calificaciones)
    mediana <- median(calificaciones)
    moda <- mlv(calificaciones, method="short")

    # Se asigna el resultado a una variable para poder imprimir en pantalla
    resultado <- paste0(
      "Estadísticas de la columna 'calificacion':\n",
      "Media: ", round(media, 2), "\n",
      "Mediana: ", mediana, "\n",
      "Moda: ", moda
    )
    resultado
  })
  
  # Diagrama Circular
  # RenderPlotly se utiliza para poder actualizar el gráfico cuando la data cambia
  output$grafico_circular_3d <- renderPlotly({
    req(datos())  # Verifica que se hayan cargado datos
    
    # Crear un data frame con las frecuencias y etiquetas personalizadas
    # Se usa tuberia %>% para encadenar todos los datos en frec
    frec <- datos() %>%
      count(calificacion) %>%                                 # Cantidad de veces que aparece cada calificación
      mutate(Etiqueta = etiquetas_calificacion(calificacion)) # Nueva columna con las etiquetas
    
    # Crear gráfico circular (pastel) en 3D con plotly
    fig <- plot_ly(
      frec,                            # Origen de datos
      labels = ~Etiqueta,              # Personalización de etiquetas de las categorías
      values = ~n,                     # Usa las frecuencias para graficar
      type = 'pie',                    # Gráfico tipo pastel
      hole = 0.3,                      # Agujero central para efecto 3D
      textinfo = 'label+percent',      # Muestra etiquetas y porcentaje
      insidetextorientation = 'radial' # Texto en orientación radial
    ) %>%
      layout(                          # Personalizar diseño
        title = list(                  # Título del gráfico
          text = "Distribución de Calificaciones (3D)",
          y = 0.98                     # Ajusta la posición vertical del título
        ),
        showlegend = TRUE,             # Mostrar leyendas del gráfico
        margin = list(t = 50)          # Ajusta margen superior
      )
    fig  # Pinta el gráfico
  })
  
  
  # Diagrama de Pareto
  # RenderPlot se utiliza para poder actualizar el gráfico cuando la data cambia
  output$grafico_pareto <- renderPlot({
    req(datos())  # Verifica que se hayan cargado datos
    
    # Crear un data frame con las frecuencias y etiquetas personalizadas
    # Se usa tuberia %>% para encadenar todos los datos en frec
    frec <- datos() %>%
      count(calificacion) %>%                                  # Cantidad de veces que aparece cada calificación
      arrange(desc(n)) %>%                                     # Ordena frecuencias de forma descendente
      mutate(Porcentaje_Acumulado = cumsum(n) / sum(n) * 100,  # Añade columna calculada porcentaje
             Etiqueta = etiquetas_calificacion(calificacion))  # Nueva columna con las etiquetas
    
    # Crear gráfico de Pareto con ggplot2
    ggplot(frec, aes(x = reorder(Etiqueta, -n), y = n)) +                        # Etiquetas en X de mayor a menor
      geom_bar(stat = "identity", fill = "skyblue") +                            # Barras del gráfico
      geom_line(aes(y = Porcentaje_Acumulado * max(n) / 100), 
                group = 1, color = "red", size = 1) +                            # Línea roja, representa el porcentaje
      geom_point(aes(y = Porcentaje_Acumulado * max(n) / 100), 
                 color = "red", size = 2) +                                      # Se añade puntos a la línea roja
      scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max(frec$n), 
                                             name = "Porcentaje Acumulado")) +   # Configuración de ejes X Y
      labs(title = "Diagrama de Pareto", x = "Calificación", y = "Frecuencia") + # Títulos
      theme_minimal()
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
