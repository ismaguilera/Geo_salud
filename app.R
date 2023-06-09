#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("leaflet")
# install.packages("sp")
# install.packages("chilemapas")

# Sys.setlocale("LC_ALL", "Spanish_Chile.utf8")
library(data.table)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(chilemapas)
library(dplyr)
library(lubridate)

# https://rpubs.com/mcortinat/759486
vacuna_dosis <- fread('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto80/vacunacion_comuna_1eraDosis.csv')

vacuna_dosis<-vacuna_dosis[!like(Comuna,"Desconocido"),]
Fechas <- names(vacuna_dosis)
Fechas <- Fechas[6:124]
vacuna_dosis_1<-melt(data = vacuna_dosis,id.vars = c("Region","Comuna","Codigo comuna","Poblacion"),measure.vars = Fechas ,variable.name = "Fecha")
vacuna_dosis_1[,suma_acumulada_primera_dosis:= cumsum(value), by=.(`Codigo comuna`,Comuna)]
vacuna_dosis_1[,Porcentaje_Primeras_dosis:=(suma_acumulada_primera_dosis/Poblacion*100),by=.(`Codigo comuna`,Comuna)]
vacuna_dosis_1[, `Codigo comuna` := ifelse(`Codigo comuna` < 10000 , paste0("0",vacuna_dosis_1$`Codigo comuna`), vacuna_dosis_1$`Codigo comuna`)] # Agregar un cero adelante en codigo de las comunas que tengan 4 digitos 
vacuna_dosis_1[,Fecha:=as.Date(Fecha, format = "%Y-%m-%d")]
vacuna_dosis_1[,ult_dia := ceiling_date(ymd(Fecha), 'month') - days(1)]
vacuna_dosis_2<-vacuna_dosis_1[ult_dia == Fecha|Fecha=="2021-04-21",]
vacuna_dosis_2[,Fecha_mes:=floor_date(ymd(Fecha), 'month')]


# PCR_covid <- fread('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto12/bulk/producto7.csv')

mapa<-merge(mapa_comunas,vacuna_dosis_2,by.x='codigo_comuna', by.y='Codigo comuna') # Merge entre el mapa de las comunas que nos proporciona el paquete chilemapas y el dataframe de los porcentajes. 


regiones<-as.data.table(codigos_territoriales)[,.N,by=.(nombre_region,codigo_region)]
lista_opciones<-append(c("Chile" = "*"),setNames(regiones[,codigo_region], regiones[,nombre_region]))

mapa<-st_sf(mapa) # Convertie el dataframe de data.table a sf

# bins<-append(seq(0,100,20),200) # Considera valores superiores
bins<-seq(0,100,20) # Intervalos del porcentaje para los colores del mapa

paleta<-colorBin(palette = 'Reds',domain = vacuna_dosis_2$Porcentaje_Primeras_dosis,bins = bins) # Paleta de colores para el mapa

# leaflet(mapa)%>%
#     addProviderTiles(provider = "OpenStreetMap.Mapnik")%>%
#     addPolygons(color = ~paleta(Porcentaje_Primeras_dosis),
#                 weight = 1,
#                 fillOpacity = 0.8,
#                 label = labels)%>%
#     addLegend(pal = paleta, values = ~bins, opacity = 1,position = "bottomright",title ="Porc. 1º dosis") # Agregar una leyenda

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Vacunas COVID-19 Chile"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "selREGION", label = "Seleccione región", choices = lista_opciones),
            textOutput("data"),
            sliderTextInput(inputId ="selFECHA", label = "Tiempo",
                            choice = format(sort(unique(mapa$Fecha_mes)), "%Y-%m"),
                            selected = "2021-04",
                            animate = TRUE)
        ),
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("leaflet")
        )
    )
    # tabPanel("Data explorer",
    #          hr(),
    #          DT::dataTableOutput("ziptable")
    # ),
    # 
    # conditionalPanel("false", icon("crosshair"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$data <- renderText({cat(paste0("Número de región: ",input$selREGION, "\n",
                                          "Fecha: ",input$selFECHA ))})
    
    # Rendereamos el texto
    output$leaflet <- renderLeaflet({
        mapa_region <- mapa %>%
            filter(codigo_region %like% input$selREGION & Fecha_mes == as.Date(paste0(input$selFECHA,"-01"), format = "%Y-%m-%d") )
        labels <- sprintf(
            "<strong>%s</strong><br/>Porc. Primera Dosis %.2f%%<br/>Población %i",
            mapa_region$Comuna, mapa_region$Porcentaje_Primeras_dosis,mapa_region$Poblacion
        ) %>% lapply(htmltools::HTML) # Labels al pasar arriba del mapa
        # Graficamos el mapa resultante
        leaflet(mapa_region)%>% 
            addProviderTiles(provider = "OpenStreetMap.Mapnik")%>%
            addPolygons(color = ~paleta(Porcentaje_Primeras_dosis),
                        weight = 5, 
                        fillOpacity = 0.8,
                        dashArray = "3",
                        label = labels)%>%
            addLegend(pal = paleta, values = ~bins*100, opacity = 1,position = "bottomright",title ="Porc. 1º dosis")
    })
    # output$ziptable <- DT::renderDataTable({
    #     df <- mapa_region
    #     action <- DT::dataTableAjax(df, outputId = "ziptable")
    #     
    #     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
