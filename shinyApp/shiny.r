library(shiny)
library(DT)

load("cityMadison.Rdata")
load('super.Rdata')

server <- function(input, output) {
  # prepare dataset
  data=data.frame(x=as.numeric(as.character(cityMadison[,2])), 
                  y = as.numeric(as.character(cityMadison[,1])), 
                  id=as.character(cityMadison$name), 
                  category = as.numeric(as.character(cityMadison[,6])))
  subdata = super[,c(5,3,1,4)]
  names(subdata) = c("rank", "name", "user id", 'cluster')
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=data, ~x , ~y, layerId=~id,
                       popup=~id, radius=8 , color="black",  
                       fillColor="red", stroke = TRUE, fillOpacity = 0.99)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    p <- input$map_marker_click
  })
  
  datasetInput <- eventReactive(input$map_marker_click, {
    curr_id = input$map_marker_click$id
    curr_cate = data[which(data$id == curr_id),4]
    temp <- subdata[which(subdata$cluster == curr_cate),]
    temp <- temp[,c(1,2,3)]
    temp = temp[order(temp$rank),]
    names(temp) <- c("rank", "name", "user id")
    temp
  }, ignoreNULL = FALSE)
  
  # Filter data based on selections
  output$table <- renderTable({
    datasetInput()
  })
}

ui <- fluidPage(
  br(),
  column(8,leafletOutput("map", height="750px")),
  column(4,br(),br(),br(),br(),tableOutput("table")),
  br()
)

shinyApp(ui = ui, server = server)