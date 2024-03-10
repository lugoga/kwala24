require(shinydashboard)
require(shiny)
library(bslib)
require(shinydashboardPlus)
require(shinyWidgets)
require(shinycustomloader)
require(leaflet)
require(sf)
require(tmap)
require(tidyverse)
require(d3po)
require(DT)
require(highcharter)
require(RColorBrewer)




cbd.plots = st_read("../kwala/Vector/CBD Plots.gpkg") %>% st_transform(4326) %>% st_zm() %>% janitor::clean_names() %>% filter(!uses == "Road Network")
cbd.plots.uses = cbd.plots %>% st_drop_geometry() %>% distinct(uses) %>% pull()



wbd_reg2 = st_read("../kwala_github/wbd_reg.gpkg")


# user interface with map and color scheme widget
ui <- fluidPage(
  leafletOutput("map", height = 800),
  absolutePanel(
    top = 10, right = 10, draggable = TRUE,
    selectInput(inputId = "feature_id", label = "Color Scheme", choices = cbd.plots.uses)
    )
  )


# server definition
server <- function(input, output) {
  output$map <- renderLeaflet({
    
    # # use this line with leafletproxy for default colors
    # pal <- colorNumeric("BrBG", wbd_reg2$AREA) 
    # 
    # # map with polygons and legend
    # leaflet() %>%
    #   addTiles() %>%
    #   # setView(lng = -76.505206, lat = 38.9767231, zoom = 7) %>%
    #   addPolygons(data = wbd_reg2, 
    #               fill = TRUE,
    #               color = "black", 
    #               weight =1,
    #               fillColor = ~pal(AREA), # color is function of the wbd AREA column
    #               fillOpacity = 0.8,
    #               popup = ~HUC_NAME) %>%
    #   addLegend(position = "bottomright", pal = pal, values = wbd_reg2$AREA)
    
    pal.cbd.plots = colorFactor(palette = thirty_colors, domain = cbd.plots$uses)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(
        data = cbd.plots, 
        fill = TRUE,
        color = "black",
        weight =1,
        fillColor = ~pal.cbd.plots(uses),
        # color is function of the wbd AREA column
        fillOpacity = 0.8,
        popup = ~uses
      ) 
  })
  
  # update map using leaflet proxy instead of recreating the whole thing
  # use observe to color layer as needed
  observe({
    select.feature = cbd.plots %>% filter(uses %in% input$feature_id)
    
    # the map is updated with the new prefered color palette
    # notice we use leafletProxy() instead of leaflet()
    leafletProxy("map") %>%
      # clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = select.feature, 
        fill = TRUE,
        color = "black", 
        weight =1,
        fillColor = "grey80",
        fillOpacity = 0.5,
        popup = ~uses) 
  })
}


shinyApp(ui, server)

