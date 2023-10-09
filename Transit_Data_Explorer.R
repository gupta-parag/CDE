library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidycensus)
library(shinythemes)
library(shinyWidgets)
library(stringr)



data("fips_codes")
county_data <- fips_codes
county_data$FIPS <- str_c(county_data$state_code, county_data$county_code)



get_transit_data <- function(state, county){

}




































#################### UI #####################
list_selection <- list(pickerInput("state_tde", "Select State", choices = unique(county_data$state),multiple = TRUE,
                                   options = list('actions-box' = TRUE, 
                                                  'deselect-all-text' = 'None',
                                                  'live-search' = TRUE,
                                                  'multiple-separator' = ' , ',
                                                  'select-all-text' = 'All',
                                                  'selected-text-format' = 'values'
                                   )),
                       pickerInput("county_tde", "Select County", 
                                   choices = c(),multiple = TRUE,
                                   options = list('actions-box' = TRUE, 
                                                  'deselect-all-text' = 'None',
                                                  'live-search' = TRUE,
                                                  'multiple-separator' = ' , ',
                                                  'select-all-text' = 'All',
                                                  'selected-text-format' = 'values'
                                   ))
)

############################################

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Transit Data Explorer",  style = "position: fixed;
              top: 50px; left: 0px; right: 0px; bottom: 0px;
              overflow: hidden;padding: 0;" ,
             leafletOutput("transit_map_tde", width = "100%", height = "100%"),
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,  
                           draggable = TRUE, top = 75, left = "auto", right = 20,
                           bottom = "auto", width = 330, height = "auto",
                           list_selection, 
                           actionButton("go","Let's Fucking do this!")
            )),
    tabPanel("GTFS data Explorer")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$state_tde, {
    updatePickerInput(session, "county_tde", choices = county_data$FIPS[county_data$state %in% input$state_tde])
  })
  
  
  

  output$transit_map_tde <- renderLeaflet(
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat = 38.852158, lng =  -95.433148, zoom = 4) %>%
      addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 150)) %>%
      addFullscreenControl( position = "topleft", pseudoFullscreen = FALSE) 
  )
    
    
}

shinyApp(ui, server)