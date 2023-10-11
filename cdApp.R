library(sf)
library(tidycensus)
library(leaflet)
library(dplyr)
library(stringr)
library(mgsub)
library(data.table)
library(shiny)
library(cartography)
library(reactable)
library(mgsub)
library(shinyWidgets)
library(shinythemes)
library(leaflet.extras)
library(cartography)
library(DT)


###################### Reactable using grouped column IDS and column names (hidden as preview) in the table   













################# Housekeeping #################

setwd("C:\\Users\\gupta\\Dropbox\\After_Ammar_House\\New-App")
source("cdFuncs.R")


###################################################










#################### Dynamic UI for selection ###########################################
list_selection <- list(pickerInput("state", "Select State", 
                                   choices = state_choices ,
                                   choicesOpt = list(
                                     subtext = paste(unique(county_data$state),state_choices, sep = " | ")
                                   ),
                                   multiple = FALSE,
                                   selected = state_choices[14], 
                                   options = list('actions-box' = TRUE, 
                                                  'deselect-all-text' = 'None',
                                                  'live-search' = TRUE,
                                                  'multiple-separator' = ' , ',
                                                  'select-all-text' = 'All',
                                                  'selected-text-format' = 'values'
                                   )),
                       pickerInput("county", "Select County", 
                                   choices = c(),multiple = TRUE,
                                   options = list('actions-box' = TRUE, 
                                                  'deselect-all-text' = 'None',
                                                  'live-search' = TRUE,
                                                  'multiple-separator' = ' , ',
                                                  'select-all-text' = 'All',
                                                  'selected-text-format' = 'values'
                                   ))
)


##############################################################################################

################ APP begins from here ##########################################



ui <- navbarPage(
  theme = shinytheme("flatly"), collapsible = TRUE, title = "Census Data Explorer", id="nav",
  tabPanel(
    "Map Explorer", style = "position: fixed;
                      top: 60px;
                      left: 0px;
                      right: 0px;
                      bottom: 0px;
                      overflow: hidden;
                      padding: 0;" ,
    
    leafletOutput("mymap", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,  
                  draggable = TRUE, top = 75, left = "auto", right = 20,
                  bottom = "auto", width = 330, height = "auto",
                  pickerInput("variable_census", "What variable do you want to visulaize?",
                              choices = table_id,
                              selected = table_id[2], options = list(`style` = "btn-info")),
                  pickerInput("geography", label = "What geography you want to visualize?",
                              choices = c( "Block Group" = "block group", "Census Tract" = "tract", "County" = "county",
                                           "State" = "state", "Nation" = "us"), selected = "tract",
                              options = list(`style` = "btn-info")),
                  textOutput("selected_variable"),
                  uiOutput("first_dynamic_block"),
                  actionButton("go", "VISUALIZE"), br(),
                  downloadButton(outputId = "download_shapefile",
                                 label = "Download Data"))),
  tabPanel("Data Selected", 
            tabsetPanel(
              tabPanel("Clean Data", dataTableOutput("selected_data")),
              tabPanel("Tables in 2019 ACS 5-Year Estimates", dataTableOutput("unique_tables"))
              )   ) #,
)

server <- function(input, output, session) {
  
  
  ##################### Choices and Dynaimc UI #################################################
  
  observeEvent(input$state, {
    updatePickerInput(session, "county", choices = returning_county(input$state),
                      selected = returning_county(input$state)[15],
                      choicesOpt = list(
                        subtext = returning_county(input$state) ))
  })
  
  output$first_dynamic_block <- renderUI({
    if (input$geography == "block group"){
      list_selection
    }
    else if (input$geography == "tract"){
      list_selection
    }
    else if (input$geography == "county"){
      list_selection
    }
    else if (input$geography == "state"){
      pickerInput("state", "Select State", 
                  choices = state_choices ,
                  choicesOpt = list(
                    subtext = paste(unique(county_data$state),state_choices, sep = " | ")
                  ),
                  multiple = TRUE,
                  options = list('actions-box' = TRUE, 
                                 'deselect-all-text' = 'None',
                                 'live-search' = TRUE,
                                 'multiple-separator' = ' , ',
                                 'select-all-text' = 'All',
                                 'selected-text-format' = 'values'
                  ))
      
    }
  })
  
#######################################################################################
  
  ####################### BackGround Map ###########################
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers[[113]]) %>%
      setView(lat = 38.852158, lng =  -75.433148, zoom = 4.2)
  })
  
  user_selected_df <-   
    eventReactive(input$go,{ get_data(table_id = input$variable_census, geo = input$geography,
                                                                        st = input$state, co = input$county)})
  
  observeEvent(input$go, {
    values =  st_drop_geometry( user_selected_df())
    values <- values[,3]
    
    if(nrow(user_selected_df()) >= 5) {
      breaks <- getBreaks(v = values, nclass = 5, method = "fisher")
      
    } else {
      breaks <-  sort(values)
    }
    
    colors_map <- colorBin("YlOrRd", domain = values, bins = breaks)
    y <- st_transform(user_selected_df(), crs = '+proj=longlat +datum=WGS84')
    coords <- st_bbox(user_selected_df())
    y_avg <- sum(coords[1], coords[3])/2
    x_avg <- sum(coords[2], coords[4])/2
    
    # labels <- sprintf(
    #   "<strong>GEOID : %s </strong><br/> %s : %s",
    #   st_drop_geometry(user_selected_df()[,"GEOID"])[,1],"Total ", 
    #   st_drop_geometry(user_selected_df()[,"Total"])[,1]) %>% lapply(htmltools::HTML)
    
    leafletProxy("mymap") %>% 
      clearShapes() %>% clearControls() %>%
      setView(lat = x_avg, lng =  y_avg, zoom = return_zoom(input$geography)) %>%
      addPolygons(data = st_transform(user_selected_df(), crs = '+proj=longlat +datum=WGS84'),
                  # label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",direction = "auto"),
                  weight = 0.5, dashArray = 2, color = "black",
                  highlight = highlightOptions(weight = 2, color = "white",
                                               fillOpacity = 0.1, bringToFront = TRUE, dashArray = ""),
                  fillColor = ~colors_map(values), fillOpacity = 0.7) %>%
      addLegend(pal = colors_map, values = values, 
                opacity = 0.7, title = "Name goes here", position = "bottomleft") %>%
      addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 150)) %>%
      addFullscreenControl( position = "topleft", pseudoFullscreen = FALSE) 
  })
  
  #####################################################################
  
  
  ####################### Data Tab ###########################
  
  output$selected_data <- renderDataTable( user_selected_df(), extensions = 'Buttons', 
                          options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
 # output$unique_tables <- renderDataTable({
 #   return_uTables()
 # })
  #################################################################
}

shinyApp(ui, server)



























############################################################################################  


























