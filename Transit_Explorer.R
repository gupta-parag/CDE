library(tidycensus)
library(stringr)
library(data.table)
library(DT)
library(leaflet)
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet.extras)
library(sf)
library(cartography)

#######################    Housekeeping   ###############################
options(tigris_use_cache = TRUE)
county_data <- fips_codes

# selecting county, state and census data

state_choices <- unique(county_data$state_code)[-56]
names(state_choices) <- unique(county_data$state_name)[-56]

county_choices <- as.data.frame(cbind(county_data$state_code, county_data$state,county_data$county_code, county_data$county))
colnames(county_choices) <- c("state_code", "state", "county_code", "county")
##############################################################################


######################################  DYNAMCI UI #######################################
list_selection_te <- list(pickerInput("state_te", "Select State", 
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
                          pickerInput("county_te", "Select County", 
                                      choices = c(),multiple = TRUE,
                                      options = list('actions-box' = TRUE, 
                                                     'deselect-all-text' = 'None',
                                                     'live-search' = TRUE,
                                                     'multiple-separator' = ' , ',
                                                     'select-all-text' = 'All',
                                                     'selected-text-format' = 'values'
                                      ))
)
############################################################################################


########################################### FUNCTIONS #################################
returning_county <- function(state_selection){
  filtered_df <- county_choices[county_choices$state_code %in% state_selection , ]
  choices_vector <- as.vector(filtered_df$county_code)
  names(choices_vector) <- filtered_df$county
  return(choices_vector)
}
get_data <- function(table_id, geo, st = "00", co = "000", geometry_logical = 1, y = 2019){
  if(geo == "us"){
    data_got <- get_acs(geography= "us", table = table_id, 
                        year = y,geometry = geometry_logical)
  } else if (geo == "state"){
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = y, state = st, output = "wide",
                        geometry = geometry_logical)
  } else {
    #county,tract, block group
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = y, state = st, county = co,
                        output = "wide",geometry = geometry_logical)
  }
  return(data_got)
}
cleaning_data <- function(name_frame, geometry_logical = 1, y = 2019){
  v_y <- load_variables(year = y, dataset = "acs5", cache = T)
  name_frame_f <- name_frame[ , !grepl(pattern = "M$", colnames(name_frame))]
  c_names <- gsub(pattern = "E$", replacement = "", 
                  x = colnames(name_frame_f)[3:ncol(name_frame_f)])
  c_names <- v_y$label[v_y$name %in% c_names]
  c_names_v1 <- gsub(pattern = "Estimate!!Total!!", replacement = "", x = c_names)
  c_names_v2 <- gsub(pattern = "!!| ", replacement = "_", c_names_v1)
  c_names_v2 <- gsub(pattern = "Estimate_Total:_", replacement = "", c_names_v2)
  
  
  if(geometry_logical){
    last_col <- ncol(name_frame_f) - 1
    colnames(name_frame_f)[3:last_col] <- c_names_v2
    
  } else {
    colnames(name_frame_f)[3:ncol(name_frame_f)] <- c_names_v2 
  }
  
  colnames(name_frame_f) <- gsub(pattern = "Estimate_Total:_", 
                                 replacement = "", x = colnames(name_frame_f))
  return(name_frame_f)
}

analysing_data <- function(){
  # Male B01001_003:B01001_006[Less than 18], B01001_020:B01001_025 [Greater than or Equal to 65]
  # FeMale B01001_027:B01001_030[Less than 18], B01001_044:B01001_049 [Greater than or Equal to 65]
  v_y <- load_variables(year = y, dataset = "acs5", cache = T)
  under_18 <- str_c("B01001_", c("003", "004", "005", "006","027", "028", "029", "030"))
  sixty5_above <- str_c("B01001_0" ,c(20:25, 44:49))
  
  race <- c("B02001_001", "B02001_002" ,"B02001_003" ,"B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008")
  x <- cleaning_data( get_acs(geography = "block group", state = "17", 
                              county = "031", variables = race, output = "wide", geometry = T))
  
}

v_2019 <- load_variables(year = 2019, dataset = "acs5")  
  


#######################################################################################


################################### APP BEGINS FROM HERE #################################

ui <- navbarPage(
  theme = shinytheme("flatly"), collapsible = TRUE, title = "Census Data Explorer", id="nav",
  tabPanel(
    "Transit Explorer", style = "position: fixed;
                      top: 60px;
                      left: 0px;
                      right: 0px;
                      bottom: 0px;
                      overflow: hidden;
                      padding: 0;" ,
    
    leafletOutput("transit_map", width = "100%", height = "100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,  
                  draggable = TRUE, top = 75, left = "auto", right = 20,
                  bottom = "auto", width = 330, height = "auto",
                 
                  uiOutput("first_dynamic_block"),
                  actionButton("go", "VISUALIZE"), br(),
                  downloadButton(outputId = "download_shapefile",
                                 label = "Download Shapefile"))))

server <- function(input, output, session) {
  
  
  #selection of variables of Map Explorer
  observeEvent(input$state_te, {
    updatePickerInput(session, "county_te", choices = returning_county(input$state_te),
                      choicesOpt = list(
                        subtext = returning_county(input$state_te) ))
  })
  output$first_dynamic_block <- renderUI({
      list_selection_te
  })
  output$transit_map <- renderLeaflet({
    x <- leaflet() %>% 
      #addProviderTiles(providers$CartoDB.Positron) %>% 
      addTiles() %>% 
      setView(lat = 38.852158, lng =  -75.433148, zoom = 4.2) %>%
      addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 150)) %>%
      addFullscreenControl( position = "topleft", pseudoFullscreen = FALSE) 
  })
}

shinyApp(ui, server)

#################################################################################################



























