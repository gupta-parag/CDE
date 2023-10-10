library(shiny)
library(reactable)
library(DT)
library(shinyWidgets)

update_table <- function(extra_variables){
  if(length(extra_variables) == 2){
    return(table_w_names)
   # return(table_w_names[ grepl(pattern = "\\d$", x = table_w_names$Table_Id) , ])
    
  } else if (length(extra_variables) == 1){
    
    if(extra_variables == "Puerto Rico"){
      
      x <- str_sub(table_w_names$Table_Id, start = str_length(table_w_names$Table_Id) - 3, end = str_length(table_w_names$Table_Id))
      aplhabets <- paste(str_to_upper(letters[1:9]), collapse = "|")
      log_vector <- grepl(pattern = alphabets, x)
      return(table_w_names[ !log_vector, ])
     
    } else {
      
      return( table_w_names[ !grepl(pattern = "PR$", x = table_w_names$Table_Id) , ])
    }
  }
   else{
     return(table_w_names_wo_PR_wo_races)
   }
}

get_data_d_e <- function(table_id, geo, st = "00", co = "000", geometry_logical = 0){
  if(geo == "nation"){
    data_got <- get_acs(geography= "us", table = table_id, 
                        year = 2018,geometry = geometry_logical)
  } else if (geo == "state"){
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = 2018, state = st, output = "wide",
                        geometry = geometry_logical)
  } else {
    #county,tract, block group
    data_got <- get_acs(geography= geo, table = table_id, 
                        year = 2018, state = st, county = co,
                        output = "wide",geometry = geometry_logical)
  }
  return(data_got)
}

cleaning_data <- function(name_frame){
  v_18 <- load_variables(year = 2018, dataset = "acs5", cache = T)
  name_frame_f <- name_frame[ , !grepl(pattern = "M$", colnames(name_frame))]
  c_names <- gsub(pattern = "E$", replacement = "", 
                  x = colnames(name_frame_f)[3:ncol(name_frame_f)])
  c_names <- v_18$label[v_18$name %in% c_names]
  c_names_v1 <- gsub(pattern = "Estimate!!Total!!", replacement = "", x = c_names)
  c_names_v2 <- gsub(pattern = "!!| ", replacement = "_", c_names_v1)
  colnames(name_frame_f)[3:ncol(name_frame_f)] <- c_names_v2 
  return(name_frame_f)
}



################# dynamic UI ###################
list_selection_d_e <- list(pickerInput("state_d_e", "Select State", choices = unique(county_data$state_code),multiple = TRUE,
                                   options = list('actions-box' = TRUE, 
                                                  'deselect-all-text' = 'None',
                                                  'live-search' = TRUE,
                                                  'multiple-separator' = ' , ',
                                                  'select-all-text' = 'All',
                                                  'selected-text-format' = 'values'
                                   )),
                       pickerInput("county_d_e", "Select County", 
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
tabsetPanel( position = "below",
  
      tabPanel("Census Data: Preview", 
           checkboxGroupInput("extra_variables", label = "Select to include the following for the preview: ", choices = c("Puerto Rico", "Races"), inline = T),
           actionButton("update", "Update Table"),
           actionButton("display", "Display Table"),
     reactableOutput("table")),
     #datatableOutput("census_data_preview")),
            tabPanel("Census Data : Download", 
                     sidebarLayout(
                       sidebarPanel(  pickerInput("table_census_d_e", "What table you want to download?", 
                                                  choices = table_w_names$Table_Id,
                                                  selected = ""),
                                      selectInput("geometry_d_e", "Do you want to download the geometry?", choices = c("1", "0")),
                                      pickerInput("geography_d_e", label = "What geography you want to visualize?",
                                                  choices = c("block group", "tract", "county",
                                                              "state", "us"), selected = "Nation"),
                                      uiOutput("dynamic_block_d_e"),
                                      actionBttn("get_data_d_e", "Get the goddamn data"),
                                      downloadBttn("final_data_d_e", "Download")),
                       mainPanel(dataTableOutput("download_data_d_e"))
                        )#sidebarlayout
                     ) #tabpanel
            ) #tabsetpanel  
) # fluidpage

server <- function(input, output, session) {

  output$dynamic_block_d_e <- renderUI({
 if (input$geography_d_e == "State"){
      pickerInput("state_d_e", "Select State", choices = unique(county_data$state_code))
 } else if(input$geography_d_e != "Nation" & input$geography_d_e != "State") {
      list_selection_d_e
    }
  })
  
  observeEvent(input$state_d_e, {
    updatePickerInput(session, "county_d_e", choices = county_data$county_code[county_data$state_code %in% input$state_d_e])
  })
  
  output$table <- renderReactable(
    reactable(table_w_names_wo_PR_wo_races[, c(1,2,3)], details = function(index) {
      nested_data <- comprehensive_acs_data_pr_r[[as.numeric(index)]]
      reactable(nested_data, outlined = TRUE, compact = T)
    })
  ) # renderReactable
  
  new_table <- eventReactive(input$display, {
    update_table(input$extra_variables)
  })
  
 observeEvent(input$display, {
   updateReactable("table", data = new_table())
 })   

 
 download_table_d_e <- eventReactive(input$get_data_d_e,{
   cleaning_data(get_data_d_e(table_id = input$table_census_d_e, 
                geo = input$geography_d_e, st = input$state_d_e, 
                co = input$county_d_e, geometry_logical = as.numeric(input$geometry_d_e)))
 })
 output$download_data_d_e <- renderDataTable(download_table_d_e())
 
 
 
}

shinyApp(ui, server)