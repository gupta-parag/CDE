library(shiny)
library(reactable)
library(shinyWidgets)



setwd("C:\\Users\\pgupta\\Dropbox\\After_Ammar_House\\New-App")
source("cdFuncs.R")



ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback');
      @import url('https://fonts.googleapis.com/css?family=Nunito:400,600,700&display=fallback');
      h2 {
          padding-left:16px;
          font-family:  Nunito, sans-serif;
      }
      .my-class {
      font-family: Nunito;
      }
      #headdd{
      background-color: #F2F2F2 ;
      padding : 10px;
      }
      
      #title{
       font-family: Nunito, sans-serif;
       font-size: 30px;
       padding: 16px;
      }
      
      .my-class2 {
      font-family: Nunito;
      }
     
      "))
  ),
  tabsetPanel(
    tabPanel( "Tables Overview",
              fluidRow( textOutput("title"),
                        column(4, pickerInput("year", "Select Year", choices = 2000:2022,
                                              options = list(
                                                `live-search` = TRUE,
                                                style = "my-class2"
                                              ), 
                                              choicesOpt = list(
                                                style = rep_len("font-family: Nunito;",length( 2000:2022
                                                ))
                                              )
                        )),
                        column(4,pickerInput("yearE", "Select Type of Estimates",
                                             choices = c("5 year Estimate", "3 year Estimate", "1 Year Estimate"),
                                             options = list(style = "my-class"),
                                             choicesOpt = list(
                                               style = rep_len("font-family: Nunito;",3)
                                             )
                        )),
                        column(4,actionButton("go", "Enter")
                        )),
              reactableOutput("acs_table")
               ),
    # tabPanel("Data Download",
    #          tags$div(
    #             h4("Select Table Attributes"),
    #          fluidRow(
    #            column(4, pickerInput("tableID", "Select Table ID", 
    #                                  choices =  x$Unique_Table_ID,
    #                                  options = list(
    #                                    `live-search` = TRUE,
    #                                    style = "my-class2"
    #                                  ), 
    #                                  choicesOpt = list(
    #                                    subtext = str_c(x$Concept," ", x$Difference),
    #                                    style = rep_len("font-family: Nunito;",length( x$Unique_Table_ID))
    #                                  )
    #            )),
    #            column(4, pickerInput("dstype", "Select DataSets", 
    #                                choices =  c("Census",
    #                                             "ACS 5 yr Est.",
    #                                             "ACS 3 yr Est.",
    #                                             "ACS 1 yr Est."),
    #                                options = list(
    #                                  `live-search` = TRUE,
    #                                  style = "my-class2"
    #                                ), 
    #                                choicesOpt = list(
    #                                  style = rep_len("font-family: Nunito;",length( c("ACS 5 yr Est.",
    #                                                                                   "ACS 3 yr Est.",
    #                                                                                   "ACS 1 yr Est.",
    #                                                                                   "Census")))
    #                                )
    #          )),
    #          column(4, pickerInput("yeardd", "Select Year", 
    #                                choices =  2000:2022,
    #                                options = list(
    #                                  `live-search` = TRUE,
    #                                  style = "my-class2"
    #                                ), 
    #                                choicesOpt = list(
    #                                  style = rep_len("font-family: Nunito;",length( 2000:2022))
    #                                )
    #          ))
    #          
    #          ), id = "headdd"), # div
    #          h4("Select Geography Attributes"),
    #          fluidRow(
    #            column(4,
    #                   pickerInput("geographydd", label = "What geography you want to visualize?",
    #                               choices = c( "Block Group" = "block group", "Census Tract" = "tract", "County" = "county",
    #                                            "State" = "state", "Nation" = "us"), selected = "tract")),
    #            column(4,
    #                   pickerInput("statedd", "Select State", 
    #                               choices = state_choices ,
    #                               choicesOpt = list(
    #                                 subtext = paste(unique(county_data$state),state_choices, sep = " | ")
    #                               ),
    #                               multiple = FALSE,
    #                               selected = state_choices[14], 
    #                               options = list('actions-box' = TRUE, 
    #                                              'deselect-all-text' = 'None',
    #                                              'live-search' = TRUE,
    #                                              'multiple-separator' = ' , ',
    #                                              'select-all-text' = 'All',
    #                                              'selected-text-format' = 'values'
    #                               ))),
    #            column(4,
    #                   pickerInput("countydd", "Select County", 
    #                               choices = c(),multiple = TRUE,
    #                               options = list('actions-box' = TRUE, 
    #                                              'deselect-all-text' = 'None',
    #                                              'live-search' = TRUE,
    #                                              'multiple-separator' = ' , ',
    #                                              'select-all-text' = 'All',
    #                                              'selected-text-format' = 'values'
    #                               ))
    #                   
    #                   )
    #            
    #          ),
             #fluidRow(actionButton("godd", "Enter")),
            # reactableOutput("dd")
           #  ) #tabPanel
  ) #tabsetpanel
   
  
)

server <- function(input,output,session){}

# server <- function(input, output, session) {
#    
#   title_text <- eventReactive(input$go,{
#     str_c("ACS ", input$year, ", ", input$yearE)
#   })
#   
#   
#   output$title <- renderText({
#     title_text()
#   })
#   
#   observeEvent(input$yearE, {
#     updatePickerInput(session, "year", 
#                       choices = list( "2000s" = 2000:2009, 
#                                      "2010s" = 2010:2019,
#                                      "2020s" = 2020:2022),
#                       choicesOpt = list(
#                         subtext = rep(input$yearE,each = length(2000:2022))
#                       )) })
#   
#   output$acs_table <- renderReactable({
#     reactable(x, groupBy = "Grouped_Table_ID",
#               columns = list(
#                 Grouped_Table_ID = colDef(name = "Table ID"),
#       Concept = colDef(name = "Table Name", aggregate = "unique"),
#       Difference = colDef(name = "DIffernece b/w Groups",aggregate = "unique"),
#       Unique_Table_ID = colDef(name = "Unique Table ID",aggregate = "unique")
#     ),
#     bordered = T,
#     height = 850,
#     highlight = T,
#     filterable = T,
#     resizable = T,
#     wrap = T,
#     paginationType = "jump",
#     showPageSizeOptions = T,
#     pageSizeOptions = c(10,25,50,100),
#     defaultPageSize = 25,
#     theme = reactableTheme(
#       style = list(
#         fontFamily = "Nunito, sans-serif"
#       )
#     ))
#   })
#   
#   
#   observeEvent(input$statedd, {
#     updatePickerInput(session, "countydd", choices = returning_county(input$statedd),
#                       selected = returning_county(input$statedd)[15],
#                       choicesOpt = list(
#                         subtext = returning_county(input$statedd) ))
#   })
#   
#   table_dd <- eventReactive(input$godd,{ 
#    get_data(table_id = input$tableID, geo = input$geographydd,
#             st = input$statedd, co = input$countydd)})
#     
#   output$dd <- renderReactable({
#       reactable(table_dd(),
#                 theme = reactableTheme(
#                   style = list(
#                     fontFamily = "Nunito, sans-serif"
#                   )
#                 ))
#     })
#     
#   
#   
#   
# }

shinyApp(ui, server)