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
library(tidyr)


options(tigris_use_cache = T)

########## Choices Selecting County, State and Census data #################

county_data <- fips_codes
state_choices<- unique(county_data$state_code)[-56]
names(state_choices) <- unique(county_data$state_name)[-56]

county_choices <- as.data.frame(cbind(county_data$state_code, county_data$state,county_data$county_code, county_data$county))
colnames(county_choices) <- c("state_code", "state", "county_code", "county")


#Choices
datasets <- c("Total Population", "Household Population","Sex by Age", "Race", 
              "Ethnicity", "Education", "HH size by vehicles", "Median HH income", "Enrollment in School")
table_id <- c("B01003", "B11002" , "B01001", "B02001", "B03001", "B15002", 
              "B08201","B19013", "B14002")
names(table_id) <- datasets
######################################################################


#################### Ready web app functions #########################


####### returning unique tables and their names 

# return_uTables <- function(y = 2019, set = "acs5"){
#   v <- load_variables(year = y, dataset = set, cache = T)
#   v_f <- v %>% 
#     separate(col = name, into = c("tableID", "variable_id"), sep = "_")
#   v_ff <- v_f[ !duplicated(v_f$tableID),c(1,4)]
#   v19 <-  setDT(v_ff)
#   v19[ ,  `:=`( tableID = str_trim(tableID, side = "both"),
#                 COUNT = str_count(concept, pattern = "\\("),
#                 Alpha_ID_Count = str_count(tableID, 
#                 pattern = "[:alpha:]") - 1)  , ][,groupedID := str_sub(tableID,start = 1, 
#                 end = str_length(tableID) - Alpha_ID_Count),][ Alpha_ID_Count == 1 , 
#                 Alpha_ID := str_sub(tableID, -1)][Alpha_ID_Count == 2 , 
#                 Alpha_ID := str_sub(tableID, -2)][Alpha_ID_Count == 3 , 
#                Alpha_ID := str_sub(tableID, -3)][Alpha_ID_Count == 2 |
#                Alpha_ID_Count == 3,Location := "PR"][ (COUNT == 1 & Alpha_ID_Count == 1) | 
#                (COUNT == 1 & Alpha_ID_Count == 3), `:=`(BY_RACE = 
#               str_extract(concept, pattern = "\\(.+\\)"), concept = gsub(concept, pattern = "\\(.+\\)", 
#               replacement = ""))][  grepl(concept, pattern = 
#                "IN PUERTO RICO") , `:=`(Location= "PR", concept = gsub(concept, pattern = "IN PUERTO RICO",
#                replacement = "")), ][ grepl(concept, pattern = "IN THE UNITED STATES"),
#                  `:=`(Location= "US", concept = gsub(concept, pattern = "IN THE UNITED STATES",
#                   replacement = "")),]
#   
#   getting_names <-  data.frame(name =  gsub(str_extract(v19$concept[v19$COUNT == 2], 
#           pattern = "[:space:]\\(.+\\)$"), pattern = "\\).+\\(", replacement = "\\)_\\("))
#   
#   getting_names_f <- getting_names %>% slice(1:(n())) %>% #slice(1:(n() - 1))
#     separate(col = name, into = c("Junk", "Race"), sep = "_")
#   
#   v19_f <- v19[COUNT == 2 , BY_RACE:= getting_names_f$Race][ , .(groupedID,concept,
#           tableID,Location,BY_RACE,COUNT), ][is.na(Location), Location:= "",][is.na(BY_RACE), 
#            BY_RACE := "" ,][ ,BY_RACE := str_trim(str_c(Location, " ", BY_RACE), side = "both"),][,
#           Location:= NULL,][ COUNT== 2 , concept:= gsub(concept, 
#           pattern = str_c(paste0(getting_names_f$Race, collapse = "|"),"|\\(|\\)"), 
#           replacement = "")  ,][, COUNT:= NULL]
#   colnames(v19_f) <- c("Grouped_Table_ID", "Concept", "Unique_Table_ID", "Difference")
#   v19_f$Concept <- str_trim(v19_f$Concept)
#   return(v19_f[,c("Grouped_Table_ID", "Concept","Difference","Unique_Table_ID")])
# }

# x <- return_uTables()

#Cleaning column names
clean_col_names <- function(sf_frame, y = 2019){
  
  acs_2019 <- load_variables(year = y, dataset = "acs5")
  sf_frame_f <- sf_frame[,!grepl(colnames(sf_frame), pattern = "M$")]
  
  logi_vector <- acs_2019$name %in% gsub(colnames(sf_frame_f)[3:ncol(sf_frame_f)], 
                                         pattern = "E$", replacement = "") # getting into final format of B01001_001 from B01001_001E
  
  col_names_dirty <- acs_2019$label[logi_vector]
  
  
  clean_names <- mgsub::mgsub(col_names_dirty, 
                              pattern = c("Estimate!!Total:!!"," ", ":!!", "Estimate!!",":"), 
                              replacement = c('', '_','_','',''))
  print(clean_names)
  
  colnames(sf_frame_f)[3:ncol(sf_frame_f)] <- c(clean_names, "geometry")
  message("renaming")
  return(sf_frame_f[!st_is_empty(sf_frame_f),])
}

#Analysing Data

#general calculations
calc_percentage <- function(data_frame, cols , total) {
  y <- as.data.frame(st_drop_geometry(data_frame))
  percentage_matrix <- apply(y[ , cols] , 2, function(x) { 
    round( x  * 100/ y [ , total] ,2)  })
  colnames(percentage_matrix) <- str_c("P_" , colnames(percentage_matrix))
  final_data <- cbind(data_frame, percentage_matrix)
  return(final_data)
}

calc_stat <- function(census_data, col_name) {
  census_data <- st_drop_geometry(census_data)
  min_row <- which(census_data[, col_name] == min(census_data[, col_name]))
  median_row <- which(census_data[, col_name] == median(census_data[, col_name]))
  max_row <- which(census_data[, col_name] == max(census_data[, col_name]))
  
  final_list <- data.frame( ID = c(census_data[ min_row, "GEOID"], census_data[ median_row, "GEOID"], census_data[ max_row, "GEOID"]),
                            Values = c(census_data[ min_row, col_name], census_data[ median_row, col_name],census_data[ max_row, col_name]))
  
  # return(census_data[ c(min_row, median_row, max_row), c("GEOID", "Estimate_Total:") ])
  
}



#specific calculation
analyse_age <- function(sf_frame){
  df <- setDT(st_drop_geometry(sf_frame))
  y <- df[ ,`:=`(GEOID = GEOID,Total = Total, Male = Male, Female = Female, 
              Under_5 = Male_Under_5_years + Female_Under_5_years,
              bw_5_17 = Male_5_to_9_years + Male_10_to_14_years+ Male_15_to_17_years+
                Female_5_to_9_years + Female_10_to_14_years+Female_15_to_17_years,
              
              bw_18_24 = Female_18_and_19_years + Female_20_years +Female_21_years +Female_22_to_24_years+
                Male_18_and_19_years +Male_20_years+Male_21_years  + Male_22_to_24_years , 
              
              bw_25_34 =Male_25_to_29_years+Male_30_to_34_years +
                Female_25_to_29_years+Female_30_to_34_years,
              
              bw_35_64 = Male_35_to_39_years+Male_40_to_44_years+ Male_45_to_49_years+Male_50_to_54_years+
                Male_55_to_59_years+Male_60_and_61_years+Male_62_to_64_years +
                Female_35_to_39_years+Female_40_to_44_years+ Female_45_to_49_years+Female_50_to_54_years+
                Female_55_to_59_years+Female_60_and_61_years+Female_62_to_64_years,
              
              bw_65_P = Female_65_and_66_years+Female_67_to_69_years+Female_70_to_74_years+
                Female_75_to_79_years+Female_80_to_84_years+Female_85_years_and_over + Male_65_and_66_years+
                Male_67_to_69_years+Male_70_to_74_years+Male_75_to_79_years+Male_80_to_84_years+Male_85_years_and_over
  ) ,]
  y_geom <- st_sf(left_join(y, sf_frame[,1], by = "GEOID"))
  
  return(y_geom)
}



#API calls
get_data <- function(table_id, geo, st = "00", co = "000", geometry_logical = 1, y = 2019){
  if(geo == "us"){
    data_got <- get_acs(geography= "us", table = table_id, 
                        year = y,geometry = geometry_logical, output = "wide")
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
  message("Got Data")
  print(class(data_got))
  if(table_id == "B01001"){
    data_got <- calc_percentage(analyse_age(clean_col_names(data_got)), cols = 52:57 , total = 3)
  } else {
    data_got <- clean_col_names(data_got)
  }
  
  return(data_got)
}
###########################################################################


############## Data Visualization Functions #####################



create_map <- function(sf_frame){
  sf_frame <- st_transform(sf_frame, 4326)
  
  map <- leaflet() %>% 
          addTiles() %>%
            addPolygons(data = sf_frame, weight = 2)
  return(map)
}



#########################   Application Housekeeping    #############################################

returning_county <- function(state_selection){
  filtered_df <- county_choices[county_choices$state_code %in% state_selection , ]
  choices_vector <- as.vector(filtered_df$county_code)
  names(choices_vector) <- filtered_df$county
  return(choices_vector)
}
return_zoom <- function(geography) {
  if (geography == "block group" | geography == "tract" | geography == "county"){
    return(9.5)
  }
  else if (geography == "state"){
    return(6.5)
  }
  else if (geography == "nation"){
    return(3.5)
  }
}








