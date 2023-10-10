library(tidycensus)
library(stringr)
library(tidyr)
library(writexl)
library(stringr)

#census_api_key("efcd106a5d0c0801a0cdceb0caeff3d3c2afa307", install = TRUE, overwrite = T)

v_18 <- load_variables(year = 2018, dataset = "acs5", cache = T)
v_18_f <- separate(as.data.frame(v_18), col = 1, into = c("Table_Id","Variable_Number"),
                   sep = "_")

#parent table
table_w_names <- v_18_f[ !duplicated(v_18_f$Table_Id) , c(-2,-3)]
table_w_names$ID <- 1:nrow(table_w_names)


#### function to clean the data
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

###### 

get_counter <- 0 
getting_data <- function(numeric_vector){
  all_Census_data <- lapply(table_w_names$Table_Id[numeric_vector], function(x){
    get_counter <<- get_counter + 1
    print(get_counter)
    cleaning_data(get_acs(geography = "tract", state = "17", county = "031", table = x,
                          output = "wide", year = 2018, survey = "acs5"))
  })
  names(all_Census_data) <- table_w_names$Table_Id[numeric_vector]
  return(all_Census_data)
}

#### data structure for getting data in packets in of 10
data_in_10 <- lapply(1:113, sum)

### generating vector of packets in 10's
packet_10 <- lapply(1:113, sum)
counter <- 1
for(i in 0:112){
  k <- i * 10 + 1
  s <- k + 9         
  x <- seq(from = k, to = s)
  packet_10[[counter]] <- x
  counter <- counter + 1
}

####downloading data 
for( data_counter in 1:113){
   num_vector <- packet_10[[data_counter]]
 data_in_10[[data_counter]] <-   getting_data(num_vector)
 }

data_in_5 <- getting_data(1131:1135)
data_in_10[[114]] <- data_in_5 

######### final 2018 acs data in x
comprehensive_acs_data <- unlist(data_in_10, recursive = FALSE)
comprehensive_acs_data_pr_r <- comprehensive_acs_data[grep(pattern = "\\d$", x = names(comprehensive_acs_data))]






######## filtering out parent table
View(table_w_names)
pattern_logical <- grepl(pattern = "PR$", x = table_w_names$Table_Id)
table_w_names_wo_PR <- table_w_names[ !pattern_logical , ]
races_logical <- grepl(pattern = "\\(", table_w_names_wo_PR$concept)
table_w_names_wo_PR_wo_races <- table_w_names[ grepl(pattern = "\\d$", x = table_w_names$Table_Id) , ]

write_xlsx(table_w_names_wo_PR_wo_races, "pure_table.xlsx")


x <- table_w_names[ !grepl(pattern = "[:upper:]$",table_w_names$Table_Id), ]







x <- update_table(c("Races"))





