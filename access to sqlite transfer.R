library(DBI)
library(tidyverse)
library(lubridate)

connection_string <-  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=J:/AQD/AQD/ASSESS/DATA MANAGEMENT AND QA UNIT/Data Review/Monthly Review/Toxics Data Review Database/Toxics Data Review DB.accdb"

con <- dbConnect(odbc::odbc(), .connection_string = connection_string)

tables <- DBI::dbListTables(con) 


tbl <- 
as_tibble(tables) %>%
  mutate(test = str_detect(tables, "tbl")) %>%
  filter(test == TRUE) %>%
  pull(value)


write_tbl <- function(tbl) {
   con <- dbConnect(odbc::odbc(), .connection_string = connection_string)
  
   value <-  tbl(con, tbl) %>%
             collect() 
  
   if ("SAMPDATE" %in% colnames(value)){
     value <- value %>% 
       mutate(SAMPDATE = as.character(SAMPDATE)) 
     
   } else {
     value <- value
   }
   
   if ("ANADATE" %in% colnames(value)){
     value <- value %>% 
       mutate(ANADATE = as.character(ANADATE)) 
     
   } else {
     value <- value
   }
   
   if ("PREPDATE" %in% colnames(value)){
     value <- value %>% 
       mutate(PREPDATE = as.character(PREPDATE)) 
     
   } else {
     value <- value
   }
   
   dbDisconnect(conn = con)
   
   con <- dbConnect(RSQLite::SQLite(), "NATTS.db")
  
   dbWriteTable(con, name = tbl, value = value , overwrite = TRUE)
  
   dbDisconnect(con)    
}


lapply(tbl,write_tbl)

dbDisconnect(con)      