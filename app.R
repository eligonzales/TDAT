library(shiny)
library(tidyverse)
library(plotly)
library(xts)
library(dygraphs)
library(ggpmisc)
library(DT)
library(glue)
library(waiter)
library(lubridate)
library(DBI)
library(RSQLite)


# Server Functions --------------------------------------------------------


# Geometric mean function for summary stats section


gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

primary <- c(1,2,3,5,6,7,12)

# Main query function that makes Master data frame ---------------------------------
query_phx_voc <- function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblPXSSVOCPri") %>%
    union(tbl(con,"tblPXSSVOCSec")) %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>% 
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
  
}


# JLG Metals Query --------------------------------------------------------
# Only has a primary

query_phx_metals <- function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblMetals") %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>% 
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
}


# South Phoenix VOC Query -------------------------------------------------
#Only has a Primary

query_spaz_voc <- function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblSPAZVOC") %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>% 
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
}


# JLG SVOC Query ----------------------------------------------------------
query_phx_svoc <- function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblSVOC") %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>%  
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
}


# PAMS Carbonyl query -----------------------------------------------------


query_pams_carb <-  function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblPAMSCarbonylPri") %>%
    union(tbl(con,"tblPAMSCarbonylSec")) %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>% 
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
  
  
}


# NATTS Carbonyl Query  ------------------------------------------------------------

query_natts_carb <-  function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  df <- tbl(con, "tblCarbonylPri") %>%
    union(tbl(con,"tblCarbonylSec")) %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE, SITE_ID, RESULT,ANOTE, Description,DL) %>% 
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT)) %>%
    mutate(category = ifelse(SITE_ID %in% primary, "primary", "secondary")) %>%
    arrange(SAMPDATE)
  
  
}


# Query Function for statistical findings --------------------------------

#Having input$parameter in the Month will give all data with the unique month for the query. 
#Having YEAR(SAMPDATE) will give all the years of data with associated with the selected Month
#Having input$parameter in the WHERE clause to only select historical data preceding the selected year
#Having Month(SAMPDATE) = Month(#",input$start_date,"#)" as to only assign Month from SAMPDATE to fields that have the same Month

# Query PAMS Carbonyl Stats -----------------------------------------------


query_stats_pcarb <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblPAMSCarbonylPri
           LEFT JOIN tblAnalyte ON tblPAMSCarbonylPri.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))}"))  %>% as_tibble()
  
}

# Query NATTS Carbonyl Stats ----------------------------------------------

query_stats_ncarb <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblCarbonylPri
           LEFT JOIN tblAnalyte ON tblCarbonylPri.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))}"))  %>% as_tibble()
  
}

# Query NATTS SVOC Stats --------------------------------------------------

query_stats_svoc <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblSVOC
           LEFT JOIN tblAnalyte ON tblSVOC.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))} "))  %>% as_tibble()
  
}

# Query NATTS Metals Stats ------------------------------------------------

query_stats_met <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblMetals
           LEFT JOIN tblAnalyte ON tblMetals.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))}"))  %>% as_tibble()
  
}


# Query JLG NATTS VOC Stats ---------------------------------------------------

query_stats_voc <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblPXSSVOCPri
           LEFT JOIN tblAnalyte ON tblPXSSVOCPri.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))}"))  %>% as_tibble()
  
}

# Query SPAZ VOC Stats ----------------------------------------------------
query_stats_spaz <- function(start_date){
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  dbGetQuery(con, glue("SELECT strftime('%m',SAMPDATE) as MONTH,
           strftime('%Y',SAMPDATE) as YEAR, ANALYTE, RESULT 
           FROM tblSPAZVOC
           LEFT JOIN tblAnalyte ON tblSPAZVOC.ANALYTE_ID = tblAnalyte.ANALYTE_ID
                     WHERE CAST(strftime('%m',SAMPDATE) AS Integer) = {month(start_date)}
                     AND
                     CAST(strftime('%Y',SAMPDATE) AS Integer) <= {as.double(year(start_date))}"))  %>% as_tibble()
  
}


# Benzaldehyde toluene query ----------------------------------------------
query_carb_voc <- function(start_date, end_date) {
  
  con <- dbConnect(SQLite(), "NATTS.db")
  
  on.exit(dbDisconnect(con))
  
  tbl(con, "tblPXSSVOCPri") %>%
    union(tbl(con,"tblCarbonylPri" )) %>%
    filter(SAMPDATE >= start_date  & SAMPDATE <= end_date) %>%
    left_join(tbl(con, "tblAnalyte")) %>%
    left_join(tbl(con, "tblLabFlags")) %>%
    select(ANALYTE,SAMPDATE,RESULT) %>% 
    filter(ANALYTE %in% c("Benzaldehyde", "Toluene"))  %>%
    collect() %>%
    mutate(SAMPDATE =ymd_hms(SAMPDATE), 
           RESULT = as.double(RESULT),
           SAMPDATE = as_date(SAMPDATE)) %>%
    distinct(ANALYTE,SAMPDATE,RESULT) %>%
    pivot_wider(names_from = ANALYTE, values_from = RESULT) 
  
}

# Flag Descriptions
con <- dbConnect(SQLite(), "NATTS.db")

data_flag_desciption <- tbl(con, "tblLabFlags") %>%
  select(ANOTE, Description) %>%
  collect()

dbDisconnect(con)
# Load Screens ------------------------------------------------------------


waiting_screen <- tagList(
  spin_solar(),
  h4("Extracting Data...")
)

waiting_screen_2 <- tagList(
  spin_solar(),
  h4("Generating Table...")
)

# ggplot theme
theme_set(theme_light())

# Main Panel --------------------------------------------------------------

main_date <- mainPanel(use_waiter(),
                       tabPanel("Cross Tab",
                                DT::dataTableOutput("table")))

main_scatter <-  mainPanel( 
  plotOutput("scatter", brush = "scatter_brush"),
  dataTableOutput("scatter_table"))

main_time_series <- mainPanel(
  tabsetPanel(
    tabPanel("Line",
             dygraphOutput("time_series",height = "800px")),
    tabPanel('Bar Charts',
             dygraphOutput("bar_chart",height = "800px")),
    tabPanel("Stacked Bar",
             dygraphOutput("stacked_bar",height = "800px"))))


main_compare <- mainPanel( 
  dygraphOutput("compare", height = "800px")
)

main_bar <- fluidPage(actionButton("backward", "Backward"),
                      actionButton("forward", "Forward"),
                      actionButton("reset", "Reset" ),
                      plotlyOutput("bar", height = "800px"))

main_carb_voc <- fluidPage(inputPanel(actionButton("carb_voc_query", label = "Query")),
                           plotOutput("scatter_compare"))

main_stat <- fluidPage(use_waiter(),
                       actionButton("report","Report"),
                       DT::dataTableOutput("stats")
)

main_flags <- fluidPage(fluidRow(DT::dataTableOutput("flag")),
                        fluidRow(DT::dataTableOutput("flag_description")))

main_rpd <- fluidPage(DT::dataTableOutput("rpd"))


main_h_box <- mainPanel(
  tabsetPanel(
    tabPanel("Monthly Comparison",
             plotlyOutput("box_month",height = "800px")),
    tabPanel("Yearly Comparison",sidebarLayout(
      sidebarPanel = sidebarPanel(width = 4,
                                  selectInput("pollutant", "Pollutant", choices = NULL)),
      mainPanel = mainPanel(plotlyOutput("box_year",height = "800px"))))
  )
)

# Sidebar Panel -----------------------------------------------------------

sp_date <- sidebarPanel(width = 2,
                        selectInput("tbl", "Species", choices = c("JLG NATTS Carbonyls", "JLG VOCs","JLG Metals", "JLG SVOCs / PAHs", "JLG PAMS Carbonyls", "South Phoenix VOCs")),
                        dateInput("start_date", "Start Date"),
                        dateInput("end_date", "End Date"),
                        actionButton("search", "Search"))


sp_scatter <-  sidebarPanel(width = 2,
                            selectInput('x_axis', "X axis", choices = NULL, selected = NULL),
                            selectInput("y_axis", "Y axis", choices = NULL, selected = NULL),
                            sliderInput('size', 'size', min = 0, max = 10, value = 2),
                            sliderInput('alpha', 'alpha',min = 0, max = 1, value = 1)
)

sp_time_series <- sidebarPanel(width = 2,
                               checkboxGroupInput("carbonyl", "Pollutant", choices = NULL))

sp_compare<- sidebarPanel(width = 2, 
                          selectInput("carbonyl2", "Pollutant", choices = NULL ))


# tab panel ---------------------------------------------------------------


panel_date <-   tabPanel("Data Frame Range",icon = icon("calendar"),
                         sidebarLayout(sp_date,
                                       main_date))

panel_scatter <- tabPanel("Scatter Plot",
                          sidebarLayout(
                            sp_scatter,
                            main_scatter
                          ))

panel_box <- tabPanel("Boxplots", 
                      fluidPage(
                        numericInput("number", "x-axis", min = 0, max = 500, value = NULL),
                        plotlyOutput("boxplot",height = "800px")
                      ))
panel_carb_voc <- tabPanel("Benzaldehyde vs. Toluene", main_carb_voc)

panel_time_series <- tabPanel("Time Series",
                              sidebarLayout(
                                sp_time_series,
                                main_time_series
                              ))

panel_compare <- tabPanel("Primary v Secondary",
                          sidebarLayout(
                            sp_compare,
                            main_compare
                          ))

panel_bar <- tabPanel("Fingerprint",main_bar)

panel_stat <- tabPanel("Summary Stats",main_stat)

panel_flags <- tabPanel("Flag Report", main_flags)

panel_rpd <- tabPanel("RPD Report", main_rpd)


panel_h_box <- tabPanel("Historical Comparison",
                        fluidPage(
                          main_h_box
                        ))
# UI ----------------------------------------------------------------------

ui <- navbarPage(
  title = "Toxics Data Analysis Tool", 
  theme = bslib::bs_theme(bootswatch = "flatly"),
  panel_date,
  navbarMenu("Plot",icon = icon("chart-line"),
             panel_box,
             panel_scatter,
             panel_time_series,
             panel_compare,
             panel_bar,
             panel_carb_voc),
  navbarMenu("Reports", icon = icon("calculator"),
             panel_flags,
             panel_rpd,
             panel_stat),
  navbarMenu("Historical", 
             panel_h_box)
)
# Server ------------------------------------------------------------------

server <- function(input, output,session) {
  
  # Reactive Events ---------------------------------------------------------------
  #Using query functions
  #Telling the function to take a reactive dependency on the search button so 
  #data will not be pulled until the button is pressed
  
  data <- eventReactive(input$search, {
    
    waiter_show(html = waiting_screen, color = "#2C3E50")
    on.exit(waiter_hide())
    
    if (input$tbl == "JLG PAMS Carbonyls") {
      query_pams_carb(input$start_date, input$end_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    } else if (input$tbl == "JLG Metals") {
      query_phx_metals(input$start_date, input$end_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "JLG SVOCs / PAHs") {
      query_phx_svoc(input$start_date, input$end_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "South Phoenix VOCs") {
      query_spaz_voc(input$start_date, input$end_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "JLG VOCs") {
      query_phx_voc(input$start_date, input$end_date) 
      
    }else if (input$tbl == "JLG NATTS Carbonyls") {
      query_natts_carb(input$start_date, input$end_date) %>%
        mutate(RESULT = as.double(RESULT))
    }
  })
  
  observeEvent(data(),{ 
    choices <- unique(data()$ANALYTE)
    updateSelectInput("x_axis", "X Axis", choices = sort(choices), session = session)
    updateSelectInput("y_axis", "Y Axis", choices = sort(choices), session = session)
    updateCheckboxGroupInput("carbonyl", "Pollutant", choices = sort(choices), session = session)
    updateSelectInput("carbonyl2", "Pollutant", choices = sort(choices), session = session)
    updateSelectInput("pollutant", "Pollutant", choices = sort(choices), session = session)
  })
  
  #Using query function
  #Telling the function to take a reactive dependency on the search button so 
  #data will not be pulled until the button is pressed
  
  data_stats <- eventReactive(input$search, {
    
    if (input$tbl == "JLG PAMS Carbonyls") {
      query_stats_pcarb(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    } else if (input$tbl == "JLG Metals") {
      query_stats_met(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "JLG SVOCs / PAHs") {
      query_stats_svoc(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "South Phoenix VOCs") {
      query_stats_spaz(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "JLG VOCs") {
      query_stats_voc(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
      
    }else if (input$tbl == "JLG NATTS Carbonyls") {
      query_stats_ncarb(input$start_date) %>%
        mutate(RESULT = as.double(RESULT))
    }
  })
  
  
  data_monthly_stats <- eventReactive(input$report,{
    waiter_show(html = waiting_screen_2, color = "#2C3E50")
    on.exit(waiter_hide())
    
    if (data_stats() %>% distinct(YEAR) %>%  count() == 1){
      
      data_stats() %>% 
        select(ANALYTE,RESULT,YEAR, MONTH) %>%
        mutate(MONTH = as.double(MONTH),
               MONTH = month(MONTH, label = T)) %>%
        mutate(length_of_record = first(YEAR), current_year = last(YEAR),
               current_record = ifelse(current_year == YEAR, "Current","Historical")) %>%
        group_by(ANALYTE,  MONTH,  current_record, current_year, length_of_record) %>%
        summarise("geometric_mean" = round(gm_mean(RESULT),digits = 2), 
                  "min" = min(RESULT, na.rm = TRUE), "max" = max(RESULT, na.rm = TRUE),
                  "median" = median(RESULT, na.rm = TRUE)) %>%
        pivot_longer(geometric_mean:median, names_to = "stat", values_to = "value") %>%
        janitor::clean_names() %>%
        ungroup() %>%
        select(analyte,year = current_year, month, stat, value)
      
    } else {
      
      data_stats() %>% 
        select(ANALYTE,RESULT,YEAR, MONTH) %>%
        mutate(MONTH = as.double(MONTH),
               MONTH = month(MONTH, label = T)) %>%
        mutate(length_of_record = first(YEAR), current_year = last(YEAR),
               current_record = ifelse(current_year == YEAR, "Current","Historical")) %>%
        group_by(ANALYTE,  MONTH,  current_record, current_year, length_of_record) %>%
        summarise("geometric_mean" = round(gm_mean(RESULT),digits = 2), 
                  "min" = min(RESULT,na.rm = TRUE), "max" = max(RESULT,na.rm = TRUE), 
                  "median" = median(RESULT, na.rm = TRUE)) %>%
        pivot_longer(geometric_mean:median, names_to = "stat", values_to = "value") %>% 
        
        pivot_wider(names_from = current_record, values_from = value) %>%
        mutate("ratio" = round(Current / Historical,digits = 2),
               stat = str_to_title(stat),
               length_of_record = paste(length_of_record,current_year, sep = "-")) %>%
        ungroup() %>%
        select(ANALYTE, MONTH, length_of_record,stat,Current, Historical, ratio) %>%
        janitor::clean_names()
    }
    
  })
  
  data_carb_voc <- eventReactive(input$carb_voc_query, {
    query_carb_voc(input$start_date, input$end_date)
  })
  
  
  
  # Reactive Expressions ----------------------------------------------------
  ## Filtering and Manipulating Query Tables (data() and data_stats() for Plots
  
  #creating cross tab data frame from data() for scatter plots
  
  data_cross_tab <- reactive({
    data() %>%
      filter(category == "primary") %>%
      distinct(SAMPDATE,ANALYTE,RESULT) %>%
      # mutate(SAMPDATE = ymd_hm(SAMPDATE)) %>%
      pivot_wider(names_from = "ANALYTE", 
                  values_from = "RESULT" )})
  
  
  #creating data frame where column names are the site ID to compare primary vs secondary
  
  data_compare <- reactive({
    data() %>%
      select(SAMPDATE, ANALYTE, category, RESULT) %>%
      distinct(SAMPDATE, ANALYTE,category,RESULT) %>%
      pivot_wider(names_from = category, values_from = RESULT)
  })
  
  #grabs the unique dates from the queried data frame
  #reactive expression will be used in the Fingerprint plot section as a means
  #to filter for unique dates in the queried data frame
  
  data_flags <- reactive({
    data() %>%
      select(SAMPDATE, ANALYTE, category, RESULT, ANOTE) %>% 
      janitor::clean_names() %>%
      filter(!is.na(anote))
  })
  
  date <- reactive({
    unique(data()$SAMPDATE)
  })
  
  
  
  
  
  # Reactive Values ---------------------------------------------------------
  #value() will be utilized in the fingerprint plot section
  #it will help index unique dates 
  
  value <- reactiveVal(1)
  
  observeEvent(input$forward,{
    if (value() < length(unique(date()))) {
      new_value <- value() + 1
      value(new_value)
    }
    else {
      new_value <- value() / value()
      value(new_value)
    }
  })
  
  observeEvent(input$backward,{
    if (value() == 1) {
      new_value <-value() * length(unique(date()))
      value(new_value)
    }
    else {
      new_value <- value() - 1
      value(new_value)
    }
  })
  observeEvent(input$reset,{
    new_value <- value()/value()
    value(new_value)
  })
  
  observeEvent(input$search,{
    new_value <- value() / value()
    value(new_value)
  })
  
  
  # Update Stats Report button ----------------------------------------------
  
  observe({
    req(input$start_date)
    
    # Updates goButton's label and icon
    updateActionButton(session, "report",
                       label =paste( month(input$start_date,label = T), year(input$start_date), sep = "-"))
    
  })
  
  
  # Outputing Data tables ---------------------------------------------------
  
  # Outputing cross tab datatable. Replace data_cross_tab() with data() to view Master data frame. Or data_compare() to view the data frame used for Primary V Secondary Comparison   
  
  output$table <- DT::renderDataTable(server = FALSE,
                                      DT::datatable({data_cross_tab()},
                                                    rownames = FALSE,
                                                    extensions = c("Scroller","Buttons"), 
                                                    options = list(
                                                      paging = TRUE,
                                                      searching = TRUE, 
                                                      autoWidth = TRUE, 
                                                      ordering = TRUE, 
                                                      scrollX = TRUE,
                                                      dom = "SBfrtip",
                                                      buttons = c('copy', 'csv', 'excel', 'colvis')
                                                    ),
                                                    class = "display") %>%
                                        formatDate("SAMPDATE", method = "toLocaleString")
  )
  
  
  #Outputing statistical finding table from data_monthly_stats()
  #rowGroup groups by the first column of the table (ANALYTE in this case)
  
  output$stats <-  DT::renderDataTable(server = FALSE,
                                       
                                       DT::datatable({data_monthly_stats()},
                                                     rownames = FALSE,
                                                     filter = "top",
                                                     extensions = c("Scroller","Buttons","RowGroup","FixedHeader"), 
                                                     options = list(
                                                       paging = FALSE,
                                                       searching = TRUE, 
                                                       ordering = TRUE, 
                                                       scrollX = TRUE,
                                                       scrollY = TRUE,
                                                       selection = "none",
                                                       fixedHeader = TRUE,
                                                       dom = "SBfrtip",
                                                       buttons = c('copy', 'csv', 'excel', 'colvis'))))
  
  output$flag <-  DT::renderDataTable(server = FALSE,
                                      
                                      DT::datatable({data_flags()},
                                                    rownames = FALSE,
                                                    filter = "top",
                                                    extensions = c("Scroller","Buttons"), 
                                                    options = list(
                                                      paging = TRUE,
                                                      searching = TRUE, 
                                                      ordering = TRUE, 
                                                      scrollX = TRUE,
                                                      scrollY = TRUE,
                                                      selection = "none",
                                                      dom = "SBfrtip",
                                                      buttons = c('copy', 'csv', 'excel', 'colvis'))) %>%
                                        
                                        formatDate("sampdate", method = "toLocaleString"))
  
  output$rpd <- DT::renderDataTable(server = FALSE,
                                    
                                    DT::datatable({ data() %>%
                                        
                                        select(SAMPDATE, ANALYTE, category, RESULT,DL) %>% 
                                        distinct(SAMPDATE, ANALYTE,category,RESULT,DL) %>%
                                        pivot_longer(RESULT:DL, names_to = "stat", values_to = "value") %>%
                                        transmute(SAMPDATE,ANALYTE, value,
                                                  stat = case_when(category == "primary" & stat == "DL" ~ "primary_mdl",
                                                                   category == "secondary" & stat == "DL" ~ "secondary_mdl",
                                                                   category == "primary" & stat == "RESULT" ~ "primary",
                                                                   category == "secondary" & stat == "RESULT" ~ "secondary")) %>%
                                        pivot_wider(names_from = stat, values_from = value) %>% 
                                        mutate(SAMPDATE, ANALYTE, primary,secondary,
                                               primary_mdl = primary_mdl * 5,
                                               secondary_mdl = secondary_mdl * 5, 
                                               dif = abs(primary - secondary),
                                               mean = ((primary + secondary) / 2),
                                               rpd = round(dif / mean, 3)*100) %>%
                                        filter(!is.na(secondary),
                                               !is.na(primary)) %>%
                                        filter(primary > primary_mdl | secondary > secondary_mdl) %>%
                                        select(SAMPDATE,ANALYTE,primary,secondary,rpd, primary_mdl, secondary_mdl) %>%
                                        arrange(SAMPDATE)},
                                        rownames = FALSE,
                                        filter = "top",
                                        extensions = c("Scroller","Buttons"), 
                                        options = list(
                                          paging = TRUE,
                                          searching = TRUE, 
                                          ordering = TRUE, 
                                          scrollX = TRUE,
                                          scrollY = TRUE,
                                          selection = "none",
                                          dom = "SBfrtip",
                                          buttons = c('copy', 'csv', 'excel', 'colvis'))) %>%
                                      
                                      formatDate("SAMPDATE", method = "toLocaleString"))
  
  
  output$flag_description <- DT::renderDataTable(server = FALSE,
                                                 DT::datatable({data_flag_desciption},
                                                               rownames = FALSE,
                                                               filter = "top",
                                                               extensions = c("Scroller"), 
                                                               options = list(
                                                                 paging = FALSE,
                                                                 searching = TRUE, 
                                                                 ordering = TRUE, 
                                                                 scrollY = TRUE
                                                               ),
                                                               class = "display"))
  
  # Plots -------------------------------------------------------------------
  output$boxplot <- renderPlotly({
    p <- data() %>%
      
      ggplot(aes(fct_reorder(ANALYTE, RESULT,na.rm = TRUE), RESULT)) +
      geom_boxplot() +
      scale_y_continuous() +
      coord_flip(ylim = c(0,input$number))  +
      
      labs(x = NULL)
    
    ggplotly(p)
  })
  
  # Scatterplots
  # Have to unlist the crosstab data to graph
  
  output$scatter <- renderPlot({
    
    my.formula <- y ~ x
    
    data_cross_tab() %>% 
      ggplot(aes( .data[[input$x_axis]], .data[[input$y_axis]])) +
      geom_point(alpha = input$alpha, size = input$size) +
      geom_smooth(method = 'lm', se = FALSE, formula = my.formula) + 
      stat_poly_eq(formula = my.formula, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      scale_x_continuous() + 
      scale_y_continuous() +
      xlab(input$x_axis) +
      ylab(input$y_axis) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16, face = "bold"))
  }) 
  
  # Outputs table for the scatter chart. 
  #Brushed points allows the highlighting of points on the graph
  
  output$scatter_table <- renderDataTable({
    
    brushedPoints(data_cross_tab(), input$scatter_brush,
                  xvar = input$x_axis, 
                  yvar = input$y_axis)
  })
  
  # Time Series
  
  output$time_series <- renderDygraph({
    dy <-data_cross_tab() %>%
      select(SAMPDATE, input$carbonyl)
    
    dy <-   xts(dy[-1], order.by = dy$SAMPDATE) 
    
    dygraph(dy) %>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3) %>%
      dyRangeSelector() 
    
  })
  
  output$bar_chart <- renderDygraph({ 
    
    dy2 <- data_cross_tab() %>%
      select(SAMPDATE, input$carbonyl)
    
    dy2 <-   xts(dy2[-1], order.by = dy2$SAMPDATE)
    
    dygraph(dy2) %>%
      dyBarChart() %>%
      dyRangeSelector() 
    
  })
  
  output$stacked_bar <- renderDygraph({
    dy3 <- data_cross_tab() %>%
      select(SAMPDATE, input$carbonyl)
    
    dy3 <-   xts(dy3[-1], order.by = dy3$SAMPDATE)
    
    dygraph(dy3) %>%
      dyStackedBarChart() %>%
      dyRangeSelector() 
  })
  
  #Time Series using the data_compare() df to compare primary and secondary
  
  output$compare <- renderDygraph({
    
    dy4 <- data_compare() %>%
      filter(ANALYTE == input$carbonyl2) %>%
      select(SAMPDATE, 'primary','secondary') 
    
    dy4 <-   xts(dy4[-1], order.by = dy4$SAMPDATE)
    
    dygraph(dy4) %>%
      dyOptions(drawPoints = TRUE, pointSize = 3) %>%
      dyBarSeries('secondary') %>%
      dyRangeSelector()
    
  })
  # Fingerprint plots
  # reactive value() is passed into the data()[value()] reactive expression
  # When value() = 1, data is filtered where SAMPDATE == the first unique date in queried data frame
  
  output$bar <- renderPlotly({ 
    p <- data() %>% 
      filter(category == "primary") %>%
      filter(SAMPDATE == date()[value()])%>% 
      ggplot(aes(ANALYTE,RESULT)) +
      geom_col(fill = "deepskyblue") +
      theme(legend.position = "none", axis.text.x = element_text(angle = -90, vjust = 0.5, size = rel(1.8)),
            axis.text.y = element_text(size = rel(1.8))) +
      xlab(NULL) +
      ggtitle(date()[value()])
    
    ggplotly(p)
    
  })
  
  output$scatter_compare <- renderPlot({
    my.formula <- y ~ x
    
    data_carb_voc() %>%
      ggplot(aes(.data[["Benzaldehyde"]],.data[["Toluene"]]))+
      
      geom_point()+
      geom_smooth(method = 'lm', se = FALSE)   +
      stat_poly_eq(formula = my.formula, 
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +
      scale_x_continuous() 
  })
  
  output$box_month <- renderPlotly({
    p <- data_stats() %>%
      mutate(
        current_year = last(YEAR),
        current_record = ifelse(current_year == YEAR, "Current","Historical")) %>%
      
      ggplot(aes(fct_reorder(ANALYTE, RESULT,na.rm = TRUE), RESULT,fill = current_record)) +
      geom_boxplot(position = position_dodge()) +
      scale_y_continuous() +
      coord_flip()  +
      labs(title = glue("{unique(month(as.double(data_stats()$MONTH),label = TRUE, abbr = FALSE))} {unique(last(data_stats()$YEAR))} Historical Comparison"),
           x = NULL)
    
    ggplotly(p) %>%
      layout(boxmode = "group")
    
  })
  
  output$box_year <- renderPlotly({
    p <- data_stats() %>%
      mutate(
        current_year = last(YEAR),
        current_record = ifelse(current_year == YEAR, "Current","Historical")) %>%
      filter(ANALYTE == input$pollutant) %>%
      
      ggplot(aes(YEAR , RESULT)) +
      geom_boxplot( fill = "aquamarine4") + 
      labs(title = glue("{input$carbonyl2} {unique(month(as.double(data_stats()$MONTH),label = TRUE, abbr = FALSE))} Yearly Comparison"))
  })
  
}

# Run the application
shinyApp(ui,server)
