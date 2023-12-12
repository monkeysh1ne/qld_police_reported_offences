#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TODO: Add 'reset' buttons to all inputSelect UI widgets to show
# [ALL] data.
# Add choropleth map to show all and allow hover-over popups and table.
# Also have RESET button for choropleth map.  Click on function to drill-down
# on specific divisions.



library(shiny)
library(shinyjs)
library(tidyverse)
library(htmlwidgets)
library(DT)

# see btnResetAll UI item.
jsResetCode <- "shinyjs.feckit = function() {history.go(0)}" # Define the js method that resets the page. name cause shiny pitched fit with 'reset' in func name

# read in data from CSV source.
setwd("~/r_dev/Playground/queensland_offenses")
df <- read_csv("data/division_Reported_Offences_Number.csv")

# Wrangle data.
# create the Year col
df$dmy <- dmy(paste("01", df$`Month Year`, sep = ""), tz = "Australia/Brisbane")
df$dmy <- as_date(df$dmy)
df$year <- strftime(df$dmy, "%Y")    # create Year col (char).
df$year <- as.numeric(df$year)
# drop un-needed cols
df <- df %>%
  select(-c(`Month Year`, dmy))

# aggregate data - totals for each offence for each year for each division.
# then transform df placing all offences in col labelled 'offence'.
agg <- df %>%
  group_by(Division, year) %>% 
  summarise(across(everything(), sum)) %>% 
  pivot_longer(-c(Division, year), names_to = "offence", values_to = "off_count" )


# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsResetCode, functions = "feckit"), # Add the js code to the page
    # Application title
    titlePanel("The number of reported offenses by police divisions, Queensland"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        # ask user to select year to look at
        selectInput("selYear",
                    label = "Select a year of reported offenses: ",
                    choices = c("", as.vector(unique(agg$year)))
        ),
        selectInput("selOff",
                    label = "Select an offense category: ",
                    choices = c("", as.vector(unique(agg$offence)))
        ),
        selectInput("selDiv",
                    label = "Select a QLD Police Division: ",
                    choices = c("", as.vector(unique(agg$Division)))
        ),
        tags$br(),
        tags$hr(),
        tags$h4("Click button to reset all filters."),
        actionButton("btnResetAll", "Show All Data", icon("refresh"),
                     class = "btn btn-primary")
      ),
      mainPanel(
        DTOutput("my_table")
      )
    )
  )
)

server <- function(input, output, session) {
  output$my_table <- renderDataTable({
    year_sel <- if (nchar(input$selYear)==0) unique(as.vector(agg$year)) else input$selYear
    off_sel <- if (nchar(input$selOff)==0) unique(as.vector(agg$offence)) else input$selOff
    div_sel <- if (nchar(input$selDiv)==0) unique(as.vector(agg$Division)) else input$selDiv
    filter(agg, year %in% year_sel, offence %in% off_sel, Division %in% div_sel)
  }
)

  observeEvent(input$btnResetAll, {
    js$feckit()
  })
}


# fixed filters - original filtering for DT  
# output$my_table <- renderDataTable({
#   DT::datatable(agg[c(agg$year == input$selYear & agg$offence == input$selOff & agg$Division == input$selDiv),],
#                 class = 'cell-border stripe',
#                 colnames = c('Division', 'Year', 'Offense', '#Instances'),
#                 caption = 'Table 1: source[https://www.police.qld.gov.au/maps-and-statistics].',
#                 options = list(
#                   statesave = FALSE
#                 ))
# })



# Run the application 
shinyApp(ui = ui, server = server)
