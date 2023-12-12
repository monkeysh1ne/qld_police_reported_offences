#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(DT)

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

off <- unique(agg$off)
x <- unique(agg$year)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("The number of reported offenses by police divisions, Queensland"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # ask user to select year to look at
          shiny::selectInput("thisYear",
                      "Select a year of reported offenses: ",
                      choices = x)
        ),
        mainPanel("My Table",
                DTOutput("table")
        )
      )
    )





server <- shinyServer(function(input, output, session){
  output$table <- renderDT({
    agg[agg$year == input$thisYear,]
  })
})
  



# Run the application 
shinyApp(ui = ui, server = server)
