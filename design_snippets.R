ui <- fluidPage(
   
   # Application title
   titlePanel("Ramen Reviews"),
   
   # Style Menu
   selectInput("style", "Style", c("All",
                                   "Bowl",
                                   "Box",
                                   "Cup",
                                   "Pack",
                                   "Tray")),
   
	# Bar Chart
	plotOutput("brandBar"),
	   
	# Map
	leafletOutput("map")
   
)


# Create bar chart of brands
output$brandBar <- renderPlot({
    
  # Filter data based on selected Style
  if (input$style != "All") {
    ratings <- filter(ratings, Style == input$style)
  }
  
  # Filter data based on selected Country
if (input$country != "All") {
  ratings <- filter(ratings, Country == input$country)
}


# Map
leafletOutput("map")







# Create world map
output$map <- renderLeaflet({
  
  # Filter data based on selected Style
  if (input$style != "All") {
    ratings <- filter(ratings, Style == input$style)
  }
  
  # Filter data based on selected Country
  if (input$country != "All") {
    ratings <- filter(ratings, Country == input$country)
  }
  
  # Hide map when user has filtered out all data
  validate (
    need(nrow(ratings) > 0, "")
  )
  
  # Get average rating by country
  countries <- group_by(ratings, Country) %>% 
    summarise(avgRating = mean(Stars))
  
  # Add spatial data to countries dataframe
  countries <- left_join(countries, mapData, c("Country" = "name_long"))
  
  # Create color palette for map
  pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgRating)
  
  # Create label text for map
  map_labels <- paste("Ramen from",
                      countries$Country, 
                      "has an average rating of", 
                      round(countries$avgRating, 1))
  
  # Generate basemap
  map <- leaflet() %>%
    addTiles() %>% 
    setView(0, 0, 1)
  
  # Add polygons to map
  map %>% addPolygons(data = countries$geom,
                      fillColor = pal(countries$avgRating),
                      fillOpacity = .7,
                      color = "grey",
                      weight = 1,
                      label = map_labels,
                      labelOptions = labelOptions(textsize = "12px")) %>% 
    
    # Add legend to map
    addLegend(pal = pal, 
              values = countries$avgRating,
              position = "bottomleft")
    
})
