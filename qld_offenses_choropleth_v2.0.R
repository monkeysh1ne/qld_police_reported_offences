# Creates a choropleth map showing all reported police
# offenses in Queensland between 2001 - 2023.
# Displayed across Police Service Divisional Boundaries
# sources:
# Police Service Divisional Boundaries [https://www.data.qld.gov.au/dataset/qps-divisions/resource/fa6a7917-43de-4036-a704-25f545c24093]
# Police Division Reported Offenses Number [https://open-crime-data.s3-ap-southeast-2.amazonaws.com/Crime%20Statistics/division_Reported_Offences_Number.csv]

# This map will eventually become a R Shiny Interactive Map which the user
# can display along with interactive controls for filtering what offenses
# the viewer wishes to focus on or a date period the user wishes to focus on.

# Load libraries
library(tidyverse) # tidy data wrangling
library(vroom) # fast reading/importing of data (csv)
library(sf) # spatial data
library(tigris) # geojoin
library(leaflet) # interactive maps
library(htmlwidgets) # interactive map labels

# set Working Directory
setwd("~/r_dev/Playground/queensland_offenses")


# read in DATA
# FIRST read in the offenses data
offenses <- read_csv("data/division_Reported_Offences_Number.csv")



# wrangle data
# !: There are what look like subcategory headers amongst the
# offenses data, e.g., 'Weapons Act Offences' is the total for 
# 'Unlawful Possess Concealable Firearm' + 'Unlawful Possess Firearm - Other' +
# 'Bomb Possess and/or use of' + 'Possess and/or use other weapons; restricted items' +
# 'Weapons Act Offences - Other'!!
# we will need to either remove these or remove their child categories
# in order to avoid mis-reporting the data.

# remove children entries to simplify the data set.  If we decide to increase
# the granularity of data at a later time we can always to so.


# I've had to manually edit this file to remove child entries
# and saved the file as 'offense_cats_main.txt' in the 'data' folder.
# this is why the following code is commented out.
# ----------START
# create list of all categories using names() function.
# offenses_cats <- names(offenses)
# save this file for later filtering of offenses DT.
# write(offenses_cats, file = "offense_cats.txt")
# ----------END

# we can now use the 'offense_cats_main.txt' file
# to filter out the data in offenses DT that we
# don't need (i.e., the child entries).

offense_cats_main <- read.table("data/offense_cats_main.txt", sep = "\n", header = FALSE)
name_list <- as.list(offense_cats_main)
trimmed_offenses <- offenses[, which((names(offenses) %in% offense_cats_main$V1)==TRUE)]

# create a new column in df to store date as ddmmyy, Date class, formatted values for 1st of month
# for each entry in `Month Year` col.
trimmed_offenses$dmy <- dmy(paste("01", trimmed_offenses$`Month Year`, sep = ""), tz = "Australia/Brisbane")
trimmed_offenses$dmy <- as_date(trimmed_offenses$dmy)

# create working copy of df
working <- trimmed_offenses
rm(trimmed_offenses) # drop the trimmed_offenses df
working$year <- strftime(working$dmy, "%Y")    # create Year col (char).
working$month <- strftime(working$dmy, "%m")   # create Month col (char).
working$year <- as.numeric(working$year)    # makes more sense than character.

# select only the columns we want to work with.
# this will be adjusted later after proof of concept.
# for now it reduces data 'bulk'.
# testing using year=2001 and col(data)=Assault
small_df <- working %>% select(-c(`Month Year`, month, dmy)) #%>% # don't need to filter now as want all data 
  # filter(year == 2022)


# aggregate data by Division and year for ALL reported offenses
working_agg1 <- small_df %>% 
  group_by(Division, year) %>% 
  summarise(across(everything(), sum))
working_agg1




#####################################################################
# SF operations START
#####################################################################
# now we can read in the shape file data for QLD Police Divisions
divs_shp <- st_read("data/QPS_DIVISIONS.shp")
# add Division col to divs_shp sf
divs_shp$Division <- unique(working_agg1$Division)



# .. and merge the offense data with the shape data
qld_offenses_map_data <- inner_join(divs_shp, working_agg1)



# sanity check to see how data is distributed (mostly for color palette but
# also to check I'm getting the data I expect.)
# hist(qld_offenses_map_data$Assault)  # firstly, with Base R
small_df  %>%  ggplot(aes(x = Robbery)) +
  geom_histogram(bins=100, color="blue3", fill="grey90")
# need to convert 'Month Year' col to date format
# qld_offenses_map_data$`Month Year` <- as.Date(qld_offenses_map_data$`Month Year`, format = "%b%y")

# we still need to split the months and the years from 'Month Year'
# column.







# save as RDS for R Shiny App
# comment out for now as not sure I need it!!
saveRDS(qld_offenses_map_data, "qld_offenses_map_data.RDS")

# make interactive map of Assaults only for now
labels <- sprintf(
  "Division: <strong>%s</strong><br/>Offense(Robbery):%i<br/>Year:%i",
  qld_offenses_map_data$Division, qld_offenses_map_data$Robbery, qld_offenses_map_data$year) %>% 
  lapply(htmltools::HTML)

################################
######STOP EXECUTION HERE TO CHECK
######DATA FRAME
################################

# colorbrewer allows us to 'cut' data into arbitrary 
# chunks for more meaningful visual rendering.
# since we are focusing on a particular category of
# crime we need a way to create 'bins' assigned to
# each number of offense (e.g., [DivA] - [Robbery] - 12, [DivB] - [Robbery] - 0 ...)
library(RColorBrewer)
# start by assigning brwer palette to holding var (pal)
# pal <- brewer.pal(9, "YlOrRd")
pal <- colorRamp(c("cornsilk", "darkorange1", "tomato"), bias = 1)
class_of_div <- cut(qld_offenses_map_data$Robbery, 10)
# pal <- pal[as.numeric(class_of_div)]
# pal <- colorNumeric(palette = pal,
                    # domain = qld_offenses_map_data$Robbery)
pal <- colorBin(pal, domain = qld_offenses_map_data$Robbery, 10, pretty = TRUE)




map_interactive <- qld_offenses_map_data %>% 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels,
              stroke = FALSE,
              smoothFactor = 0.5,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = ~pal(Robbery),
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1,
                                                  color = "black",
                                                  opacity = 1,
                                                  bringToFront = TRUE)) %>% 
addLegend("bottomright",
          pal = pal,
          values = ~Robbery,
          title = "Robberies by Police Division, QLD",
          opacity = 0.7)


map_interactive

# to save as a standalone html page incorporating interactivity funcs.
saveWidget(map_interactive, "queensland_offenses_test_map.html")
