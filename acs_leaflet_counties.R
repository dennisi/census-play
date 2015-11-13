###### Based on http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/
######

## 
## 1) Set up the packages
##

library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames

## 
## 2) Get the spatial data (tigris)
##
counties_shapes <- counties(state=c('NE','KS','IA','MO'), cb=TRUE)

##
## 3) Get the tabular data (acs) 
##     
## make geo of all the counties in the four states
NeKsIaMo_counties <- geo.make(state=c('NE','KS','IA','MO'), county = '*', check = TRUE)
# make geo of the 15 counties in the Service Area

# get total population:
total_pop <- acs.fetch(geo=NeKsIaMo_counties, 
                       table.number="B01003", col.names = "pretty", 
                       endyear = 2013, span = 5, dataset = 'acs')
attr(total_pop, "acs.colnames")

# convert to a data.frame for merging
total_pop_df <- data.frame(paste0(str_pad(total_pop@geography$state, 2, "left", pad="0"), 
                                  str_pad(total_pop@geography$county, 3, "left", pad="0")),
                           total_pop@estimate[,"Total Population:  Total "], 
                           stringsAsFactors = FALSE)

total_pop_df <- select(total_pop_df, 1:2)
rownames(total_pop_df)<-1:nrow(total_pop_df)
names(total_pop_df)<-c("GEOID", "total")
total_pop_df$percent <- 100 #*(total_pop_df$over_200/total_pop_df$total) # WRONG
    # more appropriate division (proportion) calculation:
    #apply(enrl_level_p[,7:8], MARGIN = 1, FUN = divide.acs, 
    #      denominator = enrl_level_p[,1], method = "proportion", verbose=F)


## 
## 4) Do the merge (tigris)
## 
total_pop_merged<- geo_join(counties_shapes, total_pop_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
total_pop_merged <- total_pop_merged[total_pop_merged$ALAND>0,]

## 
## 5) Make your map (leaflet)
##
popup <- paste0("County: ", total_pop_merged$NAME, "<br>", "Population: ", total_pop_merged$total)
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = total_pop_merged$total
)

map1<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = total_pop_merged, 
                fillColor = ~pal(total), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = total_pop_merged$total, 
              position = "bottomright", 
              title = "Total Population",
              labFormat = labelFormat(big.mark = ",")) 
map1
#####################################################

# get B14001: School Enrollment by Level of School
tl = acs.lookup(table.name = "B14001")
cbind(tl@results$variable.code, 
      tl@results$variable.name)

col_names = make.names( c("Total", 
                          "All.Enrolled", 
                          "Pre.K", 
                          "K", 
                          "Grades.1.4", 
                          "Grades.5.8", 
                          "High.School", 
                          "Undergraduate", 
                          "Grad.Professional", 
                          "Not.Enrolled"))

enrl_level <- acs.fetch(geo=NeKsIaMo_counties, 
                        table.number="B14001", col.names = col_names, 
                        endyear = 2013, span = 5, dataset = 'acs')

# convert to a data.frame for merging
enrl_level_df <- data.frame(paste0(str_pad(enrl_level@geography$state, 2, "left", pad="0"), 
                                   str_pad(enrl_level@geography$county, 3, "left", pad="0")), # GEOID
                            # str_pad(enrl_level@geography$state, 2, "left", pad="0"), # State FIPS
                            # str_pad(enrl_level@geography$county, 3, "left", pad="0"), # County FIPS
                            enrl_level@geography$NAME, # County, State
                            enrl_level@estimate[,c("Total",
                                                   "High.School",
                                                   "Undergraduate",
                                                   "Grad.Professional")], 
                            stringsAsFactors = FALSE)

#enrl_level_df <- select(enrl_level_df, 1:2)
rownames(enrl_level_df) <- 1:nrow(enrl_level_df)
names(enrl_level_df) <- c("GEOID",
                          "Geo.Name",
                          "Total",
                          "High.School",
                          "Undergraduate",
                          "Grad.Professional")

enrl_level_df$High.School.percent <- 100 * (enrl_level_df$High.School / enrl_level_df$Total) 
enrl_level_df$Undergraduate.percent <- 100 * (enrl_level_df$Undergraduate / enrl_level_df$Total) 
enrl_level_df$Grad.Professional.percent <- 100 * (enrl_level_df$Grad.Professional / enrl_level_df$Total) 

## 
## 4) Do the merge (tigris)
## 
enrl_level_merged<- geo_join(counties_shapes, enrl_level_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
enrl_level_merged <- enrl_level_merged[enrl_level_merged$ALAND>0,]

## 
## 5) Make your map (leaflet)
##
popup <- paste0(enrl_level_merged$Geo.Name, "<br>", 
                "UG: <b>", round(enrl_level_merged$Undergraduate.percent,1), "%</b> (", enrl_level_merged$Undergraduate, ") <br>", 
                "Population, 3+ Years: ", enrl_level_merged$Total)
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = enrl_level_merged$Undergraduate.percent
)

map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = enrl_level_merged, 
                fillColor = ~pal(Undergraduate.percent), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = enrl_level_merged$Undergraduate.percent, 
              position = "bottomright", 
              title = "%Undergraduates",
              labFormat = labelFormat(suffix = "%"))
map2

### Now color map by Count of Undergraduates
popup <- paste0(enrl_level_merged$Geo.Name, "<br>", 
                "UG: <b>", 
                enrl_level_merged$Undergraduate, "</b> (", 
                round(enrl_level_merged$Undergraduate.percent,1), "%)<br>",
                "Population, 3+ Years: ", enrl_level_merged$Total)
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = enrl_level_merged$Undergraduate
)

map3<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = enrl_level_merged, 
                fillColor = ~pal(Undergraduate), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = enrl_level_merged$Undergraduate, 
              position = "bottomright", 
              title = "Undergraduates",
              labFormat = labelFormat(suffix = "%"))
map3
#####################################################



# get C14002: School Enrollment by Level of School by Type of School

## Saving your maps
library(htmlwidgets)
saveWidget(map1, file="maps/map1-population.html", selfcontained=FALSE)
saveWidget(map2, file="maps/map2-undergraduate-pct.html", selfcontained=FALSE)
saveWidget(map3, file="maps/map3-undergraduate-count.html", selfcontained=FALSE)
