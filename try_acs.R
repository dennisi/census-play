###### Based on http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/
######
#library(leaflet)  # for interactive maps (NOT leafletR here)
#library(sp)       # for spatial objects
#library(rgdal)    # for readOGR and others
#library(dplyr)    # for working with data frames

#us_counties <- readOGR(dsn="shapefiles", layer = "cb_2014_us_county_500k")

# convert the GEOID to a character
#us_counties@data$GEOID<-as.character(us_counties@data$GEOID)

# Make use of the existing SpatialPolygonsDataFrame
#class(us_counties)


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
enrl_level <- acs.fetch(geo=NeKsIaMo_counties, 
                        table.number="B14001", 
                        endyear = 2013, span = 5, dataset = 'acs')

col.names = make.names( c("Total", 
              "All Enrolled", 
              "Pre-K", 
              "K", 
              "Grades 1-4", 
              "Grades 5-8", 
              "High School", 
              "Undergraduate", 
              "Grad-Professional", 
              "Not Enrolled"))
enrl_level_p <- acs.fetch(geo=NeKsIaMo_counties, table.number="B14001", endyear = 2013, span = 5, dataset = 'acs', col.names = c("Total", "All Enrolled", "Pre-K", "K", "Grades 1-4", "Grades 5-8", "High School", "Undergraduate", "Grad-Professional", "Not Enrolled"))

    # percentages (NOT BEST, see BELOW):
    # enrl_level_p[,8]/enrl_level_p[,1]

# more appropriate division (proportion) calculation:
apply(enrl_level_p[,7:8], MARGIN = 1, FUN = divide.acs, 
      denominator = enrl_level_p[,1], method = "proportion", verbose=F)

# get C14002: School Enrollment by Level of School by Type of School

                    # try: enrl_level@estimate, enrl_level@standard.error
enrl_df <- data.frame(estimate(enrl_level_p), 1.645*standard.error(enrl_level_p))

# clean up column names
enrl_df <- setNames(enrl_df, nm = c("Geo Name", col.names, paste0(col.names, "_err") ))

# write out the data file
write.csv(enrl_df, file="./enrollment_by_level.csv")

# Need to write out GEOID also!!

############
## How to query attributes from an acs.lookup object or an acs class:
## > names(attributes(enrl_level))
## [1] "endyear"        "span"           "acs.units"      "currency.year"  "modified"       "geography"      "acs.colnames"  
## [8] "estimate"       "standard.error" "class"         
## > attr(enrl_level, "acs.colnames")
## [1] "B14001_001" "B14001_002" "B14001_003" "B14001_004" "B14001_005" "B14001_006" "B14001_007" "B14001_008" "B14001_009"
## [10] "B14001_010"
#  > names(attributes(e))
##  [1] "endyear"        "span"           "acs.units"      "currency.year" 
##  [5] "modified"       "geography"      "acs.colnames"   "estimate"      
##  [9] "standard.error" "class"
#  > attr(e, "acs.colnames")
##  [1] "Household Income: Total:"              
##  [2] "Household Income: Less than $10,000"   
##  [3] "Household Income: $10,000 to $14,999"  
##  [4] "Household Income: $15,000 to $19,999"  
##  [5] "Household Income: $20,000 to $24,999"  
##  [6] "Household Income: $25,000 to $29,999"  
##  [7] "Household Income: $30,000 to $34,999"  
##  [8] "Household Income: $35,000 to $39,999"  
##  [9] "Household Income: $40,000 to $44,999"  
## [10] "Household Income: $45,000 to $49,999"  
## [11] "Household Income: $50,000 to $59,999"  
## [12] "Household Income: $60,000 to $74,999"  
## [13] "Household Income: $75,000 to $99,999"  
## [14] "Household Income: $100,000 to $124,999"
## [15] "Household Income: $125,000 to $149,999"
## [16] "Household Income: $150,000 to $199,999"
## [17] "Household Income: $200,000 or more"

# convert to a data.frame for merging
enrl_df2 <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

enrl_df <- select(enrl_df, 1:3)
rownames(enrl_df)<-1:nrow(enrl_df)
names(enrl_df)<-c("GEOID", "total", "over_200")
enrl_df$percent <- 100*(enrl_df$over_200/enrl_df$total) ## WRONG! This should use proper proportioning methods!

##
## 4. Merge (tigris) spacial and tabular data
## 
        income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
        # there are some tracts with no land that we should exclude
        income_merged <- income_merged[income_merged$ALAND>0,]

enrl_merged<- geo_join(NeKsIaMo_counties, enrl_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
enrl_merged <- enrl_merged[enrl_merged$ALAND>0,]

##
## 5. Make your map (leaflet)
##
popup <- paste0("GEOID: ", enrl_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(enrl_merged$percent,2))
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = enrl_merged$percent
)

map3<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = enrl_merged, 
                fillColor = ~pal(percent), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = enrl_merged$percent, 
              position = "bottomright", 
              title = "Percent of Households<br>above $200k",
              labFormat = labelFormat(suffix = "%")) 
map3

## Saving your maps
library(htmlWidgets)
saveWidget(map1, file="map1.html", selfcontained=FALSE)
saveWidget(map2, file="map2.html", selfcontained=FALSE)
saveWidget(map3, file="map3.html", selfcontained=FALSE)
