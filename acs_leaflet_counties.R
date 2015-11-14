###### Based on http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/
######

##################################################
##                                              ## 
## 1) Set up the packages                       ##
##                                              ##
##################################################
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames

##################################################
##                                              ## 
## 2) Get the spatial data (tigris)             ##
##                                              ##
##################################################

counties_shapes <- counties(state=c('NE','KS','IA','MO'), cb=TRUE)
## make geo of all the counties in the four states (for acs query)
NeKsIaMo_counties <- geo.make(state=c('NE','KS','IA','MO'), county = '*', check = TRUE)

##################################################
##                                              ## 
## 3) Query the ACS and build the tabular data  ##
##                                              ##
##################################################
# todo: make geo of the 15 counties in the Service Area

# Query B01003: "Total Population"
# --------------------------------
# tl = acs.lookup(table.name = "B01003")
# cbind(tl@results$variable.code, 
#       tl@results$variable.name)
#   [1,] "B01003_001" " Total "

total_pop <- acs.fetch(geo=NeKsIaMo_counties, 
                       table.number="B01003", col.names = "Total.Population", 
                       endyear = 2013, span = 5, dataset = 'acs')

# Query B14001: "School Enrollment by Level of School"
# --------------------------------------------------
#tl = acs.lookup(table.name = "B14001")
#cbind(tl@results$variable.code, 
#      tl@results$variable.name)
    # [1,] "B14001_001" " Total: "                                                      
    # [2,] "B14001_002" " Enrolled in school: "                                         
    # [3,] "B14001_003" " Enrolled in school: Enrolled in nursery school, preschool "   
    # [4,] "B14001_004" " Enrolled in school: Enrolled in kindergarten "                
    # [5,] "B14001_005" " Enrolled in school: Enrolled in grade 1 to grade 4 "          
    # [6,] "B14001_006" " Enrolled in school: Enrolled in grade 5 to grade 8 "          
    # [7,] "B14001_007" " Enrolled in school: Enrolled in grade 9 to grade 12 "         
    # [8,] "B14001_008" " Enrolled in school: Enrolled in college, undergraduate years "
    # [9,] "B14001_009" " Enrolled in school: Graduate or professional school "         
    # [10,] "B14001_010" " Not enrolled in school " 
col_names = make.names( c("Total.for.Enrollment", 
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

# Query B02001: "Race"
# --------------------------------------------------
# tl <- acs.lookup(table.name = "B02001") # or table.number?
# cbind(tl@results$variable.code, 
#       tl@results$variable.name)
    # [1,] "B02001_001" " Total: "                                                                         
    # [2,] "B02001_002" " White alone "                                                                    
    # [3,] "B02001_003" " Black or African American alone "                                                
    # [4,] "B02001_004" " American Indian and Alaska Native alone "                                        
    # [5,] "B02001_005" " Asian alone "                                                                    
    # [6,] "B02001_006" " Native Hawaiian and Other Pacific Islander alone "                               
    # [7,] "B02001_007" " Some other race alone "                                                          
    # [8,] "B02001_008" " Two or more races: "                                                             
    # [9,] "B02001_009" " Two or more races: Two races including Some other race "                         
    # [10,] "B02001_010" " Two or more races: Two races excluding Some other race, and three or more races "
col_names = make.names( c("Race.Total.check", 
                          "White", 
                          "Black", 
                          "AmIndian", 
                          "Asian", 
                          "Hawaiian", 
                          "Race.Other", 
                          "Race.Multiple", 
                          "Race.Multiple2", 
                          "Race.Multiple3"))
race_data <- acs.fetch(geo=NeKsIaMo_counties, 
                       table.number="B02001", col.names = col_names, 
                       endyear = 2013, span = 5, dataset = 'acs')

# Query B03003: "Hispanic or Latino Origin"
# --------------------------------------------------
# tl <- acs.lookup(table.name = "B03003") # or table.number?
# cbind(tl@results$variable.code, 
#       tl@results$variable.name)
    # [1,] "B03003_001" " Total: "                
    # [2,] "B03003_002" " Not Hispanic or Latino "
    # [3,] "B03003_003" " Hispanic or Latino " 
col_names = make.names( c("Hispanic.Total.delete", 
                          "Non.Hispanic", 
                          "Hispanic"))
hispanic_data <- acs.fetch(geo=NeKsIaMo_counties, 
                       table.number="B03003", col.names = col_names, 
                       endyear = 2013, span = 5, dataset = 'acs')


##################################################
##                                              ## 
## 4) Merge the data frames with geo            ##
##                                              ##
##################################################

# convert to a data.frame for merging
acs_df <- data.frame(paste0(str_pad(total_pop@geography$state, 2, "left", pad="0"), 
                            str_pad(total_pop@geography$county, 3, "left", pad="0")),
                     enrl_level@geography$NAME, # County, State
                     total_pop@estimate[, "Total.Population"], 
                     enrl_level@estimate[, c("Total.for.Enrollment",
                                            "High.School",
                                            "Undergraduate",
                                            "Grad.Professional")], 
                     race_data@estimate[, c("Race.Total.check", 
                                           "White", 
                                           "Black", 
                                           "AmIndian", 
                                           "Asian", 
                                           "Hawaiian", 
                                           "Race.Other", 
                                           "Race.Multiple")],
                     hispanic_data@estimate[, c("Non.Hispanic", 
                                                "Hispanic")],
                     stringsAsFactors = FALSE)

#acs_df <- select(acs_df, 1:2)
#rownames(acs_df) <- 1:nrow(acs_df)
names(acs_df)[1:3] <- c("GEOID", 
                   "Geo.Name",
                   "Total.Population")

acs_df$High.School.percent <- 100 * (acs_df$High.School / acs_df$Total.for.Enrollment) 
acs_df$Undergraduate.percent <- 100 * (acs_df$Undergraduate / acs_df$Total.for.Enrollment) 
acs_df$Grad.Professional.percent <- 100 * (acs_df$Grad.Professional / acs_df$Total.for.Enrollment)

##  Merge the shapes (tigris)
##    (will create a 'Large SpatialPolygonsDataFrame)
acs_merged <- geo_join(counties_shapes, acs_df, "GEOID", "GEOID")
acs_merged <- acs_merged[acs_merged$ALAND>0,] # there are some tracts with no land to exclude

##################################################
##                                              ## 
## 5) Build maps with (leaflet)                 ##
##                                              ##
##################################################
## TODO: How to add these as map layers?

# map1: Total population by county

popup <- paste0(acs_merged$Geo.Name, "<br>", 
                "Total Population: ", acs_merged$Total.Population)

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = acs_merged$Total.Population
)

map1 <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = acs_merged, 
                fillColor = ~pal(Total.Population), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = acs_merged$Total.Population, 
              position = "bottomright", 
              title = "Total Population",
              labFormat = labelFormat(big.mark = ","))
map1


# map2: Counties by Undergraduate students as percentage of population (3+ years old)

popup <- paste0(acs_merged$Geo.Name, "<br>", 
                "UG: <b>", round(acs_merged$Undergraduate.percent,1), "%</b> (", acs_merged$Undergraduate, ") <br>", 
                "Population, 3+ Years: ", acs_merged$Total.for.Enrollment)

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = acs_merged$Undergraduate.percent
)

map2 <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = acs_merged, 
                fillColor = ~pal(Undergraduate.percent), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = acs_merged$Undergraduate.percent, 
              position = "bottomright", 
              title = "%Undergraduates",
              labFormat = labelFormat(suffix = "%"))
map2

# map 3: Counties by count of Undergraduate students 

popup <- paste0(acs_merged$Geo.Name, "<br>", 
                "UG: <b>", 
                acs_merged$Undergraduate, "</b> (", 
                round(acs_merged$Undergraduate.percent,1), "%)<br>", 
                "Population, 3+ Years: ", acs_merged$Total.for.Enrollment)

pal <- colorNumeric(
    palette = "YlGnBu",
    domain = acs_merged$Undergraduate
)

map3 <-  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = acs_merged, 
                fillColor = ~pal(Undergraduate), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = acs_merged$Undergraduate, 
              position = "bottomright", 
              title = "Undergraduates",
              labFormat = labelFormat(big.mark = ","))
map3

#####################################################

# write out the data file
#write.csv(enrl_level_df, file="./enrollment_by_level.csv")


 


# Get B15002: "Sex by Educational Attainment for the Population 25 Years and over"
tl <- acs.lookup(table.name = "B15002") # or table.number?
cbind(tl@results$variable.code, 
      tl@results$variable.name)
    #     [1,] "B15002_001" " Total: "                                           
    #     [2,] "B15002_002" " Male: "                                            
    #     [3,] "B15002_003" " Male: No schooling completed "                     
    #     [4,] "B15002_004" " Male: Nursery to 4th grade "                       
    #     [5,] "B15002_005" " Male: 5th and 6th grade "                          
    #     [6,] "B15002_006" " Male: 7th and 8th grade "                          
    #     [7,] "B15002_007" " Male: 9th grade "                                  
    #     [8,] "B15002_008" " Male: 10th grade "                                 
    #     [9,] "B15002_009" " Male: 11th grade "                                 
    #     [10,] "B15002_010" " Male: 12th grade, no diploma "                     
    #     [11,] "B15002_011" " Male: High school graduate, GED, or alternative "  
    #     [12,] "B15002_012" " Male: Some college, less than 1 year "             
    #     [13,] "B15002_013" " Male: Some college, 1 or more years, no degree "   
    #     [14,] "B15002_014" " Male: Associate's degree "                         
    #     [15,] "B15002_015" " Male: Bachelor's degree "                          
    #     [16,] "B15002_016" " Male: Master's degree "                            
    #     [17,] "B15002_017" " Male: Professional school degree "                 
    #     [18,] "B15002_018" " Male: Doctorate degree "                           
    #     [19,] "B15002_019" " Female: "                                          
    #     [20,] "B15002_020" " Female: No schooling completed "                   
    #     [21,] "B15002_021" " Female: Nursery to 4th grade "                     
    #     [22,] "B15002_022" " Female: 5th and 6th grade "                        
    #     [23,] "B15002_023" " Female: 7th and 8th grade "                        
    #     [24,] "B15002_024" " Female: 9th grade "                                
    #     [25,] "B15002_025" " Female: 10th grade "                               
    #     [26,] "B15002_026" " Female: 11th grade "                               
    #     [27,] "B15002_027" " Female: 12th grade, no diploma "                   
    #     [28,] "B15002_028" " Female: High school graduate, GED, or alternative "
    #     [29,] "B15002_029" " Female: Some college, less than 1 year "           
    #     [30,] "B15002_030" " Female: Some college, 1 or more years, no degree " 
    #     [31,] "B15002_031" " Female: Associate's degree "                       
    #     [32,] "B15002_032" " Female: Bachelor's degree "                        
    #     [33,] "B15002_033" " Female: Master's degree "                          
    #     [34,] "B15002_034" " Female: Professional school degree "               
    #     [35,] "B15002_035" " Female: Doctorate degree "        


# B19049: Median Household Income by Age of Householder
tl <- acs.lookup(table.name = "B19049") # or table.number?
cbind(tl@results$variable.code, 
      tl@results$variable.name)
# [1,] "B19049_000.5" " Median household income in the past 12 months (in 2011 inflation-adjusted dollars) -- "                              
# [2,] "B19049_001"   " Median household income in the past 12 months (in 2010 inflation-adjusted dollars) -- Total: "                       
# [3,] "B19049_002"   " Median household income in the past 12 months (in 2010 inflation-adjusted dollars) -- Householder under 25 years "   
# [4,] "B19049_003"   " Median household income in the past 12 months (in 2010 inflation-adjusted dollars) -- Householder 25 to 44 years "   
# [5,] "B19049_004"   " Median household income in the past 12 months (in 2011 inflation-adjusted dollars) -- Householder 45 to 64 years "   
# [6,] "B19049_005"   " Median household income in the past 12 months (in 2011 inflation-adjusted dollars) -- Householder 65 years and over "


# B20004: "Median Earnings by Sex by Educational Attainment for the Population 25 Years and Over"
tl <- acs.lookup(table.name = "B20004") # or table.number?
cbind(tl@results$variable.code, 
      tl@results$variable.name)
    #     [1,] "B20004_001" " Total: "                                                    
    #     [2,] "B20004_002" " Total: Less than high school graduate "                     
    #     [3,] "B20004_003" " Total: High school graduate (includes equivalency) "        
    #     [4,] "B20004_004" " Total: Some college or associate's degree "                 
    #     [5,] "B20004_005" " Total: Bachelor's degree "                                  
    #     [6,] "B20004_006" " Total: Graduate or professional degree "                    
    #     [7,] "B20004_007" " Total: Male: "                                              
    #     [8,] "B20004_008" " Total: Male: Less than high school graduate "               
    #     [9,] "B20004_009" " Total: Male: High school graduate (includes equivalency) "  
    #     [10,] "B20004_010" " Total: Male: Some college or associate's degree "           
    #     [11,] "B20004_011" " Total: Male: Bachelor's degree "                            
    #     [12,] "B20004_012" " Total: Male: Graduate or professional degree "              
    #     [13,] "B20004_013" " Total: Female: "                                            
    #     [14,] "B20004_014" " Total: Female: Less than high school graduate "             
    #     [15,] "B20004_015" " Total: Female: High school graduate (includes equivalency) "
    #     [16,] "B20004_016" " Total: Female: Some college or associate's degree "         
    #     [17,] "B20004_017" " Total: Female: Bachelor's degree "                          
    #     [18,] "B20004_018" " Total: Female: Graduate or professional degree "    


# get C14002: School Enrollment by Level of School by Type of School
# B14005: Sex By School Enrollment By Educational Attainment By Employment Status For The Population 16 To 19 Years
## Saving your maps
library(htmlwidgets)
saveWidget(map1, file="maps/map1-population.html", selfcontained=FALSE)
saveWidget(map2, file="maps/map2-undergraduate-pct.html", selfcontained=FALSE)
saveWidget(map3, file="maps/map3-undergraduate-count.html", selfcontained=FALSE)
