# Data manipulations --------------------------------
# Highlight and run command below to load project without munging.
# This is useful when debugging data manipulation code.
# rm(list = ls()); library(ProjectTemplate); load.project(list(munging=FALSE)) 

# Check for duplicate rows in the data
# Note: the check returns 83 duplicates in the data
#sum(duplicated(crime.data))

# Remove duplicate rows in the data
crime.data <- crime.data[!(duplicated(crime.data)),]

# Understand how the data is organized, what fields are
# present, and how they are stored
# glimpse(crime.data) # Display internal structure of the data
# summary(crime.data) # Display a detailed summary of the data

# The summary returns a longitude max. of zero (0) and a
# latitude min. of zero (0). This will need to be corrected.

# Drop latitude and longitude values of 0.00 
# and/or outside the city identified via a check in Tableau
crime.data <- crime.data[crime.data$Longitude != 0.00 | crime.data$Latitude != 0.00,]
crime.data <- crime.data[crime.data$Longitude != -93.540131 | crime.data$Latitude != 44.876759,]

# Additionally, 'Precinct', 'Offense', 'Description' 
# and 'Neighboorhood' should be converted to factors as these
# are categorical variable options. 'Precinct' is loaded
# as a numeric and will need to be convereted to a character.

# Convert categorical variables to factors
crime.data$Precinct <- factor(as.character(crime.data$Precinct))
crime.data$Offense <- factor(crime.data$Offense)
crime.data$Description <- factor(crime.data$Description)
crime.data$Neighborhood <- factor(crime.data$Neighborhood)

# Next, to ensure consistency across columns, 
# 'Time.of.Offense' and 'publicaddress' will be renamed.

# Rename columns for consistency with other columns
names(crime.data)[names(crime.data) == "Time.of.Offense"] <- "OffenseTime"
names(crime.data)[names(crime.data) == "publicaddress"] <- "PublicAddress"

# A glimpse of the data illustrates 'ReportedDate' as containing
# both a date and time. This should be separated.

# Separate 'ReportedDate' into reported date 
# and reported time columns
crime.data <- crime.data %>%
  separate(ReportedDate, c("ReportedDate","ReportedTime"), "T", fill = "right")

# As not all 'ReportedDate' had a 'ReportedTime', we drop
# rows with missing 'ReportedTime' values

# Remove observations with NAs from the data
crime.data <- na.omit(crime.data)

# The summary confirms 'ReportedDate', 'ReportedTime', 
# and 'OffenseTime' as character classes. These are to 
# be convered to date and time classes.

# Convert 'ReportedDate' to a date class
crime.data$ReportedDate <- as.Date(crime.data$ReportedDate)

# Convert 'ReportedTime' to time class
crime.data$ReportedTime <- as.POSIXct(crime.data$ReportedTime, format = "%H:%M:%S", tz = "CST6CDT")
crime.data$ReportedTime <- format(crime.data$ReportedTime, "%H:%M:%S")
crime.data$ReportedTime <- chron(times=crime.data$ReportedTime)

# Convert 'OffenseTime' to a time class
crime.data$OffenseTime <- chron(times=crime.data$OffenseTime)

# To allow for easier insight analysis, we bin
# 'OffenseTime' and 'ReportedTime' into time
# period names.

# Create new columns for offense time period
# and reported time period
crime.data <- transform(crime.data,
                        EstOffenseTimePeriod = cut(OffenseTime,
                                                   breaks = times(c("00:00:00", "06:00:00", 
                                                                    "12:00:00", "18:00:00", "23:59:00")),
                                                   labels = c("Night", "Morning", "Afternoon", "Evening"), 
                                                   include.lowest = TRUE))

crime.data <- transform(crime.data,
                        ReportedTimePeriod = cut(ReportedTime,
                                                 breaks = times(c("00:00:00", "06:00:00", 
                                                                  "12:00:00", "18:00:00", "23:59:00")),
                                                 labels = c("Night", "Morning", "Afternoon", "Evening"), 
                                                 include.lowest = TRUE))

# As there is no date associated with the time of the offense, 
# we persume all offenses were reported within 24-hours of the 
# crime occuring. As such, we estimate the date the offense
# occured using the reported date and place this in a new column.

# Add new column to determine estimated date of offense
crime.data$OffenseDate <- as.Date(crime.data$ReportedDate)

crime.data$OffenseDate <- ifelse(crime.data$OffenseTime > crime.data$ReportedTime,
                                 as.Date(crime.data$OffenseDate) - ddays(1), 
                                 as.Date(crime.data$OffenseDate))

crime.data$OffenseDate <- as.Date(crime.data$OffenseDate, origin = "1970-01-01")

# Create new columns for the EstOffense weekday, month, and year
crime.data$EstOffenseWeekday <- factor(weekdays(crime.data$OffenseDate, abbreviate= FALSE))
crime.data$EstOffenseMonth <- factor(months(crime.data$OffenseDate, abbreviate= FALSE))
crime.data$EstOffenseYear <- factor(year(crime.data$OffenseDate))

# As the time and date data has been cleansed, we now combine
# the separate time and date columns into one column, each respectively
# and convert to a date-time class.

# Add new columns to combine reported/offense dates and times
crime.data <- unite(crime.data, 'ReportedDateTime', 'ReportedDate', 'ReportedTime', sep = " ", remove = FALSE)
crime.data <- unite(crime.data, 'EstOffenseDateTime', 'OffenseDate', 'OffenseTime', sep = " ", remove = FALSE)

# Convert reported/offense date and time column to date-time class
crime.data$EstOffenseDateTime <- as.POSIXct(strptime(crime.data$EstOffenseDateTime, "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
crime.data$ReportedDateTime <- as.POSIXct(strptime(crime.data$ReportedDateTime, "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))

# Add new column to calculate the difference between
# when a crime is estimated to have occured and when
# a crime was reported in hours, minutes, and seconds
td <- seconds_to_period(difftime(crime.data$ReportedDateTime,crime.data$EstOffenseDateTime, units = "secs"))
crime.data$OffenseToReported <- sprintf('%02d:%02d:%02d', td@hour, minute(td), second(td))
rm(td)

# Remove observations with NAs from the data
crime.data <- na.omit(crime.data)

# Convert 'OffenseToReported' to a time class
crime.data$OffenseToReported <- as.POSIXct(crime.data$OffenseToReported, format = "%H:%M:%S", tz = "CST6CDT")
crime.data$OffenseToReported <- format(crime.data$OffenseToReported, "%H:%M:%S")
crime.data$OffenseToReported <- chron(times=crime.data$OffenseToReported)

# Correct mispelled neighborhoods
# Note: Identified by cross-checking the list of neighborhoods in Minneapolis, MN
# Source: http://www.ci.minneapolis.mn.us/ncr/links/index.htm
crime.data$Neighborhood <- str_replace(string = crime.data$Neighborhood, pattern = "King Field Field", replacement = "King Field")
crime.data$Neighborhood <- str_replace(string = crime.data$Neighborhood, pattern = "Ceder - Isles - Dean", replacement = "Cedar - Isles - Dean")

# Overwrite offense column with Minnesota 
# Uniform Crime Reporting Offenses and their Classification
# Source: https://dps.mn.gov/divisions/bca/bca-divisions/mnjis/Documents/2017-Minnesota-Uniform-Crime-Report.pdf
crime.data$Offense <- factor(ifelse(crime.data$Description %in% c("Motor Vehicle Theft", "Other Vehicle Theft"),
                           "Motor Vehicle Theft",
                    ifelse(crime.data$Description %in% c("Burglary Of Business", "Burglary Of Dwelling"),
                           "Burglary",
                    ifelse(crime.data$Description %in% c("Robbery Of Business", "Robbery Of Person","Robbery Per Agg"),
                           "Robbery",
                    ifelse(crime.data$Description %in% c("Bike Theft","On-line Theft", "Theft From Motr Vehc",
                                                   "Other Theft","Scrapping-Recycling Theft",
                                                   "Shoplifting","Theft By Computer","Theft By Swindle",
                                                   "Theft From Building", "Theft From Person", "Theft-motr Veh Parts",
                                                   "Theft/coinop Device","Pocket-picking","Looting","Gas Station Driv-off"),
                           "Larceny - Theft",
                    ifelse(crime.data$Description %in% c("Adulteration/poison"),
                           "Aggravated Assault",
                    ifelse(crime.data$Description %in% c("Disarm a Police Officer"),
                           "Other",
                    ifelse(crime.data$Description %in% c("Arson"),
                           "Arson", crime.data$Description))))))))

# Add new column to define serious crime index
# Source: https://dps.mn.gov/divisions/bca/bca-divisions/mnjis/Documents/2017-Minnesota-Uniform-Crime-Report.pdf
crime.data$CrimeIndex <- factor(ifelse(crime.data$Offense %in% c("Robbery","Aggravated Assault"),
                                       "Violent Crimes",
                                       ifelse(crime.data$Offense %in% c("Burglary","Larceny - Theft", "Arson", "Motor Vehicle Theft"),
                                              "Property Crimes", "Other")))

# Create a trimmed version of the data set retaining only
# the columns needed for analysis and insights
crime.data <- subset(crime.data, select=c(20,8,9,2,12,10,11,6,13,16,17,18,3,14,19))

#######################################
# Review accuracy of assigned precincts
# and update as necessary. This is under
# the persumption longitude and latitude
# values are accurate.
#######################################

precincts <- readOGR("shape","Minneapolis_Police_Precincts")

# Turn the spatial object into a data frame
precincts.df <- fortify(precincts)

# Make sure precinct labels are consistent with the crime data frame
precincts.df$id <- as.numeric(precincts.df$id)
precincts.df$id <- precincts.df$id + 1

sequence <-seq(1,5,1)
for(i in sequence){
  # Filter the data frame such that it only contains data for one precinct
  temp_sp <- filter(precincts.df, id == i)
  
  # Test whether a particular longitude, latitude pair is within a particular precinct
  logical <- as.logical(point.in.polygon(crime.data$Longitude, crime.data$Latitude, temp_sp$long, temp_sp$lat))
  
  # Update the data frame with the correct precinct value
  crime.data$Precinct[logical] <- i
  rm(temp_sp)
}

rm(precincts.df)
rm(precincts)

# Convert categorical variables to factor
# omitting now empty precinct 18
crime.data$Precinct <- factor(as.character(crime.data$Precinct))

#######################################
# Review accuracy of assigned 
# neighboorhoods and update as necessary. 
# This is under the persumption longitude 
# and latitude values are accurate.
#######################################

hoods <- readOGR("shape","Neighborhoods")

# Turn the spatial object into a data frame
hoods.df <- fortify(hoods)

# Make sure neighborhood labels are consistent with the crime data frame
hoods.df$id <- as.numeric(hoods.df$id)
hoods.df$id <- hoods.df$id + 1

id <- hoods@data

new <- crime.data
new[] <- id$OBJECTID[match(unlist(crime.data), id$BDNAME)]

crime.data$NeighborhoodID <- as.factor(new$Neighborhood)

# Rearrange columns for ease of reading
crime.data <- subset(crime.data, select=c(1,2,3,4,16,5,6,7,8,9,10,11,12,13,14,15))

sequence <-seq(1,87,1)
for(i in sequence){
  # Filter the data frame such that it only contains data for one neighborhood
  temp_sp <- filter(hoods.df, id == i)
  
  # Test whether a particular longitude, latitude pair is within a particular neighborhood
  logical <- as.logical(point.in.polygon(crime.data$Longitude, crime.data$Latitude, temp_sp$long, temp_sp$lat))
  
  # Update our subset with the correct precinct value
  crime.data$NeighborhoodID[logical] <- i
  rm(temp_sp)
}

rm(hoods.df)
rm(hoods)

# # 3,428 wrong neighborhood values, according to our calculations
# sum(crime.data$neighborhood_test != crime.data$NeighborhoodID, na.rm =TRUE)

# Remove observations with NAs from the data
crime.data <- na.omit(crime.data)

# Create a lookup table 
lut <- select(id, OBJECTID, BDNAME)

# Update neighborhood names to correct neighborhood based on corrected ID 
crime.data$Neighborhood <- as.factor(lut[match(crime.data$NeighborhoodID, lut$OBJECTID),'BDNAME'])

rm(id, lut, new, i, logical, sequence)

# Create an additional column to condense neighborhoods
# into communities
# Source: https://en.wikipedia.org/wiki/Neighborhoods_of_Minneapolis
crime.data$Community <- factor(ifelse(crime.data$Neighborhood %in% c("Bryn - Mawr", 
                                                                      "CARAG",
                                                                      "Cedar - Isles - Dean",
                                                                      "East Calhoun",
                                                                      "East Isles",
                                                                      "Kenwood",
                                                                      "Lowry Hill",
                                                                      "Lowry Hill East",
                                                                      "West Calhoun",
                                                                      "ECCO"),
                                    "Calhoun-Isles",
                                    ifelse(crime.data$Neighborhood %in% c("Cleveland",
                                                                           "Folwell",
                                                                           "Lind - Bohanon",
                                                                           "McKinley",
                                                                           "Shingle Creek",
                                                                           "Victory",
                                                                           "Webber - Camden",
                                                                           "Camden Industrial",
                                                                           "Humboldt Industrial Area"),
                                           "Camden",
                                           ifelse(crime.data$Neighborhood %in% c("Downtown East",
                                                                                  "Downtown West",
                                                                                  "Elliot Park",
                                                                                  "Loring Park",
                                                                                  "North Loop",
                                                                                  "Steven's Square - Loring Heights"),
                                                  "Central",
                                                  ifelse(crime.data$Neighborhood %in% c("Cooper",
                                                                                         "Hiawatha",
                                                                                         "Howe",
                                                                                         "Longfellow",
                                                                                         "Seward"),
                                                         "Longfellow",
                                                         ifelse(crime.data$Neighborhood %in% c("Harrison",
                                                                                                "Hawthorne",
                                                                                                "Jordan",
                                                                                                "Near - North",
                                                                                                "Sumner - Glenwood",
                                                                                                "Willard - Hay"),
                                                                "Near North",
                                                                ifelse(crime.data$Neighborhood %in% c("Diamond Lake",
                                                                                                       "Ericsson",
                                                                                                       "Field",
                                                                                                       "Hale",
                                                                                                       "Keewaydin",
                                                                                                       "Minnehaha",
                                                                                                       "Morris Park",
                                                                                                       "Northrop",
                                                                                                       "Page",
                                                                                                       "Regina",
                                                                                                       "Wenonah"),
                                                                       "Nokomis",
                                                                       ifelse(crime.data$Neighborhood %in% c(
                                                                         "Audubon Park",
                                                                         "Beltrami",
                                                                         "Bottineau",
                                                                         "Columbia Park",
                                                                         "Holland",
                                                                         "Logan Park",
                                                                         "Marshall Terrace",
                                                                         "Northeast Park",
                                                                         "Sheridan",
                                                                         "St. Anthony East",
                                                                         "St. Anthony West",
                                                                         "Waite Park",
                                                                         "Windom Park"),
                                                                              "Northeast", 
                                                                         ifelse(crime.data$Neighborhood %in% c(
                                                                           "East Phillips",
                                                                           "Midtown Phillips",
                                                                           "Phillips West",
                                                                           "Ventura Village"),
                                                                           "Phillips", 
                                                                           ifelse(crime.data$Neighborhood %in% c(
                                                                             "Bancroft",
                                                                             "Bryant",
                                                                             "Central",
                                                                             "Corcoran",
                                                                             "Lyndale",
                                                                             "Powderhorn Park",
                                                                             "Standish",
                                                                             "Whittier"),
                                                                             "Powderhorn", 
                                                                             ifelse(crime.data$Neighborhood %in% c(
                                                                               "Armatage",
                                                                               "East Harriet",
                                                                               "Fulton",
                                                                               "Kenny",
                                                                               "King Field",
                                                                               "Linden Hills",
                                                                               "Lynnhurst",
                                                                               "Tangletown",
                                                                               "Windom"),
                                                                               "Southwest", 
                                                                               ifelse(crime.data$Neighborhood %in% c(
                                                                                 "Cedar Riverside",
                                                                                 "Como",
                                                                                 "Marcy Holmes",
                                                                                 "Nicollet Island - East Bank",
                                                                                 "Prospect Park",
                                                                                 "Prospect Park - East River Road",
                                                                                 "University of Minnesota",
                                                                                 "Mid - City Industrial"),
                                                                                 "University", 
                                                                                 crime.data$Neighborhood))))))))))))

# Output a cleansed version of the data frame to
# the project folder
write.csv(crime.data,'output/crime.data.clean.csv')