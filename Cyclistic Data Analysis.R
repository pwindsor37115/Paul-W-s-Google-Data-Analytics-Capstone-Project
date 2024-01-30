#--------------------------Prepare-------------------------------------------

# Install Packages
install.packages("tidyverse")
install.packages("leaflet")
install.packages("dplyr")
install.packages("magrittr")
install.packages("readr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("geosphere")
install.packages("sf")
install.packages("maps")
install.packages("lubridate")
install.packages("viridis")

# Load libraries 
library("tidyverse")
library("leaflet")
library("dplyr")
library("magrittr")
library("readr")
library("ggplot2")
library("stringr")
library("geosphere")
library("sf")
library("maps")
library("lubridate")
library("scales")
library("viridis")


# This will combine all of the monthly csv files into one file for 2022 into one dataframe. 
# It will also throw an error if it cannot join the files and will give a read-out of each file showing if there
# were differences found in column names, column types, missing or extra columns etc. No errors were found.
Data_Combined_2022 <- list.files(path = "C:/Users/paulw/Desktop/Google Data Analyst Cert/Cyclistic Data 2022",
                                 pattern="*.csv", 
                                 full.names = T) %>% 
                                 map_df(~read_csv(.))



#--------------------------Process-------------------------------------------


#Get a summary of what columns are present, what kind of data types are in the columns,
#and a preview of the new dataframe with str() and View()
str(Data_Combined_2022)
View(Data_Combined_2022)
summary(Data_Combined_2022)


# clean any leading and trailing spaces that may be before or after text in columns that have strings
Columns_with_strings = c("ride_id", "rideable_type", "start_station_name", "start_station_id", "end_station_name", "end_station_id", "member_casual")
Data_Combined_2022[Columns_with_strings] <- lapply(Data_Combined_2022[Columns_with_strings],trimws)


#There is a ride_id column to identify individual trips. Checking if the values are all unique by comparing the 
#row count to the distinct/unique count of ride_id's
Distinct_ride_ids <- n_distinct(Data_Combined_2022$ride_id)
Total_Rows_2022 <- nrow(Data_Combined_2022)


#Identify the columns in the dataframe that have a NA value.  Are these expected to occasionally have an NA value?
AnyColsNA <- colnames(Data_Combined_2022[colSums(is.na(Data_Combined_2022))>1])


#Return a data frame that has any rows that contain a NA value.
AnyRowsNA <- Data_Combined_2022[rowSums(is.na(Data_Combined_2022))>1,]


#Total Rows in Data_Combined_2022 = 5,667,717    Total Rows where a column has NA = 1,298,357
# Roughly 23% of the rows for 2022 have an NA in at least one column. Normally would 
#get with someone to figure out why almost a quarter of the data is unusable....
Total_Rows_2022 <- nrow(Data_Combined_2022)
Total_Rows_wNA_2022 <- nrow(AnyRowsNA)


#Dropping rows from the dataframe that have NA values and renaming the dataframe
Cleaned_Data_2022 <- Data_Combined_2022[rowSums(is.na(Data_Combined_2022))==0,] 


#Because R studio freezes when trying to plot the points to a map, I used Tableau to take a sneak peak at the locations
# Most of the lat and longitude locations are in Chicago but some are outside of chicago and are in areas such as Canada and 
# at a location of 0 latitude and 0 longitude.  Below is a method to see if there are points that are outside of the 
# range we are expecting

(checking_start_plot <- ggplot(Cleaned_Data_2022,aes(x = start_lat, y = start_lng)) + geom_point())
(checking_end_plot <- ggplot(Cleaned_Data_2022,aes(x = end_lat, y = end_lng)) + geom_point())


#Going online and looking up each of these points in this dataframe will show they are not in Chicago  
Not_Chicago_Locations <- subset(Cleaned_Data_2022, start_lat < 40 | start_lng > -75 | end_lat < 40 | end_lng > -75)


#Removing the rows that have ride data outside the Chicago area
Cleaned_Data_2022 <- subset(Cleaned_Data_2022, (start_lat < 43))
Cleaned_Data_2022 <- subset(Cleaned_Data_2022, (end_lng != 0))

#The columns started_at and ended_at have timestamps for each bike trip.  
#Are there rows where the start time is greater than the end time?  Yes there are 69 rows where this happens
#and these rows will give a negative value when calculating the ride time. These also have to be removed from the data
AnyRows_Invalid_Travel_Time<- subset(Cleaned_Data_2022, started_at > ended_at)
Cleaned_Data_2022 <- subset(Cleaned_Data_2022,started_at <= ended_at)#& started_at = ended_at)


#Remove any rows where the started_at time is incomplete.  
#There are rows where there is a date in the column but not a time stamp.
#These are being removed also. There is a total of 21 rows where this happens
#There is no data dictionary provided by the project but after careful examination, 
#the date time fields should have values with a length of 19.  Filtering out lengths less than this
#will remove incomplete start times.
Incomplete_Start_Time <- Cleaned_Data_2022[nchar(as.character(Cleaned_Data_2022$started_at)) < 19, ]
Cleaned_Data_2022 <- subset(Cleaned_Data_2022, nchar(as.character(started_at)) == 19)

#removing incomplete ending times, there are 20 rows of these as well
Incomplete_End_Time <-Cleaned_Data_2022[nchar(as.character(Cleaned_Data_2022$ended_at)) < 19, ]
Cleaned_Data_2022 <- subset(Cleaned_Data_2022, nchar(as.character(ended_at)) == 19)

#stopping to write the data to a csv file to be used in Tableau Public. Cannot add anymore columns to what is uploaded to Tableau Public b/c it will put the data frame over the size limit that Tableau Public has
Cleaned_Data_Path <- "C:/Users/paulw/Desktop/Google Data Analyst Cert/Cyclistic Data 2022 Cleaned/"
Cleaned_Data_2022_Filename <- "Cleaned_Data_2022.csv"
write.csv(Cleaned_Data_2022,paste0(Cleaned_Data_Path,Cleaned_Data_2022_Filename), row.names = FALSE)


# Add a column ride_time_minutes that is the difference in start and end times
Cleaned_Data_2022 <- Cleaned_Data_2022 %>% mutate(ride_time_minutes = difftime(ended_at,started_at,units="mins")) #get the difference and convert to minutes
Cleaned_Data_2022$ride_time_minutes <- as.numeric(gsub(" mins", "", Cleaned_Data_2022$ride_time_minutes))         #reformat as a number

#Add a column that has the hour the ride started, the day it started on, and the month
Cleaned_Data_2022$started_at_hour <- hour(Cleaned_Data_2022$started_at)

Cleaned_Data_2022$hour_am_pm <- ifelse(
  Cleaned_Data_2022$started_at_hour == 0, "12 AM",
  ifelse(
    Cleaned_Data_2022$started_at_hour == 12, "12 PM",
    ifelse(
      Cleaned_Data_2022$started_at_hour < 12,
      paste0(Cleaned_Data_2022$started_at_hour, " AM"),
      paste0(Cleaned_Data_2022$started_at_hour - 12, " PM")
    )
  )
)

Cleaned_Data_2022$start_day_of_week <- weekdays(Cleaned_Data_2022$started_at)
Cleaned_Data_2022$start_month <- month(Cleaned_Data_2022$started_at, label = TRUE, abbr = FALSE)
#View(unique(Cleaned_Data_2022[, c('hour_am_pm', 'started_at_hour')]))

# Add a ride_distance_feet column that is the distance between starting and ending stations.
# distGeo will return the distance between two geo locations in meters .  
# Here is the link to reference this:  https://rdrr.io/cran/geosphere/man/distGeo.html
Cleaned_Data_2022 <- Cleaned_Data_2022 %>% mutate(ride_distance_feet = distGeo(matrix(c(start_lng, start_lat), ncol=2),matrix(c(end_lng, end_lat), ncol=2))*3.2808399)



#--------------------------Analyze-------------------------------------------

# Creating the Trips By Month Grouped Bar Chart
Summarized_MonthlyTrips <- Cleaned_Data_2022 %>%
  group_by(start_month, member_casual) %>%
  summarize(total_rides = n_distinct(ride_id))


ggplot(Summarized_MonthlyTrips, aes(x = start_month, y = total_rides, fill = member_casual)) +
  #geom_bar(stat = "identity", position = "dodge") +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = .7) +
  labs(title = "Total Trips by Month",
       x = "Month", y = "Total Trips") +
  scale_fill_manual(values = c("#f56580", "#57606c"))+ # Colors for member and casual
  scale_y_continuous(labels = label_number_si())+  # Use SI unit suffixes (K, M, etc.)
  theme(plot.title = element_text(hjust=0.5))


# Creating the Weekday Trips Grouped Bar Chart
Summarized_WeekdayTrips <- Cleaned_Data_2022 %>%
  group_by(start_day_of_week, member_casual) %>%
  summarize(total_rides = n_distinct(ride_id))

# Arrange the dataframe where the days of the week are in order
Summarized_WeekdayTrips$start_day_of_week <- factor(Summarized_WeekdayTrips$start_day_of_week,
                                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

ggplot(Summarized_WeekdayTrips, aes(x = start_day_of_week, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = .7) +
  labs(title = "Weekday Trips",
       x = "Day of the Week", y = "Total Trips") +
  scale_fill_manual(values = c("#f56580", "#57606c"))+ # Colors for member and casual
  scale_y_continuous(labels = label_number_si())+  # Use SI unit suffixes (K, M, etc.)
  #scale_y_continuous(labels = scales::number_format())  # Use regular numeric formatting
  #theme(plot.title = element_text(hjust=0.5), plot.margin = margin(t=20))
  theme(plot.title = element_text(hjust=0.5))


# Creating the Trips By the Hour Grouped Bar Chart
#    Create a summarized data frame with the total count of distinct 'ride_id' for each hour and 'member_casual'
summarized_data_hour <- Cleaned_Data_2022 %>%
  group_by(hour_am_pm, member_casual) %>%
  summarize(total_rides = n_distinct(ride_id))

summarized_data_hour$hour_am_pm <- factor(summarized_data_hour$hour_am_pm, 
                                          levels = c("1 AM", "2 AM", "3 AM", 
                                                     "4 AM", "5 AM", "6 AM", "7 AM", 
                                                     "8 AM", "9 AM", "10 AM", "11 AM", 
                                                     "12 PM", "1 PM", "2 PM", "3 PM", 
                                                     "4 PM", "5 PM", "6 PM", "7 PM", 
                                                     "8 PM", "9 PM", "10 PM", "11 PM", "12 AM"))

ggplot(summarized_data_hour, aes(x = hour_am_pm, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(member_casual ~ ., scales = "free_y") +
  labs(title = "Trips By The Hour",
       x = "Hour of the Day", y = "Total Trips") +
  scale_fill_manual(values = c("#f56580", "#57606c")) +  # Colors for member and casual
  scale_y_continuous(labels = scales::label_number_si()) +  # Use SI unit suffixes (K, M, etc.)
  theme(plot.title = element_text(hjust = 0.5), 
        strip.text.y = element_blank(),  # Remove text labels on the y-axis facets
        strip.background.y = element_blank())  # Remove background for y-axis facets


# creating the Time Spent summary table.  
Time_Spent_Summary <- Cleaned_Data_2022 %>%                #Summarize the travel time by members and casual riders
  group_by(member_casual) %>%
  summarise(
    `Total Minutes Spent` = sum(ride_time_minutes),
    `Max. Minutes Spent` = max(ride_time_minutes),
    `Avg. Minutes Spent` = mean(ride_time_minutes),
    `Median Minutes Spent` = median(ride_time_minutes),
    `Min. Minutes Spent` = min(ride_time_minutes)
  ) 
Time_Spent_Summary <- t(Time_Spent_Summary)                #Swap the rows for columns and vice versa
Time_Spent_Summary <- as.data.frame(Time_Spent_Summary)    #Convert the matrix to a data frame, to rename columns
Time_Spent_Summary <- Time_Spent_Summary[-1, ]             #remove first row,it's not needed
colnames(Time_Spent_Summary) <- c('casual', 'member')      #change the column names, do not have to rename first column b/c it has become the index
Time_Spent_Summary$casual <- prettyNum(Time_Spent_Summary$casual, big.mark = ",") #convert columns to numeric, add comma separator for numbers over 999
Time_Spent_Summary$member <- prettyNum(Time_Spent_Summary$member, big.mark = ",")
View(Time_Spent_Summary)  #a viewer window can be done but does not help much. creating a tab with the data frame using View() looks better

# creating the Distance Traveled summary table.  
Distance_Traveled_Summary <- Cleaned_Data_2022 %>%                       #Summarize the distance traveled by members and casual riders
  group_by(member_casual) %>%
  summarise(
    `Total Distance Traveled` = sum(ride_distance_feet),
    `Max. Ride Distance Feet` = max(ride_distance_feet),
    `Avg. Ride Distance Feet` = mean(ride_distance_feet),
    `Median Ride Distance Feet` = median(ride_distance_feet),
    `Min. Ride Distance Feet` = min(ride_distance_feet)
  ) 
Distance_Traveled_Summary <- t(Distance_Traveled_Summary)                #Swap the rows for columns and vice versa
Distance_Traveled_Summary <- as.data.frame(Distance_Traveled_Summary)    #Convert the matrix to a data frame, to rename columns
Distance_Traveled_Summary <- Distance_Traveled_Summary[-1, ]             #remove first row,it's not needed
colnames(Distance_Traveled_Summary) <- c('casual', 'member')             #change the column names
Distance_Traveled_Summary$casual <- prettyNum(Distance_Traveled_Summary$casual, big.mark = ",") #convert columns to numeric, add comma separator for numbers over 999
Distance_Traveled_Summary$member <- prettyNum(Distance_Traveled_Summary$member, big.mark = ",")
View(Distance_Traveled_Summary)


#Before creating visuals such as top ten stations by total rides for members and casual riders, will need to split the data into two subsets
member_data <- subset(Cleaned_Data_2022, member_casual == "member")
casual_data <- subset(Cleaned_Data_2022, member_casual == "casual")



# Rideable Types - Members only pie chart
member_data %>%
  count(rideable_type) %>%
  ggplot(aes(x = "", y = n, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Rideable Types - Member") +
  scale_fill_manual(values = c("electric_bike" = "#b3b7b8", "classic_bike" = "#57606c", "docked_bike" = "#CCCCCC")) +
  geom_text(aes(label = paste0(rideable_type, "\n", scales::percent(n / sum(n)))), position = position_stack(vjust = 0.5))


# Rideable Types - Casual only pie chart
casual_data %>%
  count(rideable_type) %>%
  ggplot(aes(x = "", y = n, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("Rideable Types - Casual") +
  scale_fill_manual(values = c("electric_bike" = "#ffbed1", "classic_bike" = "#f56580", "docked_bike" = "#f498b6")) +
  geom_text(aes(label = paste0(rideable_type, "\n", scales::percent(n / sum(n)))), position = position_stack(vjust = 0.5))



# Top Ten Starting Station Names by total trip count - Members Only
Top_Ten_Start_Stations_Members <- member_data %>%   #  assuming you already created this dataset:  member_data <- subset(Cleaned_Data_2022, member_casual == "member")
  group_by(start_station_name) %>%
  summarize(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 10)

Top_Ten_Start_Stations_Members <- as.data.frame(Top_Ten_Start_Stations_Members)    
Top_Ten_Start_Stations_Members$total_trips <- prettyNum(Top_Ten_Start_Stations_Members$total_trips, big.mark = ",") 
View(Top_Ten_Start_Stations_Members)

# Top Ten Starting Station Names by total trip count - Casual Only
Top_Ten_Start_Stations_Casual <- casual_data %>%   #  assuming you already created this data subset:  casual_data <- subset(Cleaned_Data_2022, member_casual == "casual")
  group_by(start_station_name) %>%
  summarize(total_trips = n()) %>%
  arrange(desc(total_trips)) %>%
  slice_head(n = 10)

Top_Ten_Start_Stations_Casual <- as.data.frame(Top_Ten_Start_Stations_Casual)    
Top_Ten_Start_Stations_Casual$total_trips <- prettyNum(Top_Ten_Start_Stations_Casual$total_trips, big.mark = ",") 
View(Top_Ten_Start_Stations_Casual)


#test_data <- Cleaned_Data_2022 %>% slice(1:100000)  

# Map plot of Starting Stations and Total Trips - Casual Riders only .  
casual_ride_counts <- Cleaned_Data_2022 %>%                    
  filter(member_casual == "casual") %>%         #summarise the totals by casual riders only
  group_by(start_lat, start_lng) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides))

casual_map_color_pal <- colorNumeric(          # Set up the color palette
  #palette = c("#ff00cc", "#ff0033"),
  palette = c("#ffACBD", "#ff0033"),
  domain = casual_ride_counts$total_rides
)

Casual_Map <- leaflet(casual_ride_counts) %>%
  addProviderTiles("CartoDB.Voyager") %>%      # Create the leaflet map.  These are free popular maps to use:  CartoDB.Positron, CartoDB.DarkMatter and CartoDB.Voyager
  addCircles(
    lat = casual_ride_counts$start_lat,
    lng = casual_ride_counts$start_lng,
    color = ~casual_map_color_pal(total_rides),
    fillColor = ~casual_map_color_pal(total_rides),
    fillOpacity = 0.7,
    radius = ~sqrt(total_rides) * 5,
    stroke = FALSE,
    label = ~paste("Starting Stations and Total Rides, Casual Only", total_rides)
  ) %>%
  addLegend(
    pal = casual_map_color_pal,
    values = ~total_rides,
    title = "Starting Stations and Total Rides, Casual Only",
    opacity = 1
  ) 


# Map plot of Starting Stations and Total Trips - Members only .  
member_ride_counts <- Cleaned_Data_2022 %>%                    
  filter(member_casual == "member") %>%        
  group_by(start_lat, start_lng) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides))

member_map_color_pal <- colorNumeric(                      
  #palette = c("#2471a3", "#1b2631"),
  palette = c("#73A4C4", "#1b2631"),
  domain = member_ride_counts$total_rides
)

Member_Map<-leaflet(member_ride_counts) %>%
  addProviderTiles("CartoDB.Voyager") %>%      
  addCircles(
    lat = member_ride_counts$start_lat,
    lng = member_ride_counts$start_lng,
    color = ~member_map_color_pal(total_rides),
    fillColor = ~member_map_color_pal(total_rides),
    fillOpacity = 0.7,
    radius = ~sqrt(total_rides) * 5,
    stroke = FALSE,
    label = ~paste("Starting Stations and Total Rides, Member Only", total_rides)
  ) %>%
  addLegend(
    pal = member_map_color_pal,
    values = ~total_rides,
    title = "Starting Stations and Total Rides, Member Only",
    opacity = 1
  )
