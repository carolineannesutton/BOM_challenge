library(tidyverse)
bom_data <- read_csv("data/BOM_data.csv")
bom_data
bom_st <- read_csv("data/BOM_stations.csv")
bom_st

# Challenge: Putting it all together

# Question 1 (Sept 25)
# For each station, how many days have a minimum temperature, 
# a maximum temperature and 
# a rainfall measurement recorded

# sep the min/max column
bom_sep <- separate(bom_data, col = Temp_min_max, into = c("min", "max"), sep = "/") 


# filter to include only stations with data for min/max/Rainfall data
bom_sep %>% 
  filter(min != "-", max !="-", Rainfall !="-")

# assign a new table to data and count the number of stations
Data_count_ofMinMaxRainfall <- bom_sep %>%
  count(Station_number)

count(Data_count_ofMinMaxRainfall) # counting the number of row for stations with data

Bom_sumStation <- bom_sep %>% 
  group_by(Station_number)

count(Bom_sumStation)







# Question 2  Which month saw the lowest average daily temperature difference? (Sept 26)
  #change the data to numberic and filter out the "NA"

Bom_Diff <- bom_sep %>% 
  mutate (min = as.numeric(min)) %>% 
  mutate (max = as.numeric(max)) %>% 
  mutate (Rainfall = as.numeric(Rainfall)) %>% 
  mutate(temp_diff =max- min) %>% 
  filter (temp_diff != "NA")

# calculate the aveage temperature diff per month. Must group by month first, then summarise mean
# filter for the lowest value
  
Bom_Diff_Avg <- Bom_Diff %>%
  group_by(Month) %>% 
  summarise(temp_diff = mean(temp_diff)) %>%
  filter(temp_diff == min(temp_diff))
         
 Bom_Diff_Avg
  
 
# Question 3
# Which state saw the lowest average daily temperature difference
 
 
 # gather data into 3 columns ID, type of data, actual data

#gather(bom_st, Station_number, data, -info)
 
BomSt_tidy <- gather(bom_st, Station_number, values, -info) # use bom_st, give sensible column names, tell it not to use "info" 
BomSt_tidy

#Spread Data - use the "info" (type of data) for colunm names and the actual data is in the "values" column.
# "Station_number" is carried over for some reason (? thankfully)

Bom_Staions_Tidy <-spread(BomSt_tidy, info, values) %>% 
  mutate(Station_number = as.numeric(Station_number))  # making "Staion_number" numeric so it can compatible for both tables

Bom_Staions_Tidy #check


BOM_join <- full_join(Bom_Staions_Tidy,Bom_Diff) # create full join, it chose "Station_number"

BOM_join %>% 
  group_by(state) %>% 
  summarise(temp_diff = mean(temp_diff)) %>% 
  arrange(temp_diff) %>% 
  filter(temp_diff == min(temp_diff))
  

  
# Question 4: Does he westmost (lowest longitude) or eastmost (highest longitude) 
#     weather station in our dataset have a higher average solar exposure?

BOM_join %>% 
  # filter for min and max longitude
  filter(lon == min(lon) | lon == max(lon)) %>%
  #conver lon and solar exposure to numeric values
  mutate (lon = as.numeric(lon), Solar_exposure = as.numeric(Solar_exposure)) %>% 
  #filter out the "NA" data
  filter(Solar_exposure != "NA") %>% 
  # group by station to calculate the average solar exposure per station , keep the longitude value
  group_by(Station_number,lon) %>% 
  summarise(Solar_exposure_avg = mean(Solar_exposure)) %>% 
  ungroup() %>% 
  # create a new variable based on station location ie longitude
  mutate("Station_location" = case_when(lon == min(lon) ~ "westmost", lon == max(lon) ~ "eastmost"))


 
 write.csv(BOM_join, "Results/BOMST.csv")  

  
  
         