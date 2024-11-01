
library(dplyr)
library(sf)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(data.table)
library(lubridate)




#################
#upload raw data
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
lobster_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Lobster/'

lobster_logbook<-readRDS(paste0(lobster_data, "Data/Raw Data/CDFW_1980_2022_lobster_logbook_data.rds"))
landings<-readRDS(paste0(lobster_data, "Data/Raw Data/1980_2022_landings_receipts.rds"))
port_info<-read.csv(paste0(lobster_data, 'Data/Raw Data/Port Codes.csv'))%>%
  rename(port_id=port_code)
port_info$port_id<-as.double(port_info$port_id)
#################



#################
#create new columns with vesselID_date for logbook and landings and for receipts. up to two receipt ids.
lobster_logbook2<- separate(lobster_logbook, receipt_ids, into = c("receipt_id1", "receipt_id2"), sep = ",")
lobster_logbook2$receipt_id2 <- trimws(lobster_logbook2$receipt_id2)

lobster_logbook2 <- lobster_logbook2 %>%
  mutate(receipt_vesselID_1 = ifelse(is.na(receipt_id1) | is.na(vessel_id), NA, paste(receipt_id1, vessel_id, sep = "_"))) %>%
  mutate(receipt_vesselID_2 = ifelse(is.na(receipt_id2) | is.na(vessel_id), NA, paste(receipt_id2, vessel_id, sep = "_")))%>%
  mutate(receipt_vesselID_date_1 = ifelse(is.na(receipt_id1) | is.na(vessel_id) | is.na(date), NA, paste(receipt_id1, vessel_id, date,sep = "_"))) %>%
  mutate(receipt_vesselID_date_2 = ifelse(is.na(receipt_id2) | is.na(vessel_id) |is.na(date), NA, paste(receipt_id2, vessel_id, date,sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.logbooks=date)

lobster_logbook2$month <- month(lobster_logbook2$date.logbooks)

lobster_logbook2<-lobster_logbook2%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))

landings2<-landings  %>%
  mutate(receipt_vesselID_date = ifelse(is.na(receipt_id) | is.na(vessel_id) |is.na(date), NA, paste(receipt_id, vessel_id,date, sep = "_"))) %>%
  mutate(receipt_vesselID = ifelse(is.na(receipt_id) | is.na(vessel_id), NA, paste(receipt_id, vessel_id, sep = "_")))%>%
  mutate(vesselID_date = ifelse(is.na(vessel_id) | is.na(date), NA, paste(vessel_id, date, sep = "_")))%>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))%>%
  rename(date.landings=date)%>%
  left_join(dplyr::select(port_info, port_id, port.complex), by="port_id")

landings2$month <- month(landings2$date.landings)

landings2<-landings2%>%
  mutate(vesselID_year_month=ifelse(is.na(vessel_id)| is.na(year)| is.na(month), NA,paste(vessel_id, year, month, sep="_" )))%>%
  mutate(vesselID_year=ifelse(is.na(vessel_id)| is.na(year), NA,paste(vessel_id, year, sep="_" )))
#################


#################
#getting "perfect" data
# Check for duplicated values in 'ID' column
duplicated_ids <- duplicated(landings2$receipt_vesselID_date)
# Show rows with duplicated values
df<-print(landings2[duplicated_ids, ])
# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_vesselID_date) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

result%>%
  filter(same_B_values=="FALSE")#no matches can do direct matching process

landings.tomerge<- landings2%>%
  distinct(receipt_vesselID_date, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)

matched_data<-lobster_logbook2%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,receipt_vesselID_date,port_id,port,primary_gear,gear,date.landings,port.complex), by=c("receipt_vesselID_date_1" = "receipt_vesselID_date"), na_matches = "never")%>%
  filter(!is.na(port.complex))

traps<-matched_data%>%
  filter(gear=="Crab or lobster trap")
#################

#################
#Test #1 
#lobster logbooks with landing receipts 15 days after. Will see if we can properply match those with the landings data
lobster.receipt<-traps%>%
  rename(port.complex.perfect=port.complex, date.landings.perfect=date.landings)

# Check for duplicated values in 'ID' column
duplicated_ids <- duplicated(landings2$receipt_vesselID)
# Show rows with duplicated values
df<-print(landings2[duplicated_ids, ])
# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_vesselID) %>%
  summarise(same_B_values = n_distinct(port_id) == 1)
result%>%
  filter(same_B_values=="FALSE")# none- can match directly


landings.tomerge<- landings2%>%
  distinct(receipt_vesselID, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)

#match with first landing receipt available
matched_data<-lobster.receipt%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,date.landings, receipt_vesselID,port.complex), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
  filter((abs(date.landings - date.logbooks) <= 15 | is.na(date.landings)))
# Identify unmatched entries
unmatched_entries <- lobster.receipt %>%
  anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))

# Combine matched entries with unmatched entries (without matched data)
lobster.receipt2 <- bind_rows(
  matched_data,
  unmatched_entries 
)


#see the percentage correct or not - 100%
lobster.receipt3<-lobster.receipt2 %>%
  mutate(matches = (port.complex.perfect == port.complex))%>%
  rename(port.complex.test1=port.complex)

lobster.receipt3%>%
  filter(matches=="FALSE") #100% match

#################

#TEST 1.5
#matching receipts that only has one port complex regardless of date

#landings data with more more than one port complex
df <- landings2%>%
  group_by(receipt_id) %>%
  filter(n() > 1) %>%
  ungroup()

# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_id) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

df1<-result%>%
  filter(same_B_values=="FALSE") 
# Find common names
common_names <- intersect(df1$receipt_id, lobster.receipt3$receipt_id1) #4 for receipt 1, 0 for receipt 2

no.ports.1<- lobster.receipt3[!(lobster.receipt3$receipt_id1 %in% common_names), ] #these should be able to match directly with ports
no.ports.2<- lobster.receipt3[(lobster.receipt3$receipt_id1 %in% common_names), ]%>% #these have more than one port listed
  mutate(port.complex.test2= NA)

# can merge the no duplicates with the landings data
landings2.tomerge<- landings2%>%
  distinct(receipt_id, .keep_all = TRUE)

#merging with port and gear information
no.ports.1.match<- no.ports.1%>%
  #attach information from receipts
  left_join(dplyr::select(landings2.tomerge,receipt_id, port.complex), by=c("receipt_id1" = "receipt_id"), na_matches = "never")%>%
  rename(port.complex.test2=port.complex)

#merge with no.ports2. 
no.ports<-rbind(no.ports.1.match, no.ports.2)

#Add in equal thing


#see the percentage correct or not - 100%
lobster.receipt4<-no.ports %>%
  mutate(matches.2 = (port.complex.perfect == port.complex.test2))


lobster.receipt4%>%
  filter(matches.2=="FALSE") #100% match



#################
#STEP 2
#Second, if a vessel was recorded at only one port complex throughout the entire landings dataset, we assigned that port complex to the logbook data

#list of vessel ids that only had one port associated with it
result <- landings2 %>%
  group_by(vessel_id)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize( port.complex=first(port.complex))

result$vessel_id<-as.character(result$vessel_id)

#match with the remaining vessel ids with no ports 
no.ports<-lobster.receipt4%>%
  left_join(result, by="vessel_id")%>%
  rename(port.complex.test3=port.complex)

#see the percentage correct or not - 100%
lobster.receipt5<-no.ports %>%
  mutate(matches.3 = (port.complex.perfect == port.complex.test3))


lobster.receipt5%>%
  filter(matches.3=="FALSE") #100% match

#################




#################
#STEP 3
#Third, we used the combination of vessel id and date to match the datasets. For a day in which there were multiple port complexes 
#listed for a vessel within the landings data, we assigned the port complex that appeared most frequently to the corresponding logbook entry.


df <- landings2 %>%
  group_by(vesselID_date) %>%
  filter(n() > 1) %>%
  ungroup()

# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(vesselID_date) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

df1<-result%>%
  filter(same_B_values=="FALSE")

# Find common names
common_names <- intersect(df1$vesselID_date, no.ports$vesselID_date)  #77 combos with different ports now with the same vessel_ID and date.

#separate again....
no.dupes<- lobster.receipt5[!(lobster.receipt5$vesselID_date %in% common_names), ] #these should be able to match directly with ports
dupes<- lobster.receipt5[(lobster.receipt5$vesselID_date %in% common_names), ]%>% #these have more than one port listed
mutate(port.complex.test.4=NA)


# can merge the no duplicates with the landings data
landings.tomerge<- landings2%>%
  distinct(vesselID_date, .keep_all = TRUE) #just need the port info

no.dupes<-no.dupes%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,vesselID_date,port.complex), by="vesselID_date", na_matches = "never")%>%
  rename(port.complex.test.4=port.complex)

#see the percentage correct or not - 100%
lobster.receipt6<-rbind(no.dupes, dupes) %>%
  mutate(matches.4 = (port.complex.perfect == port.complex.test.4))


a<-lobster.receipt6%>%
  filter(matches.4=="FALSE") #100% match
#################


#################
#STEP 3.5
#Matching entries with port complexes most often seen that day

vessel_dates<-lobster.receipt6$vesselID_date

landings3<- landings2[(landings2$vesselID_date %in% vessel_dates), ]


df<-landings3%>%
  group_by(vesselID_date, port.complex) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1)%>%
  rename(port_complex1=port.complex)

#merging with port and gear information
no.ports<- lobster.receipt6%>%
  #attach information from receipts
  left_join(dplyr::select(df,vesselID_date,port_complex1), by="vesselID_date", na_matches = "never")%>%
  rename(port.complex.test.5=port_complex1)


#see the percentage correct or not - 
lobster.receipt7<-no.ports%>%
  mutate(matches.5 = (port.complex.perfect == port.complex.test.5))


b<-lobster.receipt7%>%
  filter(matches.5=="FALSE") #100% match
#################


#################
#STEP 4
#Fourth, we assigned a port complex from the landings data if a vessel visited the same port 
#complex 90% or more of the time during a specific year and month


vessels<-lobster.receipt7$vessel_id

df<-landings2%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, month, port.complex)%>%
  summarize(number=n())%>%
  ungroup()%>%
  group_by(vessel_id,year, month)%>%
  mutate(perc.complex=number/sum(number))


df.tomerge<-df%>%
  filter(perc.complex>=.9)%>%
  dplyr::select(-perc.complex,-number)%>%
  #mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))


#match those per year 
no.ports<-lobster.receipt7%>%
  left_join(df.tomerge, by=c("vessel_id" , "year", "month"))%>%
  rename(port.complex.test.6=port.complex)

#see the percentage correct or not - 
lobster.receipt8<-no.ports%>%
  mutate(matches.6 = (port.complex.perfect == port.complex.test.6))


c<-lobster.receipt8%>%
  filter(matches.6=="FALSE") 
#################



#################
#STEP 5
#Fifth we added a port complex to the logbook data if a vessel went to the same port complex 90% or more of the time during a specific year.


#list of vessel ids and years that only had one port associated with it
result <- landings2 %>%
  group_by(vesselID_year)%>%
  filter(n_distinct(port.complex) == 1) %>%
  summarize(port.complex=first(port.complex))

vessels<-lobster.receipt8$vessel_id

df<-landings2%>%
  filter(vessel_id %in% vessels)%>%
  group_by(vessel_id, year, port.complex)%>%
  summarize(number=n())%>%
  ungroup()%>%
  group_by(vessel_id,year)%>%
  mutate(perc.complex=number/sum(number))

df.tomerge<-df%>%
  filter(perc.complex>=.9)%>%
  dplyr::select(-perc.complex,-number)%>%
 # mutate(year=as.character(year))%>%
  mutate(vessel_id=as.character(vessel_id))

#match those per year 
no.ports<-lobster.receipt8%>%
  left_join(df.tomerge, by=c("vessel_id" , "year"))%>%
  rename(port.complex.test.7=port.complex)

#see the percentage correct or not - 100%
lobster.receipt9<-no.ports%>%
  mutate(matches.7 = (port.complex.perfect == port.complex.test.7))


d<-lobster.receipt9%>%
  filter(matches.7=="FALSE") #100% match
#################



#################
# Find cutoff for the number of days in a buffer. Create a accumulation curve with days in buffer on x axis and percent of matched logbook and landings receipts on y. 

#get entries with landing receipts.
receipts<-lobster_logbook2%>%
  filter(!is.na(receipt_id1))

# Check for duplicated values in 'ID' column
duplicated_ids <- duplicated(landings2$receipt_vesselID)
# Show rows with duplicated values
df<-print(landings2[duplicated_ids, ])
# Group by column A and check if values in column B are all the same
result <- df %>% 
  group_by(receipt_vesselID) %>%
  summarise(same_B_values = n_distinct(port.complex) == 1)

result%>%
  filter(same_B_values=="FALSE")# none in lobster


landings.tomerge<- landings2%>%
  distinct(receipt_vesselID, .keep_all = TRUE)%>%
  rename(receipt_ids=receipt_id)
#################


#################
#create a loop for going through 1-15 days 
# Initialize an empty data frame for storing results
results_df <- data.frame(day = integer(), matched = integer(), total = integer(), percent_matched = numeric())


#loop code
for(i in 1:15){
  #match with first landing receipt available
matched_data<-receipts%>%
  #attach information from receipts
  left_join(dplyr::select(landings.tomerge,date.landings, receipt_vesselID,port.complex), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
  filter(((date.landings - date.logbooks) <= i ) & (date.landings - date.logbooks) >= 0 )

# Identify unmatched entries
unmatched_entries <- receipts %>%
  anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))

# Combine matched entries with unmatched entries (without matched data)
lobster.receipt2 <- bind_rows(
  matched_data,
  unmatched_entries 
)

#dataframe for graphing
summary<-lobster.receipt2%>%
  summarise(matched=sum(!is.na(port.complex)), total=n())%>%
  mutate(percent_matched=(matched/total)*100)%>%
  mutate(day=i)

# Append the current summary to the results dataframe
results_df <- bind_rows(results_df, summary)
}

# Now plot using cumulative_percent_matched
p<-ggplot(results_df, aes(x = day, y = percent_matched)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Days in Buffer",
    y = "Percent Matched"
  ) + # base theme
theme(axis.text=element_text(size=8),
                      axis.text.y = element_text(angle = 90, hjust = 0.5),
                      axis.title=element_text(size=10),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  scale_y_continuous(breaks = seq(50, 65, 5), limits = c(50, 65))

filename1 <- paste0(lobster_data, 'Figures/Exploratory/Matching_Buffer_15 days.jpg')
ggsave(filename = filename1, plot = p, width = 6, height = 3, units = "in", dpi = 300)
#################



#################

#now for 8 days on each side

#create a loop for going through 1-15 days 
# Initialize an empty data frame for storing results
results_df <- data.frame(day = integer(), matched = integer(), total = integer(), percent_matched = numeric())

#loop code
for(i in 1:8){
  #match with first landing receipt available
  matched_data<-receipts%>%
    #attach information from receipts
    left_join(dplyr::select(landings.tomerge,date.landings, receipt_vesselID,port.complex), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
    filter((abs(date.landings - date.logbooks) <= i ))
  
  # Identify unmatched entries
  unmatched_entries <- receipts %>%
    anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))
  
  # Combine matched entries with unmatched entries (without matched data)
  lobster.receipt2 <- bind_rows(
    matched_data,
    unmatched_entries 
  )
  
  #dataframe for graphing
  summary<-lobster.receipt2%>%
    summarise(matched=sum(!is.na(port.complex)), total=n())%>%
    mutate(percent_matched=(matched/total)*100)%>%
    mutate(day=i)
  
  # Append the current summary to the results dataframe
  results_df <- bind_rows(results_df, summary)
}

p2<-ggplot(results_df, aes(x = day, y = percent_matched)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Days in Buffer",
    y = "Percent Matched"
  ) + # base theme
  theme(axis.text=element_text(size=8),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_text(size=10),
        # plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  scale_y_continuous(breaks = seq(50, 65, 5), limits = c(50, 65))

filename1 <- paste0(losbter_data,'Figures/Exploratory/Matching_Buffer_8 days either side.jpg')
ggsave(filename = filename1, plot = p2, width = 6, height = 3, units = "in", dpi = 300)

#################




#################

#now for 15 days on each side

#create a loop for going through 1-15 days 
# Initialize an empty data frame for storing results
results_df <- data.frame(day = integer(), matched = integer(), total = integer(), percent_matched = numeric())

#loop code
for(i in 1:15){
  #match with first landing receipt available
  matched_data<-receipts%>%
    #attach information from receipts
    left_join(dplyr::select(landings.tomerge,date.landings, receipt_vesselID,port.complex), by=c("receipt_vesselID_1" = "receipt_vesselID"), na_matches = "never")%>%
    filter((abs(date.landings - date.logbooks) <= i ))
  
  # Identify unmatched entries
  unmatched_entries <- receipts %>%
    anti_join(matched_data, by = c("receipt_vesselID_1", "date.logbooks"))
  
  # Combine matched entries with unmatched entries (without matched data)
  lobster.receipt2 <- bind_rows(
    matched_data,
    unmatched_entries 
  )
  
  #dataframe for graphing
  summary<-lobster.receipt2%>%
    summarise(matched=sum(!is.na(port.complex)), total=n())%>%
    mutate(percent_matched=(matched/total)*100)%>%
    mutate(day=i)
  
  # Append the current summary to the results dataframe
  results_df <- bind_rows(results_df, summary)
}

p3<-ggplot(results_df, aes(x = day, y = percent_matched)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Number of Days in Buffer",
    y = "Percent Matched"
  ) + # base theme
  theme(axis.text=element_text(size=8),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_text(size=10),
        # plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  scale_y_continuous(breaks = seq(50, 65, 5), limits = c(50, 65.5))

filename1 <- paste0(lobster_data, 'Figures/Exploratory/Matching_Buffer_15 days either side.jpg')
ggsave(filename = filename1, plot = p3, width = 6, height = 3, units = "in", dpi = 300)
#################



