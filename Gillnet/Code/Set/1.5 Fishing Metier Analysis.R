
#libraries

library(tidyverse)
library(sf)
library(sp)


#################
general_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/All Fisheries Data/'
gillnet_data<-'G:/Shared drives/bren-fisheriesclimate/mpa climate fisheries/ca_mpas_climate/Gillnet/'
#################

gillnet<-readRDS(paste0(gillnet_data, "Data/Processed Data/setgillnet_filtered.rds"))
############ 


# Create a new column that categorizes the mesh sizes
gillnet$category <- cut(gillnet$mesh_size_in_num, 
                   breaks = c(-Inf, 3.5, 5.9, 8.4, Inf), 
                   labels = c("Below 3.5", "Between 3.5 and 6", "Between 6 and 8.5", "Above 8.5"))

#Count the occurrences for each category
category_counts <- table(gillnet$category)

ggplot(data = as.data.frame(category_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Mesh Size Category") +
  ylab("Count") +
  ggtitle("Count of Mesh Sizes in Different Categories") +
  theme_minimal()


a<-gillnet%>%
  filter(mesh_size_in_num<22)%>%
  group_by(vessel_id, category)%>%
  summarize(n=n_distinct(trip.identifier))%>%
  ungroup()%>%
  group_by(vessel_id)%>%
  mutate(n.total.trips=sum(n))%>%
  group_by(vessel_id, category)%>%
  mutate(percent=n/n.total.trips)


ggplot() +
  geom_density(data=a, aes(x=percent, color=category)) 

# Assuming you have a dataframe 'mesh_data' that contains 'vessel_id', 'category', and 'n'
ggplot(a, aes(x = category, y = percent, fill = vessel_id)) +
  geom_bar(stat = "identity") 
