setwd("/Users/apple/Documents/2017_winter/277 web social analytics/individual assignment/")
geo_data<-read.csv("Geo-Fence Analytics.csv")
View(geo_data)
head(geo_data)
# Data Processing
# a. Create dummy variable imp_large for the large impression
imp_large<-vector()
for(l in geo_data$imp_size){
  if (l== "728x90"){
    imp_large<-append(imp_large,1)} else{
      imp_large<-append(imp_large,0)}}
geo_data<-cbind(geo_data, imp_large)

#	b. Create dummy variables cat_entertainment, cat_social and cat_tech for app categories:
cat_entertainment<-vector()
for(l in geo_data$app_topcat){
  if ((l== "IAB1")|(l== "IAB1-6")){
    cat_entertainment<-append(cat_entertainment,1)} else{
      cat_entertainment<-append(cat_entertainment,0)}}
geo_data<-cbind(geo_data, cat_entertainment)

# cat_social
cat_social<-vector()
for(l in geo_data$app_topcat){
  if (l== "IAB14"){
    cat_social<-append(cat_social,1)} else{
      cat_social<-append(cat_social,0)}}
geo_data<-cbind(geo_data, cat_social)

# cat_tech
cat_tech<-vector()
for(l in geo_data$app_topcat){
  if (l== "IAB19-6"){
    cat_tech<-append(cat_tech,1)} else{
      cat_tech<-append(cat_tech,0)}}
geo_data<-cbind(geo_data,cat_tech)

# c.Create dummy variable os_ios for iOS devices:

os_ios<-vector()
for(l in geo_data$device_os){
  if (l== "iOS"){
    os_ios<-append(os_ios,1)} else{
      os_ios<-append(os_ios,0)}}
geo_data<-cbind(geo_data,os_ios)

# d. Create variable distance using Harvesine formula to calculate the distance for a pair of latitude/longitude coordinates.
# Distance (in kilometers) = 6371 * acos( cos( radians(LATITUDE1) ) * cos( radians( LATITUDE2 ) ) * cos( radians( LONGITUDE1 ) - radians(LONGITUDE2) ) + sin( radians(LATITUDE1) ) * sin( radians( LATITUDE2 ) ) )
attach(geo_data)
geo_data$distance= 6371 * acos( cos( aspace::as_radians(device_lat) ) * cos( aspace::as_radians( geofence_lat ) ) * cos( aspace::as_radians( device_lon ) - aspace::as_radians(geofence_lon) ) + sin( aspace::as_radians(device_lat) ) * sin( aspace::as_radians( geofence_lat ) ) )
detach(geo_data)
#Distance is the distance from the device to the geofence point


# e. Create variable distance_squared by squaring variable distance

geo_data$distance_squared= geo_data$distance* geo_data$distance

# f. Create variable ln_app_review_vol by taking natural log of app_review_vol

geo_data$ln_app_review_vol=log(geo_data$app_review_vol)

# Descriptive Statistics
# a.	Summarize the data by calculating the summary statistics (i.e., mean, median, std. dev., minimum and maximum) for didclick, distance, imp_large, cat_entertainment, cat_social, cat_tech, os_ios, ln_app_review_vol and app_review_val.
 

important_geo_data <- geo_data[c(7,15:21,23)] #separating the important variables
summary(important_geo_data)

# b. Report the correlations among the above variables.

cor(important_geo_data)

library("psych")

pairs(~didclick+distance+imp_large+ cat_entertainment+ cat_social+ cat_tech+ os_ios+ ln_app_review_vol+ app_review_val,
      data=important_geo_data, pch=20, main = "Scatterplot Matrix")

important_geo_data <- data.frame(important_geo_data)
pairs.panels(important_geo_data, gap=0)

# c.Plot the relationship of distance (x-axis) and click-through-rate (y-axis), and any other pairs of variables of interest.
# group distance
important_geo_data <- mutate(important_geo_data, distance_group = ifelse(distance>0 & distance<=0.5,"1",
                                                                         ifelse(distance>0.5 & distance <= 1,"2",
                                                                                ifelse(distance> 1& distance<=2,"3",
                                                                                       ifelse(distance>2 & distance<=4,"4",
                                                                                              ifelse(distance>4 & distance<=7,"5",
                                                                                                     ifelse(distance>7 & distance<=10,"6",
                                                                                                            ifelse(distance>10, "7", "N/A"))))))))
install.packages("sqldf")
library("sqldf")
attach(important_geo_data)
#get CTR
CTR_table<- sqldf("select sum(didclick) as sum_click, count(didclick) as count_impression, distance_group
from important_geo_data group by distance_group")
CTR_table$CTR= CTR_table$sum_click/CTR_table$count_impression

# Join together
geo_data_final<- sqldf("select *
from important_geo_data join CTR_table on CTR_table.distance_group=important_geo_data.distance_group")
geo_data_final<-geo_data_final[,-(9:11)]

# c.Plot the relationship of distance (x-axis) and click-through-rate (y-axis), and any other pairs of variables of interest.


plot(geo_data_final$distance_group, geo_data_final$CTR)


# Logistics Regression

glm_fit_geo <- glm(didclick ~ distance+distance_squared+imp_large+cat_entertainment+cat_social+cat_tech+os_ios+ln_app_review_vol+app_review_val,
                data=geo_data, family=binomial())
summary(glm_fit_geo)


