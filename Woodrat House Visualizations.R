library(mapview)
library(rgdal)
library(lubridate)

filename <- "/Volumes/ecology/eco_woodrats/Den Data/GPS Locations/Quail_Ridge_All_Oct_2018.GPX"
houses = readOGR(dsn = filename, layer="waypoints") #using this function in RGDAL
str(houses)
houses$name <- as.character(houses$name)
houses <- houses[grepl("WC", houses$name),] #select Wragg Canoyin Only
mapview(houses)

trap <- read.csv("~/Dropbox/Quail Ridge Woodrat Data/House Occupancy Maps/ALL_WoodratTrapData_WraggCanyon_cleaned_BJB.csv")
str(trap)
str(houses)
houses@coords
houses$location <- houses$name
trap$location <- as.character(trap$location)
trap <- trap[grepl("WC", trap$location),] #select Wragg Canyon Only

sort(unique(houses$location))
sort(unique(trap$location))

##clean up names of trap location
trap$location[nchar(trap$location)==3] # id houses with 3 charachters
trap$location <- ifelse(trap$location=="WC6" , "WC06" , trap$location)
trap$location <- ifelse(trap$location=="WC5B" , "WC05B" , trap$location)
trap$location <- ifelse(trap$location=="WC1" , "WC01" , trap$location)
trap$location <- ifelse(trap$location=="WC2" , "WC02" , trap$location)
trap$location <- ifelse(trap$location=="WC1A" , "WC01" , trap$location)
trap$location <- ifelse(trap$location=="WC8" , "WC08" , trap$location)
trap$location <- ifelse(trap$location=="WC7" , "WC07" , trap$location)
trap$location <- ifelse(trap$location=="WC9" , "WC09" , trap$location)
trap$location <- ifelse(trap$location=="WC2B" , "WC02B" , trap$location)
trap$location <- ifelse(trap$location=="WC2C" , "WC14" , trap$location) #i think this is right we should check
trap$location <- ifelse(trap$location=="WC8B" , "WC08B" , trap$location)
trap$location <- ifelse(trap$location=="WC8C" , "WC08C" , trap$location) 
trap$location <- ifelse(trap$location=="WC8D" , "WC08D" , trap$location) 
trap$location <- ifelse(trap$location=="WC01A" , "WC01" , trap$location) 
trap$location <- ifelse(trap$location=="WC37A" , "WC37" , trap$location) # check this
trap$location <- ifelse(trap$location=="WC37B" , "WC37" , trap$location) # check this
trap$location <- ifelse(trap$location=="WC9B" , "WC09B" , trap$location)
trap$location <- ifelse(trap$location=="WC24B" , "WC24" , trap$location) #we had done multiple traps at house, but its one house
trap$location <- ifelse(trap$location=="WC17" , "WC15" , trap$location) #if i remeber correctly 15 and 17 are same
trap$location <- ifelse(trap$location=="WC34B" , "WC34" , trap$location) #need to add 34B datapoint to gpx
trap$location <- ifelse(trap$location=="WC16" , "WC17" , trap$location) #need to check, 16 was on other side of road, think this is a mistake

sort(unique(trap$location)) %in% sort(unique(houses$location)) #will all be true if trap data has a gps coordinate

trap[trap$Rat.ID=="NA",]

complete.cases(trap[trap$Rat.ID,])

trap2 <- droplevels(trap[ complete.cases(trap$Rat.ID) ,  ])

str(houses) 

unique(trap2$Rat.ID)
d <-  merge(houses, trap2 , by=intersect("location", "location") ,  duplicateGeoms = TRUE)
d2 <- d[ complete.cases(d$Rat.ID) ,  ]
d2$date <- dmy(d2$date)
d2$date_index <- as.integer(as.factor(d2$date))
d2$year <- year(d2$date)
d2$rat_id_index <- as.integer(as.factor(d2$Rat.ID))

########individual maps#############
for(i in unique(d2$Rat.ID)[1:20] ){
 print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
 + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0) )
}

for(i in unique(d2$Rat.ID)[21:40] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[41:60] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[61:80] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[81:100] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[101:120] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[121:140] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[141:158] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , lpha.regions=1) )
}

#######maps by year#############
for(i in sort(unique(d2$year)) ){
  print(mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery") + mapview(d2[d2$year==i,] , zcol="rat_id_index" , cex=6 ) )
}
d2$ra
