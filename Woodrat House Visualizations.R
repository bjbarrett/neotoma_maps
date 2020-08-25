library(mapview)
library(rgdal)
library(lubridate)
library(RColorBrewer)

filename <- "/Volumes/ecology/eco_woodrats/Den Data/GPS Locations/Quail_Ridge_All_Oct_2018.GPX"
houses = readOGR(dsn = filename, layer="waypoints") #using this function in RGDAL
str(houses)
houses$name <- as.character(houses$name)
houses <- houses[grepl("WC", houses$name),] #select Wragg Canyon Only
mapview(houses)
rfid18 <- read.csv("/Volumes/ecology/eco_woodrats/Woodrat Database/2018 RFID.csv")
trap <- read.csv("~/Dropbox/Quail Ridge Woodrat Data/House Occupancy Maps/ALL_WoodratTrapData_WraggCanyon_cleaned_BJB.csv")
str(trap)
str(houses)

houses$location <- houses$name
trap$location <- as.character(trap$location)
trap <- trap[grepl("WC", trap$location),] #select Wragg Canyon Only

sort(unique(houses$location))
sort(unique(trap$location))


###clean up raw files
str(houses)
houses <- houses[,names(houses) %in% c("ele" , "time" , "name" , "timeISO" , "location")]

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

trap[trap$location=="WC37B",]
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

########individual maps of rats from traps#############
for(i in unique(d2$Rat.ID)[1:20] ){
 print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
 + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0) )
}

for(i in unique(d2$Rat.ID)[21:40] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[41:60] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[61:80] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[81:100] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[101:120] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[121:140] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

for(i in unique(d2$Rat.ID)[141:158] ){
  print( mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery")
         + mapview(d2[d2$Rat.ID==i,] , zcol="date_index" , cex=5 , alpha=0 , alpha.regions=1) )
}

#######maps by year#############
for(i in sort(unique(d2$year)) ){
  print(mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0.1 , pch=19 , map.types="Esri.WorldImagery" , layer.name=i) + mapview(d2[d2$year==i,] , zcol="rat_id_index" , cex=6 ,  layer.name='Rat.ID') )
}

mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0.1 , pch=19 , map.types="Esri.WorldImagery" )


######RFID#########
str(rfid18)
rfid18$location <- as.character(rfid18$denID_FileName)
rfid18 <- rfid18[rfid18$ratID_local!='TST1',]
rfid18 <- rfid18[rfid18$ratID_local!='TST2',]
rfid18 <- rfid18[rfid18$ratID_local!='TST3',]
rfid18 <- rfid18[rfid18$ratID_local!='TST4',]
rfid18 <- rfid18[rfid18$ratID_local!='',]
rfid18 <- droplevels(rfid18)
unique(rfid18$ratID_local)

table(rfid18$ratID_local)

rfid18$Rat.ID <- rfid18$ratID_local
rfid18$dateString[1:20]

r18 <- rfid18[,names(rfid18) %in% c("location" , "Rat.ID" , "dateISO" , "timeISO")]

unique(r18$Rat.ID)
str(r18)
str(houses)
r <-  merge(houses, r18 , by=intersect("location", "location") ,  duplicateGeoms = TRUE)
r2 <- r[ complete.cases(r$Rat.ID) ,  ]
r2$date <- ymd(r2$dateISO)
r2$time_reader <- lubridate::hms(r2$timeIS0[1:20])
as.time(r2$timeIS0[1:20])
r2$date_index <- as.integer(as.factor(r2$date))
r2$year <- year(r2$date)
r2$rat_id_index <- as.integer(as.factor(r2$Rat.ID))
r2$datetime_scan <- as.POSIXct(paste(r2$dateISO, r2$timeISO))

mapview(r2 , zcol='Rat.ID') 

r2$rat_id_index <- as.integer(r2$Rat.ID)
 unique(r2$rat_id_index)

 for(i in 1:20 ){
   print(mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery") + mapview(r2[r2$rat_id_index==i,] , zcol='date_index') )
 }
 
 for(i in 21:max(r2$rat_id_index) ){
   print(mapview(houses , color="grey" ,  alpha=.99 ,alpha.regions=0 , pch=19 , map.types="Esri.WorldImagery") + mapview(r2[r2$rat_id_index==i,] , zcol='date_index') )
 }
 
mapview(r2[r2$rat_id_index==1,] ) 

##lets make a plot of woodrat locations over time
#generate color pallete per den
n <- length(unique(d2$location))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
d2$location_index <- as.integer(as.factor(d2$location))

pdf("rat_den_occ_plot.pdf", width=8, height=11)
par(mar=c(3,2,0,0) +.1)
  plot(d2$rat_id_index~d2$date_index , yaxt='n' , xaxt='n', col=col_vector[d2$location_index] , pch=15 , cex=0.8)
  for(i in 1:max(d2$rat_id_index)){  
    segments( min(d2$date_index[d2$rat_id_index==i]), i, max(d2$date_index[d2$rat_id_index==i]) ,i , col="grey")
  }
  points(d2$rat_id_index~d2$date_index , col=col_vector[d2$location_index] , pch=15 , cex=0.8)
  axis(1 , at=sort(unique(d2$date_index)), labels=sort(unique(d2$date)), cex.axis=0.4 , las=2)
  axis(2 , at=sort(unique(d2$rat_id_index)) , labels=sort(unique(d2$Rat.ID)), cex.axis=0.3 , las=2)
  text(d2$rat_id_index~d2$date_index , labels=substring(d2$location,3) , cex=0.3)

dev.off()
#assign unique index based on order of appearance and 

d3 <- d2@data
d3 <- d3[order(d3$date_index,d3$rat_id_index),]
d3$rat_id_index_time <- as.integer(as.factor(d3$Rat.ID))
d3$rat_id_index_time <- cumsum(!duplicated(as.factor(d3$Rat.ID)))
min(d2$date_index[d2$rat_id_index==1] )
max(d2$date_index[d2$rat_id_index==1])

d3$rat_id_index_time <- 0
d3$j <- 0
d3$rat_id_index_time[1] <- 1
j <- 0
for(i in 1:nrow(d)){
  
d3$j[i] <- j <- ifelse(d3$rat_id_index[i] %in% d3$rat_id_index[1:i-1] ,
              max(d3$j) ,
              max(d3$j) + 1)
              
  d3$rat_id_index_time[i] <- ifelse(d3$rat_id_index[i] %in% d3$rat_id_index[1:i-1] ,
                                  max(d3$rat_id_index_time[d3$rat_id_index==d3$rat_id_index[i] ] ) ,
                                  j )

}
d3$j
d3$rat_id_index_time

pdf("rat_den_occ_plot_timeorder.pdf", width=8, height=11)

par(mar=c(4,4,0,0) +.1)
plot(d3$rat_id_index_time~d3$date_index , yaxt='n' , xaxt='n', col=col_vector[d3$location_index] , pch=15 , cex=0.8 , xlab="Date" , ylab="Rat ID")
for(i in 1:max(d3$rat_id_index_time)){  
  segments( min(d3$date_index[d3$rat_id_index_time==i]), i, max(d3$date_index[d3$rat_id_index_time==i]) ,i , col="grey")
}
points(d3$rat_id_index_time~d3$date_index , col=col_vector[d3$location_index] , pch=15 , cex=0.8)
axis(1 , at=sort(unique(d3$date_index)), labels=sort(unique(d3$date)), cex.axis=0.4 , las=2)
axis(2 , at=sort(unique(d3$rat_id_index_time)) , labels=sort(unique(d3$Rat.ID)), cex.axis=0.3 , las=2)
text(d3$rat_id_index_time~d3$date_index , labels=substring(d3$location,3) , cex=0.3)
for(i in 2012:2019){
  abline( v=min(d3$date_index[year(d3$date)==i] )  , col="grey" , lty=3) 
}
dev.off()
