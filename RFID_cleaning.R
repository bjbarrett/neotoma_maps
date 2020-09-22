library(lubridate)
library(broman)
library(stringr)
library(janitor)
library(bit64)#large integers
library(gmp)#large integers to hex
library(RColorBrewer)


#read in a text file
#/Volumes/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A
#smb://evafs.eva.mpg.de/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A/WC01A_RFIDLOG20190713.txt


###########start cleaning###################
rat_master <- read.table("~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/WraggRFIDMasterList.csv" , header =TRUE, sep = ",", dec = "." ,  stringsAsFactors=FALSE)
rat_master <- clean_names(rat_master)
str(rat_master)

#############################2013################################

###reads all of 2013
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2013"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]

rfid2013 <- setNames(do.call(rbind,Map(`cbind`, 
                                   lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

##next step is to get databvases talking

#############################2014################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2014/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

rfid2014 <- setNames(do.call(rbind,Map(`cbind`, 
                                   lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

#############################2015################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2015/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

rfid2015 <- setNames(do.call(rbind,Map(`cbind`, 
                                       lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

#############################2016################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2016/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

rfid2016 <- setNames(do.call(rbind,Map(`cbind`, 
                                       lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

#############################2017################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2017/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

rfid2017 <- setNames(do.call(rbind,Map(`cbind`, 
                                       lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

#############################2018################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2018/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("fromdat.txt", files) ]
files <- files[ !grepl("(likely41)", files) ]
files <- files[ !grepl("WC44_RFIDLOG20180719.txt", files) ] #blank but large, no visible data
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]
files_short <- files_short[ !grepl("fromdat.txt", files_short) ]
files_short <- files_short[ !grepl("(likely41)", files_short) ]
files_short <- files_short[ !grepl("WC44_RFIDLOG20180719.txt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

rfid2018 <- setNames(do.call(rbind,Map(`cbind`, 
                                         lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

##deal with fromdat later

############################2019################################
dir <- "~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2019/"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files <- files[ !grepl("Ftxt", files) ] #excludes files that end with F.txt
files_nz <- files #indexing for files short
files_zero <- files[which(file.size(files) == 0)]
files <- files[which(!file.size(files) == 0)]

files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]
files_short <- files_short[ !grepl("Ftxt", files_short) ]

files_short_zero <- files_short[which(file.size(files_nz) == 0)]
files_short <- files_short[which(!file.size(files_nz) == 0)]

#below was for troubleshooting issues w/ raw files and identifying where to see $ or blank spaces to delete to match column numbers

ncol_tab <- rep(NA,length(files))

for(i in 1:length(files)){
  rfid2019 <- setNames(do.call(rbind,Map(`cbind`,
                              lapply(files[i], read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short[i])), nm1)
  print(i)
  print("NCOL")
  print(ncol(rfid2019) )
  ncol_tab[i] <- ncol(rfid2019)
}
 files[i]

print(ncol_tab)

which(ncol_tab %in% 4)
files_four <- files[which(ncol_tab %in% 4)] ####figure out how to see files w/ 5 things
files_five <- files[which(ncol_tab %in% 5)]

#deal with simple files 4 first 
rfid2019_four <- setNames(do.call(rbind,Map(`cbind`,
                                       lapply(files_four, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short[which(ncol_tab %in% 4)] )), nm1)


############ lets add rat id
rfid2013$year <- 2013
rfid2014$year <- 2014
rfid2015$year <- 2015
rfid2016$year <- 2016
rfid2017$year <- 2017
rfid2018$year <- 2018
rfid2019_four$year <- 2019
#rfid2019$year <- 2019

rfid <- rbind(rfid2013 ,rfid2014,rfid2015,rfid2016,rfid2017,rfid2018,rfid2019_four)
#rfid <- rfid2017

#rat_master <- rat_master[rat_master$year < 2015 | rat_master$year>2020,]
rat_master$rfid_hex2[rat_master$rat_id=="TST2"] <- 1041041 ##override weird error

rfid$decimal_rfid_ncc <- as.numeric(sub('...', '', rfid$decimal_RFID)) #cut off 999 which is country code from RFID, convert to numeric
rfid$hexidecimal_rfid <- toupper(as.character(as.bigz(rfid$decimal_rfid_ncc),b=16)) #convert to hexi
unique(rfid$hexidecimal_rfid)
unique(rfid$decimal_rfid_ncc)

sort(unique(rfid$decimal_RFID))
sort(unique(rfid$decimal_rfid_ncc))

sort(unique(rfid$hexidecimal_rfid))
sort(unique(rat_master$rfid_hex1))
sort(unique(rat_master$rfid_hex2))
sort(unique(rat_master$rfid_hex3))
sort(unique(rat_master$rfid_hex4))

rfid$rat_id <- 0

#rfid[rfid$hexidecimal_rfid=="NA",] #evaluate NAs, these seem to be weird testers of a mystery pit tag in WC21/WC21_RFIDLOG20140926.txt 2014  
#rfid[rfid$hexidecimal_rfid=="1041041",] 

rfid <- rfid[rfid$hexidecimal_rfid!="NA",] 

#append rat id
for(i in 1:nrow(rfid) ){
    xx <- rat_master$rat_id[rat_master$rfid_hex1==rfid$hexidecimal_rfid[i] |
                                         rat_master$rfid_hex2==rfid$hexidecimal_rfid[i] | 
                                         rat_master$rfid_hex3==rfid$hexidecimal_rfid[i] | 
                                         rat_master$rfid_hex4==rfid$hexidecimal_rfid[i] 
                                         ]
    rfid$rat_id[i]  <- ifelse(length(xx) > 0 , xx , "xxxx")
}

table(rfid$rat_id)
rfid$hhmmss_text <- str_sub(rfid$yymmddhhmmss,-6,-1)
rfid$yymmdd_text <- str_sub(rfid$yymmddhhmmss,1,6)
rfid$ymd <- ymd(rfid$yymmdd_text)
rfid$timestamp <-  as_datetime(rfid$yymmddhhmmss)
rfid$yearmatch <- ifelse( year(rfid$timestamp)==rfid$year , 1 , 0) #assigns a 1 if year on timestamp matches year on batch file, 0 if no match
rfid$date <- date(rfid$timestamp)
rfid$date_index <- as.integer(as.factor(rfid$date))

###csv of rfid issues
rfid_issues <- rfid[rfid$rat_id=="xxxx",] #rfid does not match up to master
write.csv(rfid_issues , "rfid_issues.csv")
unique(rfid_issues$hexidecimal_rfid)

####create houseid from filename
rfid$house_id <- substr(rfid$file_name, 1, 5)#selects first 5 charachters of file name
rfid$house_id <- gsub("_.*","",rfid$house_id)#removes everthing after_
rfid$house_id <- gsub("/.*","",rfid$house_id)#removes everthing after/
rfid$house_id <- ifelse(rfid$house_id=="WC12A" , "WC12" , rfid$house_id ) #convert
rfid$house_id <- ifelse(rfid$house_id=="WC12B" , "WC12" , rfid$house_id )

rfid_og <- rfid
write.csv (rfid[rfid$yearmatch==0,] , "rfid_year_no_match.csv")
rfid <- rfid[is.na(rfid$date)==FALSE , ] #exclude NA dates
rfid <- rfid[rfid$yearmatch==1,] #only do scabs where year of file matches year on rfid, misses rfid where there was resets

rfid$date_index <- as.integer(as.factor(rfid$date))
unique(rfid$date_index)


###########Plots


##lets make a plot of woodrat locations over time
#generate color pallete per den
n <- length(unique(rfid$house_id))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
rfid$house_id_index <- as.integer(as.factor(rfid$house_id))
rfid$rat_id_index <- as.integer(as.factor(rfid$rat_id))
#rfid[rfid$rat_id==NA,]
pdf("rat_den_occ_plot_rfid.pdf", width=40, height=10)
par(mar=c(3,2,0,0) +.1)
plot(rfid$rat_id_index ~ rfid$date_index, yaxt='n' , xaxt='n', col=rfid$house_id_index , pch=15 , cex=0.8)
for(i in 1:max(rfid$rat_id_index)){  
  segments( min(rfid$date_index[rfid$rat_id_index==i] , na.rm = TRUE), i, max(rfid$date_index[rfid$rat_id_index==i] , na.rm = TRUE) ,i , col="grey" , lw=2)
}

points(rfid$rat_id_index~rfid$date_index , col=col_vector[rfid$house_id_index] , pch=15 , cex=0.8)
axis(1 , at=sort(unique(rfid$date_index)), labels=sort(unique(rfid$date)), cex.axis=0.4 , las=2)
axis(2 , at=sort(unique(rfid$rat_id_index)) , labels=sort(unique(rfid$rat_id)), cex.axis=0.3 , las=2)
text(rfid$rat_id_index~rfid$date_index , labels=substring(rfid$house_id,3) , cex=0.3)

dev.off()





#assign unique index based on order of appearance and 

rfid2 <- rfid
rfid2 <- rfid2[order(rfid2$date_index,rfid2$rat_id_index),]
rfid2$rat_id_index_time <- as.integer(as.factor(rfid2$rat_id))
rfid2$rat_id_index_time <- cumsum(!duplicated(rfid2$rat_id))


rfid2$rat_id_index_time <- 0
rfid2$j <- 0
rfid2$rat_id_index_time[1] <- 1
j <- 0
for(i in 1:nrow(rfid2)){
  
  rfid2$j[i] <- j <- ifelse(rfid2$rat_id_index[i] %in% rfid2$rat_id_index[1:i-1] ,
                         max(rfid2$j) ,
                         max(rfid2$j) + 1)
  
  rfid2$rat_id_index_time[i] <- ifelse(rfid2$rat_id_index[i] %in% rfid2$rat_id_index[1:i-1] ,
                                    max(rfid2$rat_id_index_time[rfid2$rat_id_index==rfid2$rat_id_index[i] ] ) ,
                                    j )
  
}
rfid2$j
rfid2$rat_id_index_time


pdf("rat_den_occ_plot_timeorder_rfid.pdf", width=30, height=10)

par(mar=c(4,4,0,0) +.1)
plot(rfid2$rat_id_index_time~rfid2$date_index , yaxt='n' , xaxt='n', col=col_vector[rfid2$house_id_index] , pch=15 , cex=0.8 , xlab="Date" , ylab="Rat ID")

for(i in 1:max(rfid2$rat_id_index_time)){  
  segments( min(rfid2$date_index[rfid2$rat_id_index_time==i]), i, max(rfid2$date_index[rfid2$rat_id_index_time==i]) ,i , col="grey" , lw=2)
}
points(rfid2$rat_id_index_time~rfid2$date_index , col=col_vector[rfid2$house_id_index] , pch=15 , cex=0.8)
axis(1 , at=sort(unique(rfid2$date_index)), labels=sort(unique(rfid2$date)), cex.axis=0.4 , las=2)
axis(2 , at=sort(unique(rfid2$rat_id_index_time)) , labels=sort(unique(rfid2$rat_id)), cex.axis=0.3 , las=2)
text(rfid2$rat_id_index_time~rfid2$date_index , labels=substring(rfid2$house_id,3) , cex=0.3)
for(i in 2012:2019){
  abline( v=min(rfid2$date_index[year(rfid2$date)==i] )  , col="grey" , lty=3) 
}
dev.off()

###################################################3
##read in trap data

trap <- read.csv("~/Dropbox/Quail Ridge Woodrat Data/trap_data_all_quail_clean.csv")
traps <- data.frame(      date=ymd(trap$date),
                          house_id=trap$location, 
                          rat_id=as.character(as.numeric(trap$Rat.ID)), 
                          hexidecimal_rfid=trap$RFID,
                          stringsAsFactors=FALSE )

str(traps)
unique(traps$rat_id)
unique(rfid$rat_id)
traps$trap <- 1
rfid$trap <- 0

library(gtools)
d <- smartbind(rfid,traps)

#assign unique index based on order of appearance and 
d$date_index <-  as.integer(as.factor(d$date))
d$rat_id_index <-  as.integer(as.factor(d$rat_id))
d$house_id_index <-  as.integer(as.factor(d$house_id))

rfid2 <- d
rfid2 <- rfid2[order(rfid2$date_index,rfid2$rat_id_index),]
rfid2$rat_id_index_time <- as.integer(as.factor(rfid2$rat_id))
rfid2$rat_id_index_time <- cumsum(!duplicated(rfid2$rat_id))


rfid2$rat_id_index_time <- 0
rfid2$j <- 0
rfid2$rat_id_index_time[1] <- 1
j <- 0
for(i in 1:nrow(rfid2)){
  
  rfid2$j[i] <- j <- ifelse(rfid2$rat_id_index[i] %in% rfid2$rat_id_index[1:i-1] ,
                            max(rfid2$j) ,
                            max(rfid2$j) + 1)
  
  rfid2$rat_id_index_time[i] <- ifelse(rfid2$rat_id_index[i] %in% rfid2$rat_id_index[1:i-1] ,
                                       max(rfid2$rat_id_index_time[rfid2$rat_id_index==rfid2$rat_id_index[i] ] ) ,
                                       j )
  
}
rfid2$j
rfid2$rat_id_index_time

n <- length(unique(d$house_id))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))


pdf("rat_den_occ_plot_timeorder_trap_and_rfid.pdf", width=40, height=10)

par(mar=c(4,4,0,0) +.1)
plot(rfid2$rat_id_index_time~rfid2$date_index , yaxt='n' , xaxt='n', col=col_vector[rfid2$house_id_index] , pch=15 , cex=0.8 , xlab="Date" , ylab="Rat ID")

legend("topleft" , c("trap" , "rfid scan") , pch=22 , bty='n' , col=c("black" , "grey75") , bg=c("grey75" , "grey75") )

for(i in 1:max(rfid2$rat_id_index_time)){  
    segments( min(rfid2$date_index[rfid2$rat_id_index_time==i]), i, max(rfid2$date_index[rfid2$rat_id_index_time==i]) ,i , col="grey75" , lw=3)
    
    segments( min(rfid2$date_index[rfid2$rat_id_index_time==i & rfid2$trap==1]), i, max(rfid2$date_index[rfid2$rat_id_index_time==i & rfid2$trap==1]) ,i , col="grey25" , lw=1)
}

points(rfid2$rat_id_index_time~rfid2$date_index , bg=col_vector[rfid2$house_id_index]  , col=ifelse(rfid2$trap==1 , 1 , col_vector[rfid2$house_id_index]  ), pch=22 , cex=0.8)
axis(1 , at=sort(unique(rfid2$date_index)), labels=sort(unique(rfid2$date)), cex.axis=0.4 , las=2)
axis(2 , at=unique(rfid2$rat_id_index_time) , labels=unique(rfid2$rat_id), cex.axis=0.3 , las=2)
text(rfid2$rat_id_index_time~rfid2$date_index , labels=substring(rfid2$house_id,3) , cex=0.25)
for(i in 2012:2019){
  abline( v=min(rfid2$date_index[year(rfid2$date)==i] )  , col="grey" , lty=3) 
}
dev.off()

####Stopped here######


#########notes on misfit tags not in original master, test tags were added
##test tag before deployment: 1B79702128 dirunal most days right
#likely a tester , dates right many houses 1B79702287

#another test tag: 1C2C8985EB--FIXED
#another test tag: 1B79702128----FIXED
# rat at 34B 1B7970228F 2015---FIXED
# another rat 1B797025C4 that moved around much //2015-2016 ----FIXED
# another rat that moved around 1B797025F4 //2015-2016
# likely tester 1B797028E4 2013-2015 ---FIXED
# rat at WC31. dates all messed up 1B79702A01---FIXED IS A RAT, DATES NEED ATTENTION
#likely a rat in 2013 and 2014 1B79702A67 @ WC21,22,23 __NEEDS FIXIN
#one time afternoon in 2016, maybe it belonged to annies or one i borrowed from tez 2056C463
### eff if i know. date messed just after midnight, 2 scans 9C9
##seems like a tester in 2014 1B79702287, mostly diurnal
####2018 11C06590 need to figure out at 2 and 2B



# another rat that moved around 1B797025F4 //2015-2016
#likely a rat in 2013 and 2014 1B79702A67 @ WC21,22,23 __NEEDS FIXIN
##seems like a tester in 2014 1B79702287, mostly diurnal

##what are WC12A and WC 12B?