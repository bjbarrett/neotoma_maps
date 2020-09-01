library(lubridate)
library(data.table)
library(broman)
library(stringr)
library(janitor)

#read in a text file
#/Volumes/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A
#smb://evafs.eva.mpg.de/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A/WC01A_RFIDLOG20190713.txt

#below master needs 2019 rata added
rat_master <- read.table("~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/WraggRFIDMasterList.csv" , header =TRUE, sep = ";", dec = "." ,  stringsAsFactors=FALSE)
rat_master$RFID_HEX1_8 <- str_sub(rat_master$RFID_HEX1_10, start= -8) #selects last 8 charachters in stringr package
rat_master$RFID_HEX2_8 <- str_sub(rat_master$RFID_HEX2_10, start= -8) 
rat_master$RFID_HEX3_8 <- str_sub(rat_master$RFID_HEX3_10, start= -8)
rat_master$RFID_HEX4_8 <- str_sub(rat_master$RFID_HEX4_10, start= -8) 

rat_master$DECIMAL1 <-  hex2dec(rat_master$RFID_HEX1_8) #converts to decimal
rat_master$DECIMAL2 <-  hex2dec(rat_master$RFID_HEX2_8) #converts to decimal
rat_master$DECIMAL3 <-  hex2dec(rat_master$RFID_HEX3_8) #converts to decimal
rat_master$DECIMAL4 <-  hex2dec(rat_master$RFID_HEX4_8) #converts to decimal

#read rfids

setwd("/Volumes/ecology/eco_woodrats/RFID/RFID Data 2018/WC06/")

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files()
rfid <- setNames(do.call(rbind,Map(`cbind`, 
                                   lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files)), nm1)

rfid$hexidecimal_RFID <-  sub('.....', '', rfid$decimal_RFID) #cuts off first six charachters as indicated by dots 
rfid$hexidecimal_RFID <- toupper(as.hexmode(as.integer(rfid$hexidecimal_RFID))) #converts to hexidecimal and the uppercases letters

#attach rat id to RFID data based off of 8 digit hexidecimal code

#z <- d$Eartags[d$RFID_HEX1_8==rfid$hexidecimal_RFID | d$RFID_HEX2_8==rfid$hexidecimal_RFID | d$RFID_HEX3_8==rfid$hexidecimal_RFID | d$RFID_HEX4_8==rfid$hexidecimal_RFID]
rfid$rat_id <- 0
for(i in 1:nrow(rfid) ){
  rfid$rat_id[i] <- rat_master$Eartags[rat_master$RFID_HEX1_8==rfid$hexidecimal_RFID[i] | 
                                         rat_master$RFID_HEX2_8==rfid$hexidecimal_RFID[i] | 
                                         rat_master$RFID_HEX3_8==rfid$hexidecimal_RFID[i] | 
                                         rat_master$RFID_HEX4_8==rfid$hexidecimal_RFID[i] ]
  #rfid$rat_id[i] <- d$Eartags[d$RFID_HEX1_8==rfid$hexidecimal_RFID[i]]
}

rfid$house_id <- substr(rfid$file_name, 1, 5)#selects first 5 charachters of file name
rfid$house_id <- gsub("_.*","",rfid$house_id)#removes everthing after_







########newer ways below, ignore for now

#d <- read.table("/Users/brendanbarrett/Dropbox/Quail Ridge Woodrat Data/RFID Data/RFID Data 2019/WC13_RFIDLOG20190524.txt" , sep="\t")
#setwd("/Volumes/ecology/eco_woodrats/RFID/RFID Data 2019/WC24/")
folder <- "/Volumes/ecology/eco_woodrats/RFID/RFID Data 2018/WC01A/"
filelist_long <- list.files(folder , pattern = ".*.txt" , full.names=TRUE)
filelist_short <- list.files(folder , pattern = ".*.txt")

datalist <- lapply(filelist_long, FUN=read.table) ###using long gets files not in wd, short needs wd, this reads zeros too
datafr = do.call("rbind", datalist) 


nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector

rfid2 <- setNames(do.call("rbind",Map(`cbind`, 
                                   lapply(filelist_long, FUN=read.table), V4=files)), nm1)

#d <- read.table("/Volumes/ecology/eco_woodrats/RFID/RFID Data 2019/WC24/WC24_RFIDLOG20190524.txt")

#https://stackoverflow.com/questions/33175505/skip-empty-files-when-importing-text-files
#https://stackoverflow.com/questions/1407647/reading-text-files-using-read-table
#https://stackoverflow.com/questions/3397885/how-do-you-read-in-multiple-txt-files-into-r
DT <- rbindlist(sapply(filelist_short, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")
str(DT)
DT$V1
DT$location <- substr(DT$FileName, 1, 4)
names(DT)[names(DT)=="V1"] <- "yymmddhhmmss"
names(DT)[names(DT)=="V2"] <- "master"
names(DT)[names(DT)=="V3"] <- "rfid_hexi"

str(DT)
rfids

###########start cleaning ignnore above###################
rat_master <- read.table("~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/WraggRFIDMasterList.csv" , header =TRUE, sep = ";", dec = "." ,  stringsAsFactors=FALSE)
rat_master <- clean_names(rat_master)
str(rat_master)

###make length 8 hexidec
rat_master$rfid_hex1_8 <- str_sub(rat_master$rfid_hex1, start= -8) #selects last 8 charachters in stringr package
rat_master$rfid_hex2_8 <- str_sub(rat_master$rfid_hex2, start= -8) 
rat_master$rfid_hex3_8 <- str_sub(rat_master$rfid_hex3, start= -8) 
rat_master$rfid_hex4_8 <- str_sub(rat_master$rfid_hex4, start= -8) 
##convert to decimal
rat_master$decimal1_8 <-  hex2dec(rat_master$rfid_hex1_8)
rat_master$decimal2_8 <-  hex2dec(rat_master$rfid_hex2_8) 
rat_master$decimal3_8 <-  hex2dec(rat_master$rfid_hex3_8) 
rat_master$decimal4_8 <-  hex2dec(rat_master$rfid_hex4_8) 
##lets do same with 10 just because
rat_master$decimal1_10 <-  hex2dec(rat_master$rfid_hex1)
rat_master$decimal2_10 <-  hex2dec(rat_master$rfid_hex2) 
rat_master$decimal3_10 <-  hex2dec(rat_master$rfid_hex3) 
rat_master$decimal4_10 <-  hex2dec(rat_master$rfid_hex4) 

#############################2013################################

###reads one folder
dir <- "/Users/BJB/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2013/WC01"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]

rfid <- setNames(do.call(rbind,Map(`cbind`, 
                                   lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

###reads all of 2013
dir <- "/Users/BJB/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2013"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]

rfid2013 <- setNames(do.call(rbind,Map(`cbind`, 
                                   lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

##next step is to get databvases talking
options(scipen = 12)#disable scinoations

vals <- unique(rfid2013$decimal_RFID)
vals2 <-  sub('......', '', vals) #cuts off first six charachters as indicated by dots 
vals2
vals3 <- toupper(as.hexmode(as.integer(vals2))) #converts to hexidecimal and the uppercases letters
vals3

toupper(as.hexmode(as.integer(vals)))

#############################2014################################



dir <- "/Users/BJB/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2014/"

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

vals <- unique(rfid2014$decimal_RFID)
vals2 <-  sub('......', '', vals) #cuts off first six charachters as indicated by dots 
vals2
vals3 <- toupper(as.hexmode(as.integer(vals2))) #converts to hexidecimal and the uppercases letters
vals3      