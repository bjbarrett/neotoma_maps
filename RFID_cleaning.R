library(lubridate)
#library(data.table)
library(broman)
library(stringr)
library(janitor)
library(bit64)#large integers
library(gmp)#large integers to hex


#read in a text file
#/Volumes/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A
#smb://evafs.eva.mpg.de/ecology/eco_woodrats/RFID/RFID Data 2019/WC01A/WC01A_RFIDLOG20190713.txt


###########start cleaning ignnore above###################
rat_master <- read.table("~/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/WraggRFIDMasterList.csv" , header =TRUE, sep = ";", dec = "." ,  stringsAsFactors=FALSE)
rat_master <- clean_names(rat_master)
str(rat_master)

###make length 8 hexidec
# rat_master$rfid_hex1_8 <- str_sub(rat_master$rfid_hex1, start= -8) #selects last 8 charachters in stringr package
# rat_master$rfid_hex2_8 <- str_sub(rat_master$rfid_hex2, start= -8) 
# rat_master$rfid_hex3_8 <- str_sub(rat_master$rfid_hex3, start= -8) 
# rat_master$rfid_hex4_8 <- str_sub(rat_master$rfid_hex4, start= -8) 
##convert to decimal
# rat_master$decimal1_8 <-  hex2dec(rat_master$rfid_hex1_8)
# rat_master$decimal2_8 <-  hex2dec(rat_master$rfid_hex2_8) 
# rat_master$decimal3_8 <-  hex2dec(rat_master$rfid_hex3_8) 
# rat_master$decimal4_8 <-  hex2dec(rat_master$rfid_hex4_8)

##lets do same with 10 just because
# rat_master$decimal1_10 <-  hex2dec(rat_master$rfid_hex1)
# rat_master$decimal2_10 <-  hex2dec(rat_master$rfid_hex2) 
# rat_master$decimal3_10 <-  hex2dec(rat_master$rfid_hex3) 
# rat_master$decimal4_10 <-  hex2dec(rat_master$rfid_hex4) 

#############################2013################################

###reads one folder
dir <- "/Users/BJB/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2013/WC01"

list.files(dir,recursive = TRUE) #reads subfiles

nm1 <- c("yymmddhhmmss","main_or_aux","decimal_RFID","file_name") #rename columns vector
files = list.files(dir,recursive = TRUE , full.names = TRUE , pattern=".txt")
files <- files[ !grepl("F.txt", files) ] #excludes files that end with F.txt
files_short <- list.files(dir,recursive = TRUE , full.names = FALSE , pattern=".txt")
files_short <- files_short[ !grepl("F.txt", files_short) ]

# rfid <- setNames(do.call(rbind,Map(`cbind`, 
#                                    lapply(files, read.table, header = FALSE, sep = " ", dec = "." ,  stringsAsFactors=FALSE), V4=files_short)), nm1)

###reads all of 2013
dir <- "/Users/BJB/Dropbox/Quail Ridge Woodrat Data/RFID_from_server/RFID Data 2013"
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
#options(scipen = 8)#disable scinoations
#issues is size of integer that can be stored
.Machine$integer.max #max integer size



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


############ lets add rat id
rfid2013$year <- 2013
rfid2014$year <- 2014
rfid <- rbind(rfid2013 , rfid2014)
rat_master <- rat_master[rat_master$year < 2015 | rat_master$year>2020,]
rat_master$rfid_hex2[rat_master$rat_id=="TST2"] <- 1041041 ##override weird error

# vals <- unique(rfid$decimal_RFID)
# vals
# vals2 <-  sub('...', '', vals) #cuts off first 5 charachters as indicated by dots 
# as.integer(vals2)
# as.integer64(vals2)
# toupper(as.character(as.bigz(vals2),b=16))
# 
# toupper(as.character(as.bigz(1006294507),b=16))
# toupper(as.hexmode(as.integer64(1006294507))) 
# 
# toupper(as.character(as.bigz(0365192668),b=16))
# toupper(as.hexmode(as.integer64(0365192668))) 
# 
# 
# as.hexmode(as.numeric(vals2))
# vals3 <- toupper(as.hexmode(as.integer64(vals2))) #converts to hexidecimal and the uppercases letters
# vals3      
# 
# toupper(as.hexmode(as.integer.integer64(vals)))
# rat_master$rfid_hex1_8
# rat_master$rfid_hex1
# rat_master$rfid_hex1_8
# rat_master$rfid_hex1
# rat_master$decimal1


rfid$decimal_rfid_ncc <-sub('...', '', rfid$decimal_RFID) #cut off 999 which is country code from RFID
#rfid$hexidecimal_rfid <- toupper(as.hexmode(as.integer(rfid$hexidecimal_rfid))) #converts to hexidecimal and the uppercases letters
rfid$hexidecimal_rfid <- toupper(as.character(as.bigz(rfid$decimal_rfid_ncc),b=16)) #convert to hexi


sort(unique(rfid$decimal_RFID))
sort(unique(rfid$decimal_rfid_ncc))
toupper(as.character(as.bigz(000101010101),b=16))
toupper(as.character(as.bigz(unique(rfid$decimal_rfid_ncc)),b=16))

sort(unique(rfid$hexidecimal_rfid))
#sort(unique(rat_master$rfid_hex1_8))
sort(unique(rat_master$rfid_hex1))
sort(unique(rat_master$rfid_hex2))
sort(unique(rat_master$rfid_hex3))
sort(unique(rat_master$rfid_hex4))

rfid$rat_id <- 0

toupper(as.hexmode(as.integer(000101010101)))
toupper(as.hexmode(as.integer(000101010101)))


rfid[rfid$hexidecimal_rfid=="NA",] #evaluate NAs, these seem to be weird testers of a mystery pit tag in WC21/WC21_RFIDLOG20140926.txt 2014  
rfid[rfid$hexidecimal_rfid=="1041041",] 
rfid <- rfid[rfid$hexidecimal_rfid!="NA",] 

which(unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex1))
which(unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex2))
which(unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex3))


rfid$hexidecimal_rfid[which(unique(rat_master$rfid_hex1) %in% unique(rfid$hexidecimal_rfid)) ]  
rfid$hexidecimal_rfid[which(unique(rat_master$rfid_hex2) %in% unique(rfid$hexidecimal_rfid)) ]  

       
which(unique(rat_master$rfid_hex2) %in% unique(rfid$hexidecimal_rfid))

rat_master$rfid_hex1[which(unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex1))]
rat_master$rfid_hex2[which(unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex2))]


unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex2)
unique(rfid$hexidecimal_rfid) %in% unique(rat_master$rfid_hex3)

rat_master$rat_id[rat_master$rfid_hex1==rfid$hexidecimal_rfid[1]]      

for(i in 1:nrow(rfid) ){
    xx <- rat_master$rat_id[rat_master$rfid_hex1==rfid$hexidecimal_rfid[i] |
                                         rat_master$rfid_hex2==rfid$hexidecimal_rfid[i] | 
                                         rat_master$rfid_hex3==rfid$hexidecimal_rfid[i] | 
                                         rat_master$rfid_hex4==rfid$hexidecimal_rfid[i] 
                                         ]
    rfid$rat_id[i]  <- ifelse(length(xx) > 0 , xx , "xxxx")
}

table(rfid$rat_id)
rfid$hhmmss <- str_sub(rfid$yymmddhhmmss,-6,-1)
rfid_issues <- rfid[rfid$rat_id=="xxxx",]
write.csv(rfid_issues , "rfid_issues.csv")

####Stopped here######

sort(unique(rfid$decimal_RFID))
sort(unique(rfid$decimal_RFID))

rfid$house_id <- substr(rfid$file_name, 1, 5)#selects first 5 charachters of file name
rfid$house_id <- gsub("_.*","",rfid$house_id)#removes everthing after_



























########olde shite

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

