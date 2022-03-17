############################-
#Garmin photo metadata import script------
#
#Description:
#The primiary role of this script is to import csv file from Garmin Basecamp
#which contained the metadata from photo taken along a handfish transect. This
#script will extract and organise the datafiles in a format which can then be 
#imported to the Master Access file
#
#Date:
#2022-03-04
############################-
library(tidyverse)

#Locating work directory on network drive----
netdir<-"\\\\fs1-hba.nexus.csiro.au\\{oa-handfish}\\work"
HFPhotoDir<-"Handfish_photos_All_2022"

#listing folder containing photo metadata
folderloop<-str_subset(list.files(file.path(netdir,HFPhotoDir))
                      ,"\\d+")

#loop through all folders within the all photo directory
#find the csv file corresponding to the Garmin export from each dive day and
#clean the metadata
AccessOP<-data.frame()
for (i in 1:length(folderloop)){
#accessing folder
  HFSSfolder<-file.path(netdir,HFPhotoDir,folderloop[i])
  CsvSS<-list.files(HFSSfolder, pattern = "*\\.csv")
  #selecting csv file
    if (!is_empty(CsvSS)==TRUE){
    GarminIP<-read.csv(file = file.path(HFSSfolder,CsvSS[1]),
                       col.names = paste(rep("V",30),c(1:30),sep=""))
    
    #locate header for section we want
    #identify row number for the section, photo data precede by the heading "wpt"
    metahead<-which(GarminIP$V1=="wpt")
    #photo data procede by the heading "Address"
    metaend<-which(GarminIP$V1=="Address")
    
    photometa<-GarminIP[(metahead+1):(metaend-1),]
    colnames(photometa)<-photometa[1,]
    photometa<-photometa[-1,]
    
    #select data required from input
    photometa.sub<-data.frame(photometa$name,photometa$lat,photometa$lon,photometa$time)
    accesshead<-c("PhotoNames","Latitude","Longitude","Sighted_DatTime")
    names(photometa.sub)<-accesshead
    
    #cleaning date/time field to remove extra character
    photometa.sub$dtclean<-gsub("[T,Z]"," ",photometa.sub$Sighted_DatTime)
    #convert date/time field to UNIX time in GMT time szone
    photometa.sub$GMTtime<-as.POSIXct(photometa.sub$dtclean, tz="GMT")
    #convert GMT time to hobart local time and change format to Access accepted format
    #note- this should automatically adjust based on day light hour
    photometa.sub$AEDTtime<-format(photometa.sub$GMTtime, tz="Australia/Hobart", 
                                   usetz = FALSE, "%d/%m/%Y %H:%M")

    #clean df for prepration for export
    dropcol<-c("Sighted_DatTime","dtclean","GMTtime")
    AccessOP.sub<-photometa.sub%>%
      select(-dropcol)%>%
      rename(Sighted_DateTime=AEDTtime)
    
    AccessOP<-rbind(AccessOP,AccessOP.sub)
    } else if (!is_empty(CsvSS)==FALSE){
    
  }
}


#Exporting file to designated folder
#auto generating output name with accessed date as prefix
opname<-paste(as.character(format(Sys.Date(),"%Y%m%d")),
              "AccdbPhotoInput.csv",sep="_")
#writing to HF data analysis folder
write.csv(AccessOP,row.names = FALSE,
          file=file.path(netdir,"Handfish_Data_analysis","Photo",
                         opname))



