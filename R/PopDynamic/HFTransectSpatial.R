#######
#Generating line shapefile from hf transect
#By Lincoln Wong
#Date: 20-06-2020
#######
library(tidyverse)
library(chron)
library(sp)
library (rgdal)
library(rgeos)

library(tmap)
library(raster)
#finding the current directory
cwd<-getwd()
ccom<-regmatches(cwd,regexpr("^\\D{2}\\/\\w+\\/{1}\\D[^/]+\\/{1}",cwd))


#Section 1 - Converting files that are separated (ie same dive day but tracks on seperate csv files) -----
## -- Defining function "mergetran" for joining the two files, organising by time
mergetran <- function (file1,file2,opdate){
  file1dat<-read.csv(file1)
  TN<-unique(file1dat$TRACK.NUMBER)
  TNlen<-sum(!is.na(TN))
  TNO1<- c()
  for (i in 1:TNlen) {
    len<-file1dat %>% 
      filter(TRACK.NUMBER == i)%>%
      count()
    TRACK.NO.NEW.CODE<-rep((TN[i]+10),len)
    TNO1<-c(TNO1,TRACK.NO.NEW.CODE)
  }
  file1dat<-cbind(file1dat,TNO1)
  
  file2dat<-read.csv(file2)
  TN<-unique(file2dat$TRACK.NUMBER)
  TNlen<-sum(!is.na(TN))
  TNO1<- c()
  for (i in 1:TNlen) {
    len<-file2dat %>% 
      filter(TRACK.NUMBER == i)%>%
      count()
    TRACK.NO.NEW.CODE<-rep((TN[i]+20),len)
    TNO1<-c(TNO1,TRACK.NO.NEW.CODE)
  }
  file2dat<-cbind(file2dat,TNO1)
  
  filemerge<-rbind(file1dat,file2dat)
  
  filemerge<-filemerge %>%
    unite("DateTime",LOCAL.DATE,LOCAL.TIME, sep=" ",remove=FALSE)
  
  filemerge$DateTime<-as.POSIXct(filemerge$DateTime)
  filemerge<-filemerge[order(filemerge$DateTime),]
  
  fmseq<-unique(filemerge$TNO1)
  fmlen<-sum(!is.na(fmseq))
  
  fmfin<-data.frame()
  for (i in 1:fmlen){
    pos<-fmseq[i]
    fmreorg<-filemerge %>%
      filter(TNO1 == pos)%>%
      mutate(TRACK.NUMBER=i)
    fmfin<-rbind(fmfin,fmreorg)  
  }
  
  dropcol=c("DateTime", "TNO1")
  fmfin<- fmfin %>%
    dplyr::select(-dropcol)
  
  opname<-paste(opdate,"merge.csv",sep="_")
  write.csv(fmfin,opname,row.names = FALSE)
}

## -- looping function for files required for conversion (only required to run one time)
## -- only conducted for 2015
wd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database/HistoricTrack"
year<-"2016"
setwd(file.path(ccom,wd,year))

filelist<-Sys.glob("*.csv")

setwd(file.path(ccom,wd))
fname<-paste("csvfile",year,".csv",sep="")
write.csv(filelist,fname,row.names = FALSE)

ff1<-c("20160824_Track_GPS_MAB-D6-HMB-D1.csv")
ff2<-c("20160824_Track_GPS_TRD2-D4.csv")
opd<-c("20160824")

setwd(file.path(ccom,wd,year))
for (i in 1){
  mergetran(ff1[i],ff2[i],opd[i])
}



#Section 2 -Importing transect master file from access database#####

swd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database"
wd<-file.path(ccom,swd)
setwd(wd)


metadat<-read.csv("HFTran.csv")

metadat$Site<-as.factor(metadat$Site)
SiteS<-c(NA,"BP","BR","FB","HMB",'HMB',"HB","MAB","OP","PS","RB","RB","SB","SP","TR")
levels(metadat$Site)<-SiteS

rmlist<-c(0,9999)
metadat<- metadat %>%
  filter(!Dive_no %in% rmlist)


metadat <- metadat %>% 
  unite("Filecode2",Site,Dive_no, sep="_D",remove=FALSE) %>%
  unite("Filecode",Filecode2,Transect_no,sep="_T",remove=FALSE)%>%
  dplyr::select(-c(Filecode2))

metadat$Date<-as.Date(metadat$Date, "%d/%m/%Y")


#Section 3 - loading all transect for the year and merging to one df####

## -- define a function to match the transect file to the recorded hf transect id based on date
tranjoin<-function(tf){

trandat<-read.csv(tf)

droplist<-c("UTC.DATE","UTC.TIME","LATITUDE","N.S","LONGITUDE","E.W","ALTITUDE","SPEED")

trandat$N.S<-ifelse(trandat$N.S=="N",1,-1)
trandat$E.W<-ifelse(trandat$E.W=="E",1,-1)

trandat <- trandat %>%
  mutate(long=LONGITUDE*as.integer(E.W), lat=LATITUDE*as.integer(N.S))%>%
  dplyr::select(-droplist)


trandate<-levels(as.factor(trandat$LOCAL.DATE))
ymd<- "\\d{4}\\D+\\d{1,2}\\D+\\d{1,2}"
dmy<-"\\d{1,2}\\D+\\d{1,2}\\D+\\d{4}"
if (str_detect(trandate,ymd)==TRUE){
  trandate<-as.Date(trandate, "%Y/%m/%d")
}else if (str_detect(trandate,dmy)==TRUE){
  trandate<-as.Date(trandate, "%d/%m/%Y")
}

metamatch<-metadat%>%
  filter(Date==trandate)

FC<-unique(metamatch$Filecode)
flen<-sum(!is.na(FC))

Filename<- c()
for (i in 1:flen) {
  len<-trandat %>% 
    filter(TRACK.NUMBER == i)%>%
    count()
  Filecomp<-rep(FC[i],len)
  Filename<-c(Filename,Filecomp)
}
trandat<-cbind(trandat,Filename)
}

## -- only doing this for one year 2015
wd2<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database/HistoricTrack"

setwd(file.path(ccom,wd2,year))

## -- load all file from wd and running function

alltranlist<-Sys.glob("*.csv") #listing all csv
alltranlen<-sum(!is.na(alltranlist)) #find the length of the list

alltran15<-data.frame() #final product

for (i in 1:alltranlen){
  trancomp15<-tranjoin(alltranlist[i])
  alltran15<-rbind(alltran15,trancomp15)
}

site<-regmatches(alltran15$Filename,regexpr("\\D{2,3}",alltran15$Filename))
alltran15$site<-site


opwd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database/HFSpatial/Transect"
opwdfull<-file.path(ccom,opwd)
setwd(opwdfull)
fname<-paste("Alltran",year,".csv",sep="")
write.csv(alltran15, fname, row.names = FALSE)

# Section 4 - generating spatial file from df #####
## Generating transect spatial file
#### -- shortcut - loading completed df file for 2015
swd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database/HFSpatial/Transect"
setwd(file.path(ccom,swd))
alltran15<-read.csv("Alltran15.csv")

#### -- creating filter for BP and SB site only 
alltran15<- alltran15 %>%
  dplyr::filter(site == c("BP_","SB_"))
crs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
tranlength<-length(unique(alltran15$Filename))
Fname<-unique(alltran15$Filename)
tcount<-1:tranlength

sdf<-data.frame(ID=tcount,TranName=Fname)
sdf<- sdf %>%
  mutate(site=regmatches(sdf$TranName,regexpr("\\D{2,3}",sdf$TranName)))
tranbuffer<-list()
transp<-list()
for (i in 1:tranlength){
  idtran<-alltran15%>%
    filter(Filename==Fname[i])
  m<-data.matrix(idtran[5:6])
  mline<-Line(m)
  indline<-Lines(mline, ID=Fname[i])
  transp[i]<-indline
}  

tSpL<-SpatialLines(transp,proj4string = crs)
tSpLpro<-spTransform(tSpL, CRS("+init=epsg:28355"))

rownames(sdf)<-sapply(1:length(tSpLpro),function(i) tSpLpro@lines[[i]]@ID)
TranSpDf<-SpatialLinesDataFrame(tSpLpro,sdf)



Tranpoly<-gBuffer(TranSpDf, byid=TRUE, width=rep(1.5,tranlength), capStyle = "FLAT")

Tranbuffcom<-gUnaryUnion(Tranpoly, id=Tranpoly$site)
dummydf<-data.frame(ID=c(1,2),Year=rep(as.numeric(year),2), site=c("BP","SB"))
rownames(dummydf)<-sapply(1:length(Tranbuffcom),function(i) Tranbuffcom@polygons[[i]]@ID)
Tranpolydf<-SpatialPolygonsDataFrame(Tranbuffcom,dummydf)

#### -- Importing Mooring and creating impacted buffer
moorwd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/Ch1-Engineering/ClearenceModelling"
setwd(file.path(ccom,moorwd))

moorfield<-read.csv("ESpresent.csv")

droplist<-c("X","Site","Len_Boat","Len_Apr","Len_new","ESYN")

moorfield<-moorfield  %>%
  dplyr::select(-droplist)

moorcoor<-data.matrix(moorfield[,3:4])
moorspatial<-SpatialPointsDataFrame(moorcoor,moorfield, proj4string = CRS("+init=epsg:28355"))

imzon<-moorfield$Depth*2
moorzone<-gBuffer(moorspatial, byid=TRUE, width = imzon )

moorzonecom<-rgeos::gUnaryUnion(moorzone)

dummydf<-data.frame(ID=1,Year=as.numeric(2015))
rownames(dummydf)<-sapply(1,function(i) moorzonecom@polygons[[i]]@ID)
moorbufdf<-SpatialPolygonsDataFrame(moorzonecom,dummydf)

#### -- Creating spatial file for HandfishCount
######## Due to error with access database, to ensure accuracy, we need to manually join the fish obs
######## file with photocoor file (with coordinates) based on the sighted datetime

hfwd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database"
setwd(file.path(ccom,hfwd))

hfobs<-read.csv("tblFishObs.csv",na.strings=c("","NA"))
hfcoor<-read.csv("tblPhotoCoor.csv",na.strings=c("","NA"))

dropcol<-c("Comment","Site_Name","Latitude","Longitude","Fish_Flighty")

hfobs<- hfobs %>%
  drop_na(Sighted_DateTime)%>%
  dplyr::select(-dropcol)
  

hfobsdate<-regmatches(hfobs$Sighted_DateTime, regexpr("^\\S+",hfobs$Sighted_DateTime))
hfobstime<-regmatches(hfobs$Sighted_DateTime, regexpr("\\S+$",hfobs$Sighted_DateTime))
hfobstd<-data.frame(hfobsdate,hfobstime)

hftd<-chron(dates=hfobstd[,1],times=hfobstd[,2],format=c('d/m/y','h:m:s'))
hfobs$newtd<-hftd

hfcoor<- hfcoor%>%
  drop_na(Latitude)

hfcoordate<-regmatches(hfcoor$Sighted_DateTime, regexpr("^\\S+",hfcoor$Sighted_DateTime))
hfcoortime<-regmatches(hfcoor$Sighted_DateTime, regexpr("\\S+$",hfcoor$Sighted_DateTime))
hfcoortd<-data.frame(hfcoordate,hfcoortime)

hfctd<-chron(dates=hfcoortd[,1],times=hfcoortd[,2],format=c('d/m/y','h:m:s'))

hfcoor$newtd<-hfctd

hfobsnew <- inner_join(hfobs,hfcoor,by="Sighted_DateTime")
hfobsnew <- hfobsnew %>%
  distinct(Fish_Obs_ID, .keep_all = TRUE)


hfjoined<- left_join(hfobsnew,metadat,by="Transect_ID")


hfjoined$year<-as.Date(hfjoined$Date)
hfjoined$year<-format(hfjoined$year,"%Y")

hfobs15<- hfjoined%>%
  filter(year==2015)

droplist<-c("newtd.x","ï..PhotoNames","Latitude","Longitude","newtd.y","Dive_no","Transect_no","Start","End","Length")

hfobsclean<-hfobs15 %>%
  dplyr::select(-droplist)

hflatlong<-data.matrix(hfobs15[,9:8])

crs<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
hfsp<-SpatialPointsDataFrame(hflatlong,hfobsclean, proj4string = crs)

hfsp<-spTransform(hfsp, CRS("+init=epsg:28355"))


#### -- geoprocessing for analysis
######## -- geoprocess for combinded site
moorintec<-gIntersection(moorzonecom,Tranpolydf, byid = TRUE, id= Tranpolydf$site)
interdf<-data.frame(site=c("BP","SB"))
rownames(interdf)<-sapply(1:length(moorintec),function(i) moorintec@polygons[[i]]@ID)
moorintersect<-SpatialPolygonsDataFrame(moorintec,interdf)
  
hfselected<-raster::intersect(hfsp,moorintersect)



TotSA<-gArea(Tranpolydf,byid=TRUE)
MoorSA<-gArea(moorintersect,byid=TRUE)
OSSA<-TotSA-MoorSA
HFinMoor<-summary(hfselected$Site)
HFinMoor<-HFinMoor[c(1,10)]
HFTot<-summary(hfobs15$Site)
HFTot<-HFTot[c(1,10)]
HFoutside<-HFTot-HFinMoor

sname<-rep(c("InMoor","OutMoor"),each=2)
sumstat<-c(HFinMoor,HFoutside)
ssite<-rep(c("BP","SB"),2)
HFTotrep<-rep(HFTot,2)
SA<-c(MoorSA,OSSA)

Sstatdf<-data.frame(sname,ssite,HFTotrep,sumstat,SA)
Sstatdf$den.m2<-Sstatdf$sumstat/Sstatdf$SA
write.csv(Sstatdf,"")



######## - geoprocess for individual transect
imoorintec<-gIntersection(Tranpoly,moorzonecom, byid = TRUE, id= Tranpoly$TranName)
interdf<-data.frame(tran=Tranpoly$TranName)
rownames(interdf)<-sapply(1:length(imoorintec),function(i) imoorintec@polygons[[i]]@ID)
imoorintersect<-SpatialPolygonsDataFrame(imoorintec,interdf)

ihfselected<-raster::intersect(hfsp,imoorintersect)

iTotSA<-gArea(Tranpolydf,byid=TRUE)
iMoorSA<-gArea(moorintersect,byid=TRUE)
iOSSA<-TotSA-MoorSA
iHFinMoor<-summary(hfselected$Site)
iHFinMoor<-HFinMoor[c(1,10)]
iHFTot<-summary(hfobs15$Site)
iHFTot<-HFTot[c(1,10)]
iHFoutside<-HFTot-HFinMoor

sname<-rep(c("InMoor","OutMoor"),each=2)
sumstat<-c(HFinMoor,HFoutside)
ssite<-rep(c("BP","SB"),2)
HFTotrep<-rep(HFTot,2)
SA<-c(MoorSA,OSSA)

Sstatdf<-data.frame(sname,ssite,HFTotrep,sumstat,SA)
Sstatdf$den.m2<-Sstatdf$sumstat/Sstatdf$SA
write.csv(Sstatdf,"")

#### Creating map using tmap

hfmap<-tmap::tm_shape(Tranpolydf)+tmap::tm_polygons(col="site")+tm_facets(by="site",free.coords = TRUE, ncol = 2)+
       tmap::tm_shape(moorintersect)+tmap::tm_polygons(col="green")+
       tmap::tm_shape(hfsp)+tmap::tm_dots(col="red", size=0.25)+
       tmap::tm_shape(moorzonecom)+tmap::tm_polygons(border.col = "black",alpha=0)+
       tmap::tm_shape(hfselected)+tmap::tm_dots(size=0.3)+
       tmap::tm_scale_bar()
hfmap

tmap_save(hfmap, "hfmap.png",height=10,width = 20)

p<-tmap::tm_shape(Tranpoly)+tmap::tm_polygons(col="TranName")+tm_facets(by="site",free.coords = TRUE)+
  tmap::tm_shape(imoorintec)+tmap::tm_polygons(col="red")+
  tmap::tm_shape(moorzonecom)+tmap::tm_polygons(border.col = "black",alpha=0)
  
tmap_save(p, "transect.png",height=10,width = 20)

#output shapefile----
opwd<-"OneDrive - University of Tasmania/ESMooring-PhD 2019/HF Database/HFSpatial/Shapefile"
setwd(file.path(ccom,opwd))

#hfbuffer
writeOGR(Tranpolydf,dsn="HF15TranBuff.shp",layer="HF15TranBuff.shp" ,driver="ESRI Shapefile")
#moorbuff
writeOGR(moorbufdf,dsn="HF15MoorBuff.shp",layer="HF15MoorBuff.shp" ,driver="ESRI Shapefile")
#HFlocale
writeOGR(hfsp,dsn="HF15pos.shp",layer="HF15pos.shp" ,driver="ESRI Shapefile")
#PolyGrid-BatteryPointSandyBay
rgdal::writeOGR(GridRefdf, dsn="BPSBpolygrid.shp",layer="BPSBpolygrid.shp",driver = "ESRI Shapefile")


#Section5 - Creating polygrid ----

pgrid<-data.frame()

bblat<-c(5249300,5251400)
bblon<-c(526900,528900)
glen<-100


corlist<-data.frame(matrix(ncol=2, nrow=0))

pblat<-bblat/100
pblon<-bblon/100


for (i in pblat[1]:pblat[2]){
  for (j in pblon[1]:pblon[2]){
    newlat<-c(i,j)
    corlist<-rbind(corlist,newlat)
  }
}

corlist<-corlist*100
colname<-c("lat","long")
colnames(corlist)<-colname

cllen<-sum(!is.na(corlist$lat))

gsize<-99.999
for (i in 1:cllen){
  xmin<-corlist$long[i]
  xmax<-xmin+gsize
  ymin<-corlist$lat[i]
  ymax<-ymin+gsize
  pgrid<-rbind(pgrid,c(xmin,xmax,ymin,ymax))
}

pgname<-c("xmin","xmax","ymin","ymax")
colnames(pgrid)<-pgname

countlat<-(pblat[2]-pblat[1])+1
countlon<-(pblon[2]-pblon[1])+1

CID<-paste(rep(1:countlon,countlat),rep(1:countlat,each=countlon),sep="_")
pgrid<-cbind(CID,pgrid)

psplist<-list()

for (i in 1:cllen){
  polyss<-pgrid%>%
    dplyr::filter(pgrid$CID == CID[i])
  polylong<-c(polyss[[2]],polyss[[2]],polyss[[3]],polyss[[3]],polyss[[2]])
  polylat<-c(polyss[[4]],polyss[[5]],polyss[[5]],polyss[[4]],polyss[[4]])
  polycor<-sp::Polygon(cbind(polylong,polylat))
  polyspatial<-sp::Polygons(list(polycor), ID=CID[i])
  psplist[i]<-polyspatial
}

crs<-CRS("+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
PolySpL<-SpatialPolygons(psplist,proj4string = crs)

polydf<-data.frame(ID=1:cllen,GridRef=CID)
rownames(polydf)<-sapply(1:cllen,function(i) PolySpL@polygons[[i]]@ID)
GridRefdf<-SpatialPolygonsDataFrame(PolySpL,polydf)



#old code-----
#old mapping code
library(ozmaps)
library(ggspatial)
library(raster)
library(sf)
library(tmap)

mga55<-"+init=epsg:28355"
ggplot2::ggplot()+
  ggspatial::geom_spatial_polygon(data=Tranbuffcom, mapping=aes(colour="black"))+
  coord_sf(crs=mga55)
+
  
  
  
  
  blat<-c(min(alltran15$lat),max(alltran15$lat))
blong<-c(min(alltran15$long),max(alltran15$long))


oz_states <- ozmaps::ozmap_states
ggplot(oz_states) + 
  geom_sf(data = oz_states, colour = "black", fill = "lightblue") + 
  geom_rect(data=pgrid, mapping=aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="red")+
  coord_sf(xlim = c(blong[1],blong[2]), ylim = c(blat[1],blat[2]))
