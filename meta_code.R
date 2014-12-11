#### MATCHING BIOORACLE DATA TO METADATA FILE

meta<-read.csv("",header=TRUE)

# read from ESRI asciigrid format; extract mean sea-surface temperature
gridtemp <- readAsciiGrid("~/Desktop/roshan study/BioOracle_7070RV/sstmean.asc")
# create data frame
tabletemp <- data.frame(gridtemp)

gridtempmax <- readAsciiGrid("~/Desktop/roshan study/BioOracle_7070RV/sstmax.asc")
tabletempmax <- data.frame(gridtempmax)

alldata=read.csv("occurence2.csv")
sites=ddply(alldata,c("Site"),summarize,SiteLat=mean(SiteLat),SiteLong=mean(SiteLong))

SST=matrix(0,dim(sites)[1])
for (a in 1:dim(sites)[1]){
  mins=data.frame(sqrt((abs(sites[a,]$SiteLat+70)-abs(tabletemp$s2+70))^2+(abs(sites[a,]$SiteLong+180)-abs(tabletemp$s1+180))^2))
  SST[a,]=tabletemp[which(mins==(min(mins))),][1,1] 
}

SSTmax=matrix(0,dim(sites)[1])
for (a in 1:dim(sites)[1]){
  mins=data.frame(sqrt((abs(sites[a,]$SiteLat+70)-abs(tabletemp$s2+70))^2+(abs(sites[a,]$SiteLong+180)-abs(tabletemp$s1+180))^2))
  SSTmax[a,]=tabletempmax[which(mins==(min(mins))),][1,1] 
}

####take what you have done and add to original data set