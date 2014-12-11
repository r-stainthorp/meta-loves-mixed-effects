# load .csv file
meta <- read.csv("lat_long.csv", header=TRUE)
names(meta)

#### MAKING A MAP

library(maps)

# set country lines to grey
par(fg="grey")
map("world",cex=1,fill=TRUE,col="grey",ylim=c(-80,80),xlim=c(-180,180))

# add latitude and longitude from your data file
points(data=meta,source_lat~source_long,col="black",ylim=c(-80,80),xlim=c(-180,180)) 

points(data=meta,lat~long,col="red",ylim=c(-80,80),xlim=c(-180,180)) 

# subset data for one column and just include growth rate
grate<-subset(meta,meta$r1=="growth rate") 
# look at distribution
hist(grate$r1_measure)

# subset data for one column and just include respiration rate
o2<-subset(meta,meta$r1=="respiration rate")
# look at distribution
hist(o2$r1_measure)

#### MAKING A MIXED EFFECTS MODEL

library(nlme)
library(plyr)
library(reshape2)

# look at relationship between growth rate and experimental temperature
plot(data=grate,r1_measure~temp_deg)

# display structure of "temp_deg" column in grate object
str(grate$temp_deg)

# set structure of "temp_deg" column to numeric
as.numeric(grate$temp_deg)

# fit a linear model using generalised least squares
model1<-gls(r1_measure~temp_deg,data=grate)
plot(model1)

# produce a normal QQ plot of the model
qqnorm(model1)

# fit a linear mixed-effects model (allows for nested random effects)
model2<-lme(log(r1_measure+4)~temp_deg,data=grate,random=~1|source) # add random effect
plot(model2)

# produce a normal QQ plot of the model
qqnorm(model2)

# try "+4" to remove negative values
plot(data=grate,log(r1_measure+4)~temp_deg)

# fit a linear mixed-effects model adding an interaction between temp and o2
model3<-lme(log(r1_measure+4)~temp_deg*o2_sat,data=grate,random=~1|source)
plot(model3)

summary(model3)

qqnorm(model3)

