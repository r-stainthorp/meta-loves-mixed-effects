### unedited!!!! Bates code for mixed effects modelling

m1=lme(r1_measure ~ t_deg  * phylum + I(t_deg^2)*o2_sat,random=~1|source.year,weights=varIdent(form=~1|source.year),data=r1,method="ML")
m2=lme(r1_measure ~ t_deg * o2_sat + phylum + I(t_deg^2)*o2_sat,random=~1|source.year,weights=varIdent(form=~1|source.year),data=r1,method="ML")
m3=lme(r1_measure ~ t_deg * o2_sat + phylum + I(t_deg^2),random=~1|source.year,weights=varIdent(form=~1|source.year),data=r1,method="ML")

newdata1<-data.frame(t_deg=seq(min(r1$t_deg), max(r1$t_deg), length.out=100),o2_sat=100,phylum="chordata")
pred<-predict(m1,newdata=newdata1,level=0)
plot(pred~newdata1$t_deg, pch=16,lwd=4, type="l", lty=1, xlab="t_deg", ylab="Model Predicted Coefficients",col="orange",cex=1.8,cex.axis=1.5, cex.lab=1.6)

newdata2<-data.frame(t_deg=seq(min(r1$t_deg), max(r1$t_deg), length.out=100),o2_sat=10,phylum="chordata")
pred1<-predict(m1,newdata=newdata2,level=0)
lines(newdata2$t_deg,pred1,col="black",lwd="4",lty=2)