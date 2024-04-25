#Import data to R and calculate averages
qphoto<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
summary(object=qphoto$qual)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for bait & lure vs. none
qbl.model<-glm(formula=qphoto$qual~qphoto$baitlure)
shapiro.test(x=residuals(object=qbl.model))
qbl.model<-glm(formula=log(x=qphoto$qual)~qphoto$baitlure)
shapiro.test(x=residuals(object=qbl.model))
qbl.model<-glm(formula=sqrt(x=qphoto$qual)~qphoto$baitlure)
shapiro.test(x=residuals(object=qbl.model))
wilcox.test(formula=qphoto$qual~qphoto$baitlure)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for two types of bait vs. none
qbt.model<-glm(formula=qphoto$qual~qphoto$baittype)
shapiro.test(x=residuals(object=qbt.model))
qbt.model<-glm(formula=log(x=qphoto$qual)~qphoto$baittype)
shapiro.test(x=residuals(object=qbt.model))
qbt.model<-glm(formula=sqrt(x=qphoto$qual)~qphoto$baittype)
shapiro.test(x=residuals(object=qbt.model))
kruskal.test(formula=qphoto$qual~qphoto$baittype)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for three types of lure vs. none
qlt.model<-glm(formula=qphoto$qual~qphoto$luretype)
shapiro.test(x=residuals(object=qlt.model))
qlt.model<-glm(formula=log(x=qphoto$qual)~qphoto$luretype)
shapiro.test(x=residuals(object=qlt.model))
qlt.model<-glm(formula=sqrt(x=qphoto$qual)~qphoto$luretype)
shapiro.test(x=residuals(object=qlt.model))
kruskal.test(formula=qphoto$qual~qphoto$luretype)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for Kill Squeak vs. none
qks.model<-glm(formula=qphoto$qual~qphoto$KS)
shapiro.test(x=residuals(object=qks.model))
qks.model<-glm(formula=log(x=qphoto$qual)~qphoto$KS)
shapiro.test(x=residuals(object=qks.model))
qks.model<-glm(formula=sqrt(x=qphoto$qual)~qphoto$KS)
shapiro.test(x=residuals(object=qks.model))
wilcox.test(formula=qphoto$qual~qphoto$KS)

#Reorder levels of explanatory variable and plot
qphoto$luretype<-factor(qphoto$luretype,levels=c("none", "gusto", "supreme", "salmon"))
par(mfrow=(c(1,2)))
plot(formula=qphoto$qual~qphoto$baitlure,xlab="Bait and Lure Use",ylab="Photo Clarity",main="a)")
plot(formula=qphoto$qual~qphoto$luretype,xlab="Lure Type",ylab="Photo Clarity",main="b)")