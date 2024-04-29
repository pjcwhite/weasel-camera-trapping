#Import data to R and calculate averages
nphoto<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
summary(object=nphoto$avg.photo)
mean(x=nphoto$avg.photo)
sd(x=nphoto$avg.photo)
summary(object=nphoto$high.photo)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for bait & lure vs. none
bl.model<-glm(formula=nphoto$avg.photo~nphoto$baitlure)
shapiro.test(x=residuals(object=bl.model))
bl.model<-glm(formula=log(x=nphoto$avg.photo)~nphoto$baitlure)
shapiro.test(x=residuals(object=bl.model))
bl.model<-glm(formula=sqrt(x=nphoto$avg.photo)~nphoto$baitlure)
shapiro.test(x=residuals(object=bl.model))
wilcox.test(formula=nphoto$avg.photo~nphoto$baitlure)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for two types of bait vs.none
baittype.model<-glm(formula=nphoto$avg.photo~nphoto$baittype)
shapiro.test(x=residuals(object=baittype.model))
baittype.model<-glm(formula=log(x=nphoto$avg.photo)~nphoto$baittype)
shapiro.test(x=residuals(object=baittype.model))
baittype.model<-glm(formula=sqrt(x=nphoto$avg.photo)~nphoto$baittype)
shapiro.test(x=residuals(object=baittype.model))
kruskal.test(formula=nphoto$avg.photo~nphoto$baittype)

#Test for normal distribution, perform data transformation, and generate P-value for three types of lure vs.none
luretype.model<-glm(formula=nphoto$avg.photo~nphoto$luretype)
shapiro.test(x=residuals(object=luretype.model))
luretype.model<-glm(formula=log(x=nphoto$avg.photo)~nphoto$luretype)
shapiro.test(x=residuals(object=luretype.model))
anova(object=glm(formula=log(x=nphoto$avg.photo)~nphoto$luretype),test="F")

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for bait and lure type combinations vs.none
baitluretype.model<-glm(formula=nphoto$avg.photo~nphoto$baitluretype)
shapiro.test(x=residuals(object=baitluretype.model))
baitluretype.model<-glm(formula=log(x=nphoto$avg.photo)~nphoto$baitluretype)
shapiro.test(x=residuals(object=baitluretype.model))
baitluretype.model<-glm(formula=sqrt(x=nphoto$avg.photo)~nphoto$baitluretype)
shapiro.test(x=residuals(object=baitluretype.model))
kruskal.test(formula=nphoto$avg.photo~nphoto$baitluretype)

#Test for normal distribution, attempt data transformations, and create a non-parametric alternative model for Kill Squeak vs. none
ks.model<-glm(formula=nphoto$avg.photo~nphoto$KS)
shapiro.test(x=residuals(object=ks.model))
ks.model<-glm(formula=log(x=nphoto$avg.photo)~nphoto$KS)
shapiro.test(x=residuals(object=ks.model))
ks.model<-glm(formula=sqrt(x=nphoto$avg.photo)~nphoto$KS)
shapiro.test(x=residuals(object=ks.model))
wilcox.test(formula=nphoto$avg.photo~nphoto$KS)