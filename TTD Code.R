#Load packages in R and import data
library("tibble")
library("survival")
library("survminer")
library("ggsci")
library("lme4")
library("Matrix")
library("gridExtra")
ttd<-read.csv(file=file.choose(),stringsAsFactors=TRUE)

#Log rank test for each explanatory variable
survreg(Surv(time,status)~baitlure,cluster=site,data=ttd)
survreg(Surv(time,status)~baittype,cluster=site,data=ttd)
survreg(Surv(time,status)~luretype,cluster=site,data=ttd)
survreg(Surv(time,status)~baitluretype,cluster=site,data=ttd)
survreg(Surv(time,status)~KS,cluster=site,data=ttd)

#Reorder levels of each explanatory variable with a significant result and create survfit object
blp<-ttd%>%
  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  as_tibble()
bln<-survfit(Surv(time,status)~baitlure.c,cluster=site,data=blp)

btp<-ttd%>%
  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  as_tibble()
bt<-survfit(Surv(time,status)~baittype.c,cluster=site,data=btp)

#Create plots for explanatory variables and arrange them in a figure
plot1<-ggsurvplot(bln,
                  pval = 0.036, conf.int = TRUE,
                  pval.coord=c(1,0.5),
                  pval.size=4.5,
                  fun="event",
                  linetype = "strata",
                  legend.title = "",
                  legend.labs = c("Bait & Lure", "None"),
                  xlab="Time to Detection (Days)",
                  ylab="Probability of Weasel Detection",
                  title="a)",
                  ggtheme = theme_bw(), 
                  palette = c("#E7B800", "#2E9FDF"))

plot2<-ggsurvplot(bt,
                  pval = 0.023,
                  pval.coord=c(1,0.5),
                  pval.size=4.5,
                  fun="event",
                  linetype = "strata",
                  legend.title = "Bait type",
                  legend.labs = c("Beaver", "Paste", "None"),
                  xlab="Time to Detection (Days)",
                  ylab="Probability of Weasel Detection",
                  title= "b)",
                  ggtheme = theme_bw(), 
                  palette = c("#E7B800", "#009E73", "#2E9FDF"))

plots<-list(plot1,plot2)
arrange_ggsurvplots(plots,print=TRUE,
                    ncol=2,nrow=1)
