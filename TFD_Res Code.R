#Load packages in R and import data
library("tibble")
library("survival")
library("survminer")
library("ggsci")
library("lme4")
library("Matrix")
library("gridExtra")
ttd_res<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_res_fall<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_res_spring<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_res_summer<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_res_winter<-read.csv(file=file.choose(),stringsAsFactors=TRUE)

#Log rank test for each explanatory variable
survreg(Surv(time,status)~season+baitlure,cluster=site,data=ttd_res)
survreg(Surv(time,status)~season+baittype,cluster=site,data=ttd_res)

#Reorder levels of each explanatory variable and create survfit object
	#Baitlure
	rblpf<-ttd_res_fall%>%
 	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	rblf<-survfit(Surv(time,status)~baitlure.c,cluster=site,data=rblpf)
	
	rblpsp<-ttd_res_spring%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	rblsp<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=rblpsp)

	rblpsu<-ttd_res_summer%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	rblsu<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=rblpsu)

	rblpw<-ttd_res_winter%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	rblw<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=rblpw)

	#Baittype
	rbtpf<-ttd_res_fall%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	rbtf<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=rbtpf)

	rbtpsp<-ttd_res_spring%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
 	  as_tibble()
	rbtsp<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=rbtpsp)

	rbtpsu<-ttd_res_summer%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	rbtsu<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=rbtpsu)

	rbtpw<-ttd_res_winter%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	rbtw<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=rbtpw)

#Create plots for explanatory variables and arrange them in a figure
	#Baitlure
	blresplot1<-ggsurvplot(rblf,
                     title = "fall",
                     fun="event",
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#2E9FDF"))
	blresplot2<-ggsurvplot(rblsp,
                     fun="event",
                     title = "spring",
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#2E9FDF"))
	blresplot3<-ggsurvplot(rblw,
                     title = "winter",
                     fun="event",
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#2E9FDF"))
	blresplot4<-ggsurvplot(rblsu,
                     fun="event",
                     title = "summer",
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#2E9FDF"))
	
	blresplots<-list(blresplot1,blresplot2,blresplot3,blresplot4)
	arrange_ggsurvplots(blresplots,print=TRUE,
                    ncol=2,nrow=2)

	#Baittype
	btresplot1<-ggsurvplot(rbtf,
                     fun="event",
                     title = "fall",
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), 
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btresplot2<-ggsurvplot(rbtsp,
                     fun="event",
                     title = "spring",
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btresplot3<-ggsurvplot(rbtw,
                     title = "winter",
                     fun="event",
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btresplot4<-ggsurvplot(rbtsu,
                     fun="event",
                     title = "summer",
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	
	btresplots<-list(btresplot1,btresplot2,btresplot3,btresplot4)
	arrange_ggsurvplots(btresplots,print=TRUE,
                    ncol=2,nrow=2)
