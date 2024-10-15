#Load packages in R and import data
library("tibble")
library("survival")
library("survminer")
library("ggsci")
library("lme4")
library("Matrix")
library("gridExtra")
ttd_all<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_all_fall<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_all_spring<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_all_summer<-read.csv(file=file.choose(),stringsAsFactors=TRUE)
ttd_all_winter<-read.csv(file=file.choose(),stringsAsFactors=TRUE)

#Log rank test for each explanatory variable
survreg(Surv(time,status)~season+baitlure,cluster=site,data=ttd_all)
survreg(Surv(time,status)~season+baittype,cluster=site,data=ttd_all)
survreg(Surv(time,status)~season,cluster=site,data=ttd_all)

#Reorder levels of each explanatory variable and create survfit object
	#Baitlure
	blpf<-ttd_all_fall%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
	  as_tibble()
	blf<-survfit(Surv(time,status)~baitlure.c,cluster=site,data=blpf)
	
	blpsp<-ttd_all_spring%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	blsp<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=blpsp)

	blpsu<-ttd_all_summer%>%
 	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	blsu<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=blpsu)

	blpw<-ttd_all_winter%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	blw<-survfit(Surv(time,status)~season+baitlure.c,cluster=site,data=blpw)

	#Baittype
	btpf<-ttd_all_fall%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	btf<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=btpf)

	btpsp<-ttd_all_spring%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	btsp<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=btpsp)

	btpsu<-ttd_all_summer%>%
  	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	btsu<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=btpsu)

	btpw<-ttd_all_winter%>%
 	  mutate(baittype.c=ordered(baittype,c("beaver","paste","none")))%>%
  	  as_tibble()
	btw<-survfit(Surv(time,status)~season+baittype.c,cluster=site,data=btpw)

	#Season
	blp<-ttd_all%>%
  	  mutate(baitlure.c=ordered(baitlure,c("yes","no")))%>%
  	  as_tibble()
	ssn<-survfit(Surv(time,status)~season,cluster=site,data=ttd_all)

#Create plots for explanatory variables and arrange them in a figure
	#Baitlure
	blallplot1<-ggsurvplot(blf,
                     title = "fall",
                     fun="event",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), 
                     palette = c("#E7B800", "#2E9FDF"))
	blallplot2<-ggsurvplot(blsp,
	                   title = "spring",
	                   fun="event",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), 
                     palette = c("#E7B800", "#2E9FDF"))
	blallplot3<-ggsurvplot(blw,
                     title = "winter",
                     fun="event",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(),
                     palette = c("#E7B800", "#2E9FDF"))
	blallplot4<-ggsurvplot(blsu,
                     fun="event",
                     title = "summer",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "",
                     legend.labs = c("Bait & Lure", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), 
                     palette = c("#E7B800", "#2E9FDF"))
	
	blallplots<-list(blallplot1,blallplot2,blallplot3,blallplot4)
	arrange_ggsurvplots(blallplots,print=TRUE,
                    ncol=2,nrow=2)

	#Baittype
	btallplot1<-ggsurvplot(btf,
                     fun="event",
                     title = "fall",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btallplot2<-ggsurvplot(btsp,
                     fun="event",
                     title = "spring",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btallplot3<-ggsurvplot(btw,
                     title = "winter",
                     fun="event",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))
	btallplot4<-ggsurvplot(btsu,
                     fun="event",
                     title = "summer",
                     conf.int = TRUE,
                     linetype = "strata",
                     legend.title = "Bait type",
                     legend.labs = c("Beaver", "Paste", "None"),
                     xlab="Time to Detection (Days)",
                     ylab="Probability of Weasel Detection",
                     ggtheme = theme_bw(), # Change ggplot2 theme
                     palette = c("#E7B800", "#009E73", "#2E9FDF"))

	btallplots<-list(btallplot1,btallplot2,btallplot3,btallplot4)
	arrange_ggsurvplots(btallplots,print=TRUE,
                    ncol=2,nrow=2)

	#Season
	ggsurvplot(ssn,
           fun="event",
           linetype = "strata",
	         title = "c)",
           conf.int = TRUE,
	         legend.title = "",
           legend.labs = c("fall", "spring", "summer", "winter"),
           xlab="Time to Detection (Days)",
           ylab="Probability of Weasel Detection",
           ggtheme = theme_bw(),
           palette = c("#999999", "#E7B800", "#009E73", "#2E9FDF"))
