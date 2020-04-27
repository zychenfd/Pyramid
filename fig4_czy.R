library(dplyr)
library(readxl)
library(reshape2)
library(ggplot2)
library(ggpubr)

getwd()
setwd("/Users/liujiangyi/Desktop/2019nCoV/hop_rate/20200312")
#-----------------------20200309-------------------
load("ili_cleaned_2019.dat")

ili$year_new <- as.integer(format(ili$date, "%Y"))
ili$month_new <- as.integer(format(ili$date, "%m"))
head(ili)
ili <- ili[ili$province == "Hubei",]
ili$clinic_new <- NA
ili$clinic_new[ili$clinic == 1 |ili$clinic == 2] <- "internal medicine" #inter med outpat
ili$clinic_new[ili$clinic == 3 |ili$clinic == 4] <- "paediatric"
ili$clinic_new[ili$clinic == 5 ] <- "fever"
sum(is.na(ili$clinic_new))

##divide period
ili_new <- ili
ili_new$period[ili_new$month_new >= 12] <-  "12.01-12.31"
ili_new$period[ili_new$month_new >= 1 & ili_new$month_new <=3] <- "01.01-03.31"
ili_new <- ili_new[!(is.na(ili_new$period)),] 

##select Wuhan and Hubei

# ili_Hubei <- ili_new[ili_new$province == "Hubei",]
ili_Wuhan <- ili_new[ili_new$city.code == 4201,]

# ili_Hubei_tabel <- ili_Hubei %>% group_by(year_new, period, pedi,clinic_new) %>% summarise(No.hos = n_distinct(hospital.code),ILI_0 = sum(age0004),ILI_5 = sum(age0514),
#                                                                                            ILI_15 = sum(age1524),ILI_25 = sum(age2559),
#                                                                                            ILI_60 = sum(age6000),ili.total = sum(ili.total), 
#                                                                                            con.total = sum(con.total))
# 
# ili_Hubei_tabel <- left_join(ili_Hubei_tabel, ili %>% filter(province == "Hubei") %>% group_by(year_new, clinic_new) %>% 
#                                summarise(con.total.year = sum(con.total)))


ili_Wuhan_tabel <- ili_Wuhan %>% group_by(year_new, period, pedi,clinic_new) %>% summarise(No.hos = n_distinct(hospital.code),ILI_0 = sum(age0004),ILI_5 = sum(age0514),
                                                                                           ILI_15 = sum(age1524),ILI_25 = sum(age2559),
                                                                                           ILI_60 = sum(age6000), ili.total = sum(ili.total),
                                                                                           con.total = sum(con.total))

ili_Wuhan_tabel <- left_join(ili_Wuhan_tabel, ili %>% filter(city.code == 4201) %>% group_by(year_new, clinic_new) %>% 
                               summarise(on.total.year = sum(con.total)))

# write.csv(ili_Hubei_tabel, "ili_Hubei_tabel.csv")
# write.csv(ili_Wuhan_tabel, "ili_Wuhan_tabel.csv")


#-----------------------20200310-------------------

####the next step####  ##20200310
####Hubei constitutional ratio of ILI/Outpatinte
ili_Hubei <- ili_Wuhan_tabel
ili_Hubei_tabel_1 <- ili_Hubei %>% group_by(year_new,period,clinic_new) %>% 
  summarise( ili.total = sum(ili.total))
ili_Hubei_tabel_1 <- left_join(ili_Hubei_tabel_1, ili %>% filter(city.code == 4201) 
                               %>% group_by(year_new, clinic_new) %>% summarise(con.total = sum(con.total)))
ili_Hubei_tabel_1$rate <- ili_Hubei_tabel_1$ili.total / ili_Hubei_tabel_1$con.total
ili_Hubei_tabel_1  <- ili_Hubei_tabel_1[!ili_Hubei_tabel_1$year_new %in% c(2005, 2017),]

Hubei_inter <- ili_Hubei_tabel_1[ili_Hubei_tabel_1$clinic_new == "internal medicine",]
# Hubei_inter$x <- c(rep(1:11,each =2))
Hubei_pae <- ili_Hubei_tabel_1[ili_Hubei_tabel_1$clinic_new == "paediatric",]
# Hubei_pae$x <- c(rep(1:11,each =2))
Hubei_fever<- ili_Hubei_tabel_1[ili_Hubei_tabel_1$clinic_new == "fever",]
# Hubei_fever$x <- c(1, rep(2:11,each =2))
Hubei_fever$rate[is.na(Hubei_fever$rate)] <- 0

Hubei_clinics <- ili_Hubei %>% group_by(year_new, clinic_new) %>% summarise(ili.total = sum(ili.total))
Hubei_clinics  <- Hubei_clinics[!Hubei_clinics$year_new %in% c(2005, 2017),]
# Hubei_clinics$x <- c(rep(1:11,each =3))

hubei_PanelCD <- rbind(Hubei_inter,Hubei_pae,Hubei_fever) 
hubei_PanelCD <- hubei_PanelCD[hubei_PanelCD$year_new %in% c(2010:2016),]
hubei_PanelCD$x <- c(rep(1:7,each =2),rep(1:7,each =2),rep(1:6,each =2),7)

#=================fig3 panel C&D
##easy plot the trend of ILI/Outpatient in sentinel hos 

#panel D
hubei_PanelCD$ili.total[is.na(hubei_PanelCD$ili.total)] <- 0
hubei_PanelCD$con.total[is.na(hubei_PanelCD$con.total)] <- 0
hubei_PanelCD1 <- hubei_PanelCD %>% group_by(year_new,period) %>% summarise(ili.total = sum(ili.total),
                                                                            con.total = sum(con.total))
hubei_PanelCD1$clinic_new <- "All"
hubei_PanelCD1$rate <- hubei_PanelCD1$ili.total / hubei_PanelCD1$con.total
hubei_PanelCD1$x <- rep(1:7, each = 2)
hubei_PanelCD2 <- hubei_PanelCD1[,c(1,2,5,3,4,6,7)]
hubei_PanelCD <- rbind(hubei_PanelCD, hubei_PanelCD2)

write.csv(hubei_PanelCD, file = "1.csv")
hubei_PanelCD <- read_excel("1.xlsx")

factor(hubei_PanelCD$clinic_new, 
       levels = c("All","internal medicine","paediatric","fever")) -> hubei_PanelCD$clinic_new

ggplot() + geom_bar(data = hubei_PanelCD[hubei_PanelCD$period == "01.01-03.31",], 
                    aes(x= x, y = rate, fill =clinic_new),
                    stat = "identity", position = "dodge") +
  scale_x_continuous("Year", breaks = seq(1:7),
                     labels = c(2010:2016)) +
  scale_y_continuous("ILI / All consultations (%)", breaks = seq(0, 0.06, 0.02),
                     labels = seq(0,6.0, 2.0),
                     limits = c(0, 0.06))+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 14),
        text = element_text(size = 18),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.5,"lines"),
        # legend.position = c(0.8, 0.9),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_text(vjust = -0.5, hjust = 0.5))+
  # guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("#E6CCBE", "#F8DF99", "#D38C32","#BD5B3C"),
                    name="Department",
                    breaks=c("All","internal medicine","paediatric","fever"),
                    labels=c("All","Internal","Paediatric","Fever")) +
  scale_color_manual(values = c("#E6CCBE", "#F8DF99", "#D38C32","#BD5B3C")) +
  labs(title = "D")-> figD # (1.1-3.31)

"#3288BD", "#FDAE61","#FEE08B","#66C2A5","#ABDDA4"

# write.csv(hubei_PanelCD, file = "wuhanFig4D.csv")
# hubei_PanelCD <- read_excel("wuhanFig4D.xlsx")

#panel C
ggplot() + geom_bar(data = hubei_PanelCD[hubei_PanelCD$period == "12.01-12.31",], 
                    aes(x= x, y = rate, fill =clinic_new),
                    stat = "identity", position = "dodge") +
  scale_x_continuous("Year", breaks = seq(1:7),
                     labels = c(2010:2016)) +
  scale_y_continuous("ILI / All consultations (%)", breaks = seq(0, 0.012, 0.003),
                     labels = seq(0, 1.2, 0.3),
                     limits = c(0, 0.012))+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 14),
        text = element_text(size = 18),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.5,"lines"),
        # legend.position = c(0.8, 0.9),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_text(vjust = -0.5, hjust = 0.5))+
  # guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("#E6CCBE", "#F8DF99", "#D38C32","#BD5B3C"),
                    name="Department",
                    breaks=c("All","internal medicine","paediatric","fever"),
                    labels=c("All","Internal","Paediatric","Fever")) +
  scale_color_manual(values = c("#E6CCBE", "#F8DF99", "#D38C32","#BD5B3C")) +
  labs(title = "C")-> figC # (12.1-12.31)


#------------------20200312---------------
#fig panel B
####comparison of age group within three clinics
load("ili_cleaned_2019.dat")
ili <- ili[ili$province == "Hubei",]
ili$year_new <- as.integer(format(ili$date, "%Y"))
ili$month_new <- as.integer(format(ili$date, "%m"))

ili$clinic_new <- NA
ili$clinic_new[ili$clinic == 1 |ili$clinic == 2] <- "internal medicine" #inter med outpat
ili$clinic_new[ili$clinic == 3 |ili$clinic == 4] <- "paediatric"
ili$clinic_new[ili$clinic == 5 ] <- "fever"
sum(is.na(ili$clinic_new))

ili <- ili[ili$city.code == 4201,]
##paediatric
age_com_pae <- ili %>% filter(year_new > 2009 & year_new < 2017 & clinic_new == "paediatric") %>%
  group_by(year_new) %>% summarise(ILI_0 = sum(age0004),ILI_5 = sum(age0514),
                                   ILI_15 = sum(age1524),ILI_25 = sum(age2559),ILI_60 = sum(age6000),ili.total = sum(ili.total))  %>% 
  mutate(ILI_0_p = ILI_0/ili.total, ILI_5_p = ILI_5/ili.total, ILI_15_p = ILI_15/ili.total,
         ILI_25_p = ILI_25/ili.total,ILI_60_p = ILI_60/ili.total)
age_com_pae <- melt(age_com_pae[,c(1,8:12)], id = c("year_new"))
age_com_pae <- age_com_pae[order(age_com_pae$year_new),]
age_com_pae$x <- c(rep(1,each = 5), rep(4,each = 5), rep(7,each = 5), rep(10,each = 5), rep(13,each = 5),
                   rep(16,each = 5), rep(19,each = 5))
##fever
age_com_fever <- ili %>% filter(year_new > 2009 & year_new < 2017 & clinic_new == "fever") %>%
  group_by(year_new) %>% summarise(ILI_0 = sum(age0004),ILI_5 = sum(age0514),
                                   ILI_15 = sum(age1524),ILI_25 = sum(age2559),ILI_60 = sum(age6000),ili.total = sum(ili.total))  %>% 
  mutate(ILI_0_p = ILI_0/ili.total, ILI_5_p = ILI_5/ili.total, ILI_15_p = ILI_15/ili.total,
         ILI_25_p = ILI_25/ili.total,ILI_60_p = ILI_60/ili.total)
age_com_fever <- melt(age_com_fever[,c(1,8:12)], id = c("year_new"))
age_com_fever <- age_com_fever[order(age_com_fever$year_new),]
age_com_fever$x <- c(rep(1,each = 5), rep(4,each = 5), rep(7,each = 5), rep(10,each = 5), rep(13,each = 5),
                     rep(16,each = 5), rep(19,each = 5))

##internal medicine
age_com_inter <- ili %>% filter(year_new > 2009 & year_new < 2017 & clinic_new == "internal medicine") %>%
  group_by(year_new) %>% summarise(ILI_0 = sum(age0004),ILI_5 = sum(age0514),
                                   ILI_15 = sum(age1524),ILI_25 = sum(age2559),ILI_60 = sum(age6000),ili.total = sum(ili.total))  %>% 
  mutate(ILI_0_p = ILI_0/ili.total, ILI_5_p = ILI_5/ili.total, ILI_15_p = ILI_15/ili.total,
         ILI_25_p = ILI_25/ili.total,ILI_60_p = ILI_60/ili.total)
age_com_inter <- melt(age_com_inter[,c(1,8:12)], id = c("year_new"))
age_com_inter <- age_com_inter[order(age_com_inter$year_new),]
age_com_inter$x <- c(rep(1,each = 5), rep(4,each = 5), rep(7,each = 5), rep(10,each = 5), rep(13,each = 5),
                     rep(16,each = 5), rep(19,each = 5))

age_com_pae <- age_com_pae[age_com_pae$year_new %in% c(2012:2016),]
age_com_inter <- age_com_inter[age_com_inter$year_new %in% c(2012:2016),]
age_com_fever <- age_com_fever[age_com_fever$year_new %in% c(2012:2016),]

age_com_pae$x <- age_com_pae$x -6
age_com_inter$x <- age_com_inter$x -6
age_com_fever$x <- age_com_fever$x -6

labels <- c("2012","2013","2014","2015","2016")
breaks <- c(seq(1,13, length.out =5))
length(labels)
length(breaks)

# write.csv(age_com_fever,"fever.csv")
# write.csv(age_com_inter,"inter.csv")
# write.csv(age_com_pae,"pae.csv")

# scale_fill_discrete(name="Age gruop",
#                     breaks=c("ILI_0_p","ILI_5_p","ILI_15_p","ILI_25_p","ILI_60_p"),
#                     labels=c("0-4","5-14","15-24","25-59","≥60")) +
ggplot() +  geom_bar(data = age_com_pae, aes(x = x-0.6, y = value, fill = variable), 
                     stat = "identity", position = "stack", width = 0.5) +
  geom_bar(data = age_com_inter, aes(x = x, y = value, fill = variable), 
           stat = "identity", position = "stack", width = 0.5)+
  geom_bar(data = age_com_fever, aes(x = x+0.6, y = value, fill = variable), 
           stat = "identity", position = "stack", width = 0.5) +
  scale_x_continuous(name = "Year",  labels = labels, breaks = breaks, limits = c(0,14))+
  scale_y_continuous(name = "Proportion (%)",  breaks = seq(0, 1, 0.25),expand=c(0,0), 
                     labels = seq(0, 100, 25),
                     limits = c(0,1.1)) +
  geom_text(aes(x = c(0.4,1,1.6, 3.4,4,4.6, 6.4,7,7.6, 9.4,10,10.6, 12.4,13,13.6), y = 1.05),
            label = c("P","I","",rep(c("P","I","F"),4)),fontface = "bold",size =6)+
  annotate("text", x = 16, y = -0.08, label = c("")) +
  coord_cartesian(ylim = c(0,1), clip = "off")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(2,1,1,1, "cm"),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5))+
  scale_fill_manual(values = c("#FEE08B","#FDAE61","#ABDDA4","#66C2A5", "#3288BD"),
                    name="Age (years)",
                    breaks=c("ILI_0_p","ILI_5_p","ILI_15_p","ILI_25_p","ILI_60_p"),
                    labels=c("0-4","5-14","15-24","25-59","≥60")) +
  scale_color_manual(values = c( "#FAF9AA","#F8DF99","#E6CCBE", "#D38C32","#BD5B3C")) -> figB

setwd("/Users/liujiangyi/Desktop/2019nCoV/hop_rate/20200322")
# pdf(file = "figS9.pdf",width = 12, height = 7)
tiff(file = "figS9.tiff", width = 11, height = 7,
     units = "in", compression = "lzw", res = 300)
ggarrange(figB, nrow = 1, ncol = 1)
dev.off()

#======fig panel A========
load("ili_cleaned_2019.dat")
ili <- ili[ili$province == "Hubei",]
ili <- ili[ili$city.code == 4201,]
ili$year_new <- as.integer(format(ili$date, "%Y"))
ili$month_new <- as.integer(format(ili$date, "%m"))

ili$clinic_new <- NA
ili$clinic_new[ili$clinic == 1 |ili$clinic == 2] <- "internal medicine" #inter med outpat
ili$clinic_new[ili$clinic == 3 |ili$clinic == 4] <- "paediatric"
ili$clinic_new[ili$clinic == 5 ] <- "fever"
sum(is.na(ili$clinic_new))

ili <- ili[ili$month_new %in% c(1,2,3,12),]

ili <- ili %>% group_by(year_new,clinic_new) %>% summarise(no.ili = sum(ili.total))
ili <- ili[ili$year_new %in% c(2010:2016),]
ili$x <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(7,3))

factor(ili$clinic_new, levels = c("internal medicine","paediatric","fever")) -> ili$clinic_new

ggplot() + geom_bar(data = ili, aes(x=x, y = no.ili, fill = clinic_new),
                    stat = "identity", position = "stack", width = 0.5) +
  scale_x_continuous("Year", breaks = seq(1:7),
                     labels = c(2010:2016)) +
  scale_y_continuous("No. of ILI cases (1,000)", breaks = seq(0, 25000, 5000),
                     labels = seq(0, 25, 5),
                     limits = c(0, 25000))+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 14),
        text = element_text(size = 18),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0.5,"lines"),
        # legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_text(vjust = -0.5, hjust = 0.5))+
  # guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("#F8DF99", "#D38C32","#BD5B3C"),
                    name="Department",
                    breaks=c("internal medicine","paediatric","fever"),
                    labels=c("Internal","Paediatric","Fever")) +
  scale_color_manual(values = c("#F8DF99", "#D38C32","#BD5B3C"))+
  labs(title = "A")-> figA

setwd("/Users/liujiangyi/Desktop/2019nCoV/hop_rate/20200322")
tiff(file = "figS2.tiff", width = 15, height = 10, 
     units = "in", compression = "lzw", res = 300)
ggarrange(figA, figB, figC, figD,nrow = 2, ncol = 2)
dev.off()




