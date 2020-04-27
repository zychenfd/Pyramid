library(dplyr)
library(readxl)
library(ggpubr)
library(ggplot2)
library(reshape2)

load("~/Nutstore Files/2019-nCoV Epi and Modeling/7. Pyramid of diseases/Database/ili_cleaned_2019.dat")
outpatient <- read_excel("~/Nutstore Files/2019-nCoV Epi and Modeling/7. Pyramid of diseases/Database/hubei_outpatient_10_17.xlsx")  

# epiyear must be consistent to epiweek
# here, we need change %Y into %G
ili$year_new <- as.integer(format(ili$date, "%G"))
# ili$month_new <- as.integer(format(ili$date, "%m"))
ili$week_new <- as.integer(format(ili$date, "%V"))
# ili$date_new <- as.integer(format(ili$date, "%d"))
str(ili)

ili$year_range <- NA
ili$year_range[(ili$year_new == 2011 & ili$week_new >= 14) |
                 (ili$year_new == 2012 & ili$week_new < 14)] <- "2011-2012"
ili$year_range[(ili$year_new == 2012 & ili$week_new >= 14) |
                 (ili$year_new == 2013 & ili$week_new < 14)] <- "2012-2013"
ili$year_range[(ili$year_new == 2013 & ili$week_new >= 14) |
                 (ili$year_new == 2014 & ili$week_new < 14)] <- "2013-2014"
ili$year_range[(ili$year_new == 2014 & ili$week_new >= 14) |
                 (ili$year_new == 2015 & ili$week_new < 14)] <- "2014-2015"
ili$year_range[(ili$year_new == 2015 & ili$week_new >= 14) |
                 (ili$year_new == 2016 & ili$week_new < 14)] <- "2015-2016"

ili[!(is.na(ili$year_range) | ili$week_new == 53), ] -> ili

south_province <- c("Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi",
                    "Hubei", "Hunan", "Guangdogn", "Guangxi", "Hainan", "Chognqing",
                    "Sichuan", "Guizhou", "Yunnan")

ili_Southern <- ili[ili$province %in% south_province, ]
ili_Wuhan <- ili[ili$city.code == 4201, ]
 ili_Hubei <- ili[ili$province == "Hubei",]

ili_Hubei <- ili_Hubei %>%
  group_by(year_range, year_new, week_new) %>%
  summarise(ili_total = sum(ili.total), con.total = sum(con.total))
ili_Hubei$region <- "Hubei"

ili_Southern <- ili_Southern %>% 
  group_by(year_range,year_new, week_new) %>%  
  summarise(ili_total = sum(ili.total),  con.total = sum(con.total))
ili_Southern$region <- "Southern"                                                                                        

ili_Wuhan<- ili_Wuhan %>% 
  group_by(year_range,year_new, week_new) %>% 
  summarise(ili_total = sum(ili.total),  con.total = sum(con.total))
ili_Wuhan$region <- "Wuhan"   

ili_new <- rbind(ili_Southern, ili_Wuhan )

ili_new$rate <- ili_new$ili_total/ili_new$con.total

ili_new <- ili_new %>% arrange(region, year_new, week_new)

ili_new$x <- rep(1:52, 10)

ili_Hubei <- ili_Hubei %>% filter(!(year_new == 2011 & week_new < 14))   %>% filter(!(year_new == 2012 & week_new > 13))  
ili_Hubei$rate <- ili_Hubei$ili_total / ili_Hubei$con.total
ili_Hubei$x <- rep(seq(1,52,1),3)




###plot

x_breaks <- seq(1,54,2)
x_labels <- c("14", "16", "18", "20", "22", "24", "26", "28", "30", "32", "34", "36", "38", "40", "42", "44", "46",
              "48", "50", "52", "02", "04", "06", "08", "10", "12", "")
length(x_breaks )
length(x_labels)

ggplot() + geom_line(data = ili_new[ili_new$year_range == "2011-2012",],
                     aes(x = x, y = rate, colour = region),size =2) +
  scale_x_continuous("Week",breaks = seq(10,50,10),labels = seq(10,50,10)) +
  scale_y_continuous("Proportion (%)",labels = seq(0,6,1),expand = c(0,0),
                     breaks = seq(0,0.06,0.01),
                     limits = c(0, 0.06)) +
  annotate("text", x= 25, y = 0.055, label = "P = 0.68",size= 6, fontface = "bold")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18))+
  scale_fill_manual(values = c("#FEE08B","#3288BD")) +
  scale_color_manual(values = c("#FEE08B","#3288BD")) +
  labs(title = "A. 2011-2012") -> fig1
 
ggplot() + geom_line(data = ili_new[ili_new$year_range == "2012-2013",],
                     aes(x = x, y = rate, colour = region),size =2) +
  scale_x_continuous("Week",breaks = seq(10,50,10),labels = seq(10,50,10)) +
  scale_y_continuous("Proportion (%)",labels = seq(0,6,1),expand = c(0,0),
                     breaks = seq(0,0.06,0.01),
                     limits = c(0, 0.06)) +
  annotate("text", x= 25, y = 0.055, label = "P = 0.82",size= 6, fontface = "bold")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18))+
  scale_fill_manual(values = c("#F8DF99","#D38C32", "#BD5B3C")) +
  scale_color_manual(values = c("#FEE08B","#3288BD")) +
  labs(title = "B. 2012-2013") -> fig2

ggplot() + geom_line(data = ili_new[ili_new$year_range == "2013-2014",],
                     aes(x = x, y = rate, colour = region),size =2) +
  scale_x_continuous("Week",breaks = seq(10,50,10),labels = seq(10,50,10)) +
  scale_y_continuous("Proportion (%)",labels = seq(0,6,1),expand = c(0,0),
                     breaks = seq(0,0.06,0.01),
                     limits = c(0, 0.06)) +
  annotate("text", x= 25, y = 0.055, label = "P = 0.68",size= 6, fontface = "bold")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18))+
  scale_fill_manual(values = c("#F8DF99","#D38C32", "#BD5B3C")) +
  scale_color_manual(values = c("#FEE08B","#3288BD")) +
  labs(title = "C. 2013-2014") -> fig3

ggplot() + geom_line(data = ili_new[ili_new$year_range == "2014-2015",],
                     aes(x = x, y = rate, colour = region),size =2) +
  scale_x_continuous("Week",breaks = seq(10,50,10),labels = seq(10,50,10)) +
  scale_y_continuous("Proportion (%)",labels = seq(0,6,1),expand = c(0,0),
                     breaks = seq(0,0.06,0.01),
                     limits = c(0, 0.06)) +
  annotate("text", x= 25, y = 0.055, label = "P = 0.72",size= 6, fontface = "bold")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18))+
  scale_fill_manual(values = c("#F8DF99","#D38C32", "#BD5B3C")) +
  scale_color_manual(values = c("#FEE08B","#3288BD")) +
  labs(title = "D. 2014-2015") -> fig4

ggplot() + geom_line(data = ili_new[ili_new$year_range == "2015-2016",],
                     aes(x = x, y = rate, colour = region),size =2) +
  scale_x_continuous("Week",breaks = seq(10,50,10),labels = seq(10,50,10)) +
  scale_y_continuous("Proportion (%)",labels = seq(0,10,2),expand = c(0,0),
                     breaks = seq(0,0.1,0.02),
                     limits = c(0, 0.1)) +
  annotate("text", x= 25, y = 0.09, label = "P = 0.69",size= 6, fontface = "bold")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(colour = "black", size = 0.8),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.ticks.length = unit(0.4,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        plot.margin = margin(1,1,1,1, "cm"),
        axis.title = element_text(size = 20, face = "bold"),
        text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5),
        axis.title.y = element_text(vjust = 3, hjust = 0.5),
        axis.text.x = element_text(colour = "black",size = 18),
        axis.text.y = element_text(colour = "black",size = 18))+
  scale_fill_manual(values = c("#F8DF99","#D38C32", "#BD5B3C")) +
  scale_color_manual(values = c("#FEE08B","#3288BD")) +
  labs(title = "E. 2015-2016") -> fig5


ggplot() + annotate("text", x= 1.1, y =1, label = "Wuhan",
                    fontface = "bold",size = 5, hjust =0) +
  annotate("text", x= 1.1, y =1.1, label = "Southern China",
           fontface = "bold",size=5,hjust =0)+
  annotate("rect", xmin = 0.6,xmax = 0.9, ymin = 0.98, ymax = 1.02, fill = "#3288BD")+
  annotate("rect", xmin = 0.6,xmax = 0.9, ymin = 1.08, ymax = 1.12, fill ="#FEE08B") +
  scale_y_continuous(limits = c(0.5, 1.6))+
  scale_x_continuous(limits = c(0,3)) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) -> fig6

  

tiff(file = "figS6.tiff", width = 16, height = 10, 
     units = "in", compression = "lzw", res = 300)
ggarrange(fig1,fig2,fig3,fig4,fig5, fig6,nrow = 2, ncol = 3)
dev.off()

# ggplot() + facet_wrap(~year_range, scales = "free_x")+
#   geom_line(data = ili_new, aes(x = year_new, y = rate, colour = region))



# correlation analysis  2011-12  to  2015-2016
tmp <- dcast(ili_new, year_range + week_new ~ region, value.var = "rate")
str(tmp)
year_range <- unique(tmp$year_range)
p <- c()
cor <- c()
for (i in year_range) {
  tmp1 <- tmp[tmp$year_range == i, ]
  tmp2 <- cor.test(tmp1$Hubei, tmp1$Southern, method = "pearson") 
  p <- c(p, tmp2$p.value)
  cor <- c(cor, tmp2$estimate)
}
cor_HB_South <- data.frame(year = unique(tmp$year_range), cor = cor,p = p, 
                           region = "Hubei_vs_South")

p1 <- c()
cor1 <- c()
for ( i in year_range) {
  tmp1 <- tmp[tmp$year_range == i, ]
  tmp2 <- cor.test(tmp1$Wuhan, tmp1$Southern, method = "pearson")
  p1 <- c(p1, tmp2$p.value)
  cor1 <- c(cor1, tmp2$estimate)
}
cor_WH_South <- data.frame(year = unique(tmp$year_range), cor = cor1,p = p1, 
                           region = "Wuhan_vs_South")


p2 <- c()
cor2 <- c()
for ( i in year_range) {
  tmp1 <- tmp[tmp$year_range == i, ]
  tmp2 <- cor.test(tmp1$Wuhan, tmp1$Hubei, method = "pearson")
  p2 <- c(p2, tmp2$p.value)
  cor2 <- c(cor2, tmp2$estimate)
}
cor_WH_HB <- data.frame(year = unique(tmp$year_range), cor = cor2,p = p2, 
                        region = "Wuhan_vs_Hubei")

correlation <- rbind(cor_WH_HB, cor_WH_South, cor_HB_South)
write.csv(correlation, "correlation.csv")
