library(pdftools)
library(tm)
library(rJava)
library(tabulizerjars)
library(tabulizer)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyr)
# Why people don't work, per 100 person
not_working_per_100 <- read.csv('overall_data.csv', encoding = "UTF-8")
years <- c("виды", "2012", "2013", "2014", "2015", "2016")
names(not_working_per_100) <- years

about_days <- not_working_per_100[13:24, ]
about_reasons <- not_working_per_100[1:12, ]
row.names(about_reasons)

about_desease <- about_reasons[3, ]
about_desease <- as.data.frame(t(about_desease))
about_desease$`Год` <- c(2011:2016)
about_desease <- about_desease[-1,]

colnames(about_desease) <- c("На каждые 100 случаев временной нетрудоспособности", "Год")
about_desease <- about_desease[, c(2,1)]

about_desease_1 <- as.data.frame(apply(apply(about_desease, 2, gsub, patt=",", replace="."), 2, as.numeric))

b <- ggplot(about_desease_1, aes(x = `Год`, y = as.numeric(`На каждые 100 случаев временной нетрудоспособности`), group = 1))+
  geom_point(size = 4)+
  geom_line(size = 1) + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(0, 100),breaks = c(20, 40, 60, 80, 100), name = "Количество временно нетрудоспособных по болезни, на 100 работающих")+
  ggtitle("Временная нетрудоспособность жителей Татарстана по болезни")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)
ggplotly(b)


# Desease per 100 000
overall_mistake <- read.csv("overall with mistake.csv", encoding = "UTF-8")
overall_mistake <- overall_mistake[-1,]
overall_mistake<- separate(data = overall_mistake, col = X2013.2014, into = c("X2013", "X2014"), sep = ",\\d")
overall_mistake$X.1 <- NULL
overall_mistake_1 <- as.data.frame(apply(overall_mistake, 2, gsub, patt=",", replace="."))

overall_mistake_1_per_100000 <- overall_mistake_1[29:56,]

overall_mistake_1_per_100000 <- overall_mistake_1_per_100000[-c(1,2,3,6,7, 9, 18, 21,22,24,26,28), ]

years <- c("Заболевания", 2012:2016)
names(overall_mistake_1_per_100000) <- years
overall_mistake_1_per_100000_by_years <- overall_mistake_1_per_100000[1, ]
overall_mistake_1_per_100000_by_years <- as.data.frame(t(overall_mistake_1_per_100000_by_years))
overall_mistake_1_per_100000_by_years$`Год` <- c(2011:2016)
overall_mistake_1_per_100000_by_years <- overall_mistake_1_per_100000_by_years[-1,]


colnames(overall_mistake_1_per_100000_by_years) <- c("Год", "Количество заболеваний на 100 000 человек")
as.numeric(levels(overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`))[overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`]
overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек` <- as.numeric(levels(overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`))[overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`]
h <- ggplot(overall_mistake_1_per_100000_by_years, aes(x = `Год`, y = `Количество заболеваний на 100 000 человек`))+
  geom_point(size = 4)+
  geom_line(size = 1) + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(50000, 100000),breaks = c(60000, 80000, 100000), name = "Количество пациентов на 100 000 жителей РТ")+
  ggtitle("Заболеваемость населения")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)
ggplotly(h)


#Here I look to certain deseases
overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек` <- as.numeric(levels(overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`))[overall_mistake_1_per_100000_by_years$`Количество заболеваний на 100 000 человек`]

disease_statistics <- overall_mistake_1_per_100000

#which.max(as.numeric(levels(disease_statistics$`2012`))[disease_statistics$`2012`]) # Болезни органов дыхания 
#which.max(as.numeric(levels(disease_statistics$`2013`))[disease_statistics$`2013`]) # Болезни органов дыхания 
#which.max(as.numeric(levels(disease_statistics$`2014`))[disease_statistics$`2014`]) # Болезни органов дыхания 
#which.max(as.numeric(levels(disease_statistics$`2015`))[disease_statistics$`2015`]) # Болезни органов дыхания 
#which.max(as.numeric(levels(disease_statistics$`2016`))[disease_statistics$`2016`]) # Болезни органов дыхания 

disease_statistics$`2012` <- as.numeric(levels(disease_statistics$`2012`))[disease_statistics$`2012`]
disease_statistics$`2013` <- as.numeric(levels(disease_statistics$`2013`))[disease_statistics$`2013`]
disease_statistics$`2014` <- as.numeric(levels(disease_statistics$`2014`))[disease_statistics$`2014`]
disease_statistics$`2015` <- as.numeric(levels(disease_statistics$`2015`))[disease_statistics$`2015`]
disease_statistics$`2016` <- as.numeric(levels(disease_statistics$`2016`))[disease_statistics$`2016`]

rownames(disease_statistics) <- c(1:16)
head(disease_statistics[order(disease_statistics$`2012`, decreasing = TRUE), ])
head(disease_statistics[order(disease_statistics$`2013`, decreasing = TRUE), ])
head(disease_statistics[order(disease_statistics$`2014`, decreasing = TRUE), ])
head(disease_statistics[order(disease_statistics$`2015`, decreasing = TRUE), ])
head(disease_statistics[order(disease_statistics$`2016`, decreasing = TRUE), ])


g <- subset(head(disease_statistics[order(disease_statistics$`2012`, decreasing = TRUE), ]))
names(g)
final_g <- as.data.frame(t(g))
new_header <- c('Болезни органов дыхания', 'Травмы, отравления и некоторые другие последствия воздействия внешних причин', 'Осложнения беременности, родов и послеродового периода', 'Болезни кожи и подкожной клетчатки', 'Болезни мочеполовой системы', 'Болезни костно-мышечной системы и соединительной ткани')
names(final_g) <- new_header
final_g <- final_g[-1,]
final_g$`Год` <- c(2012:2016)
final_g[1,1]
is.numeric(final_g$`Болезни органов дыхания`)
final_g <- as.data.frame(apply(final_g, 2, as.numeric))
is.numeric(final_g$`Болезни органов дыхания`)
is.numeric(final_g$`Год`)

s <- ggplot(final_g, aes(x = `Год`))+
  geom_line(aes(y = `Болезни органов дыхания`, colour = "Органы дыхания"), size = 1) + 
  geom_line(aes(y = `Травмы, отравления и некоторые другие последствия воздействия внешних причин`, colour = "Травмы"), size = 1)+
  geom_line(aes(y = `Осложнения беременности, родов и послеродового периода`, colour = "Болезни, связанные с беременностью"), size = 1)+
  geom_line(aes(y = `Болезни кожи и подкожной клетчатки`, colour = "Кожа"), size = 1)+
  geom_line(aes(y = `Болезни мочеполовой системы`, colour = "Мочеполовая система"), size = 1)+
  geom_line(aes(y = `Болезни костно-мышечной системы и соединительной ткани`, colour = "Соединительная ткань"), size = 1)+
  geom_point(aes(y = `Болезни органов дыхания`, colour = "Органы дыхания"), size = 2.5)+
  geom_point(aes(y = `Травмы, отравления и некоторые другие последствия воздействия внешних причин`, colour = "Травмы"), size = 2.5)+
  geom_point(aes(y = `Осложнения беременности, родов и послеродового периода`, colour = "Болезни, связанные с беременностью"), size = 2.5)+
  geom_point(aes(y = `Болезни кожи и подкожной клетчатки`, colour = "Кожа"), size = 2.5)+
  geom_point(aes(y = `Болезни мочеполовой системы`, colour = "Мочеполовая система"), size = 2.5)+
  geom_point(aes(y = `Болезни костно-мышечной системы и соединительной ткани`, colour = "Соединительная ткань"), size = 2.5)+
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения РТ")+
  scale_y_continuous(limits = c(0, 50000), breaks = c(10000, 20000, 30000), name = "Болезни на 100 000 человек")+
  ggtitle("Заболеваемость населения по основным классам болезней, 2012-2016 гг.")+
  scale_color_manual(name="Болезни", values = c("black", "orange", "red", "blue", "green", "brown"))+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"), legend.position = "bottom")
ggplotly(s)


#
ggg<- read.csv("overall cancer.csv", encoding = "UTF-8")
ggg <- ggg[-c(1,4),]

ggg_new <- ggg[-c(1,3,4),]
ggg_new <- as.data.frame(t(ggg_new))
ggg_new$year <- c(2011:2016)
ggg_new<- ggg_new[-1, ]
ng <- c("Количество людей, у которых выявлен рак", "Год")
names(ggg_new) <- ng
is.numeric(ggg_new$`Количество людей, у которых выявлен рак`)
ggg_new <- as.data.frame(apply(ggg_new, 2, as.numeric))

gggplot <- ggplot(ggg_new, aes(x = `Год`, y = `Количество людей, у которых выявлен рак`))+
  geom_point(size = 4, color= "blue")+
  geom_line(size = 1, color= "red") + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(0, 500),breaks = c(100, 200, 300, 400, 500), name = "Количество людей с выявленным диагнозом")+
  ggtitle("Заболеваемость злокачественными новообразованиями, на 100 000 человек")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)
ggplotly(gggplot)


#
ggg_all<- ggg[-c(1,2,4), ]
ggg_all <- as.data.frame(t(ggg_all))
ggg_all$year <- c(2011:2016)
ggg_all<- ggg_all[-1, ]
ng_1 <- c("Общая численность пациентов на учете с диагнозом рак", "Год")
names(ggg_all) <- ng_1
is.numeric(ggg_all$`Общая численность пациентов на учете с диагнозом рак`)
ggg_all <- as.data.frame(apply(ggg_all, 2, as.numeric))

gggplot_all <- ggplot(ggg_all, aes(x = `Год`, y = `Общая численность пациентов на учете с диагнозом рак`))+
  geom_point(size = 4, color= "red")+
  geom_line(size = 1, color= "green") + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(0, 100000),breaks = c(50000, 60000, 70000, 80000, 100000), name = "Численность пациентов, состоящих на учете в лечебно-профилактических учреждениях, всего")+
  ggtitle("Заболеваемость злокачественными новообразованиями, всего")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"))
ggplotly(gggplot_all)


# Установленные впервые, всего
ggg_new_all<- ggg[-c(2,3,4), ]
ggg_new_all <- as.data.frame(t(ggg_new_all))
ggg_new_all$year <- c(2011:2016)
ggg_new_all<- ggg_new_all[-1, ]
ng_2 <- c("Выявлено пациентов с установленным впервые диагнозом", "Год")
names(ggg_new_all) <- ng_2
is.numeric(ggg_new_all$`Выявлено пациентов с установленным впервые диагнозом`)
ggg_new_all <- as.data.frame(apply(ggg_new_all, 2, as.numeric))

gggplot_new_all <- ggplot(ggg_new_all, aes(x = `Год`, y = `Выявлено пациентов с установленным впервые диагнозом`))+
  geom_point(size = 4, color= "black")+
  geom_line(size = 1, color= "blue") + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(0, 20000),breaks = c(10000, 14000, 15000, 16000, 20000), name = "Пациенты с установленным впервые диагнозом, всего")+
  ggtitle("Заболеваемость злокачественными новообразованиями, всего новых пациентов за год")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"))
ggplotly(gggplot_new_all)



### все пациенты на 100 000
ggg_patients_per100<- ggg[-c(1,2,3), ]
ggg_patients_per100 <- as.data.frame(t(ggg_patients_per100))
ggg_patients_per100$year <- c(2011:2016)
ggg_patients_per100<- ggg_patients_per100[-1, ]
ng_3<- c("Пациенты с диагнозом на 100 000 человек", "Год")
names(ggg_patients_per100) <- ng_3
is.numeric(ggg_patients_per100$`Пациенты с диагнозом на 100 000 человек`)
ggg_patients_per100 <- as.data.frame(apply(ggg_patients_per100, 2, as.numeric))

gggplot_patients_per100 <- ggplot(ggg_patients_per100, aes(x = `Год`, y = `Пациенты с диагнозом на 100 000 человек`))+
  geom_point(size = 4, color= "blue")+
  geom_line(size = 1, color= "orange") + 
  scale_x_continuous(name = "Год\n
                     Источник: Министерство здравоохранения Республики Татарстан")+
  scale_y_continuous(limits=c(0, 2600),breaks = c(1900, 2100, 2300, 2500, 2600), name = "Пациенты с диагнозом на 100 000 человек")+
  ggtitle("Заболеваемость злокачественными новообразованиями, пациенты на 100 000 человек")+
  theme_economist(horizontal = TRUE, stata = TRUE, dkpanel = TRUE)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold"))
ggplotly(gggplot_patients_per100)





types <- read.csv("overall cancer detailed.csv", encoding = "UTF-8")
