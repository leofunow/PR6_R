if ("ggplot2" %in% installed.packages() == FALSE) {
  install.packages("ggplot2")
}
library("ggplot2")
download.file("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv",
              destfile = "demography.csv")
data <- read.csv(file = "demography.csv",
                 encoding = "UTF-8")
View(data)

young_share <- (data$young_total) / (data$popul_total / 100)
trud_share <- (data$wa_total) / (data$popul_total / 100)
old_share <- (data$ret_total) / (data$popul_total / 100)
data$trud_share <- trud_share
data$young_share <- young_share
data$old_share <- old_share

ggplot(data = data, aes(x = trud_share)) +
  geom_histogram(color = 'black', fill = 'gray')+
  geom_rug(color = 'red')+
  geom_vline(aes(xintercept = mean(trud_share)), color = "red", linetype = "dashed", size = 1)

ggplot(data = data, aes(x = trud_share, group=region, fill = region))+
  geom_density(alpha=0.3)
ggplot(data = data, aes(x = region, y = trud_share, fill = region))+
  geom_violin(alpha=0.3)+
  geom_jitter(shape=16, position=position_jitter(0.2))
ggplot(data = data, aes(x = region, y = trud_share, fill = region))+
  geom_boxplot(alpha=0.3)

ggplot(data = data, aes(x = young_share, y = old_share, color = region))+
  geom_point(shape=17, size = 3)

male_share <- (data$ret_male +data$wa_male+data$young_male)/data$popul_total*100
data$male_share <- male_share
male <- c()
for (i in male_share){
  if(i>50){
    male <- append(male, 1)
  }
  else
    male <- append(male, 0)
}
data$male <- male
install.packages("hrbrthemes")#для красоты
install.packages("viridis")
library("hrbrthemes");
library(viridis)
ggplot(data = data, aes(x = young_share, y = old_share, color = male, size = male_share))+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  geom_point(alpha = 0.5)

ggplot(data = data, aes(x = region, fill = region )) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="C") +
  theme_ipsum() +
  geom_bar()

# Часть 2
data <- mtcars
install.packages("xkcd",dependencies = TRUE)
library(xkcd)

ggplot(data = data, aes(x = hp, y = wt, color = as.logical(am), size = cyl))+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  geom_point(alpha = 0.3)+
  scale_color_manual("Коробка передач",breaks = c(F,T),values = c("red", "green"), labels=c("РКПП","АКПП"))+
  scale_size("Количество\nциллиндров")+
  xlab("Gross horsepower")+
  ylab("Weight")+
  ggtitle("График для задания 1")

ggplot(data = data, aes(x = hp, size = cyl, group = am))+
  geom_histogram(fill="brown", color="black", bins = 6)+
  theme_xkcd()+
  facet_grid(cols = vars(am), labeller = labeller(am = c('0' = "Automatic",'1' = "Mechanic")))+
  xlab("Horsepower")+
  ggtitle("Gross horsepower")

data <- sleep
View(data)

ggplot(data = data, aes(y = extra, group=group, fill=group))+
  geom_boxplot()+
  xlab("area")+
  ylab("extra")+
  ggtitle("График для задания 3")+
  theme(legend.position="none")

install.packages(c("dplyr", "readr", "stringr"))

data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

data1 <- t(data)[c(100:300),]
data1 <- as.data.frame(data)
ggplot(data = data1, aes(x=rownames(data1)))+
  geom_line(aes(y = as.numeric(V1)),group=1, color="red")+
  geom_point(aes(y = as.numeric(V1)),group=1, color="red")+
  geom_line(aes(y = as.numeric(V2)),group=1, color="green")+
  geom_point(aes(y = as.numeric(V2)),group=1, color="green")+
  geom_point(aes(y = as.numeric(V3)),group=1, color="blue")+
  geom_line(aes(y = as.numeric(V3)),group=1, color="blue")
ggplot(data = data1)+
  geom_rug(aes(x = as.numeric(V1)),color = 'red')+
  geom_rug(aes(x = as.numeric(V2)),color = 'green')+
  geom_rug(aes(x = as.numeric(V3)),color = 'blue')+
  geom_histogram(aes(x = as.numeric(V1)), fill ="red", alpha=0.3, color="black")+
  geom_histogram(aes(x = as.numeric(V2)), fill = "green", alpha=0.3)+
  geom_histogram(aes(x = as.numeric(V3)),fill = "blue", alpha=0.3)+
  geom_vline(aes(xintercept = mean(as.numeric(V1))), color ="red")+
  geom_vline(aes(xintercept = mean(as.numeric(V2))), color = "green")+
  geom_vline(aes(xintercept = mean(as.numeric(V3))),color = "blue")
ggplot(data = data1)+
  geom_density(aes(y = as.numeric(V1)), fill ="red", alpha=0.3, color="black")+
  geom_density(aes(y = as.numeric(V2)), fill = "green", alpha=0.3)+
  geom_density(aes(y = as.numeric(V3)),fill = "blue", alpha=0.3)
ggplot(data = data1)+
  geom_boxplot(aes(y = as.numeric(V1)), fill ="red", alpha=0.3, color="black")
ggplot(data = data1)+
  geom_boxplot(aes(y = as.numeric(V2)), fill = "green", alpha=0.3)
ggplot(data = data1)+
  geom_boxplot(aes(y = as.numeric(V3)),fill = "blue", alpha=0.3)
ggplot(data = data1, aes(x=""))+
  geom_violin(aes(y = as.numeric(V1)), fill ="red", alpha=0.3, color="black")+
  geom_violin(aes(y = as.numeric(V2)), fill = "green", alpha=0.3)+
  geom_violin(aes(y = as.numeric(V3)),fill = "blue", alpha=0.3)
ggplot(data = data1)+
  geom_bar(aes(x = as.numeric(V1)), fill ="red", alpha=0.3, color="black")+
  geom_bar(aes(x = as.numeric(V2)), fill = "green", alpha=0.3)+
  geom_bar(aes(x = as.numeric(V3)),fill = "blue", alpha=0.3)





