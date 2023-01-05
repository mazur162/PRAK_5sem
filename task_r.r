
# Выключаю вывод предупреждающих сообщений warning
oldw <- getOption("warn")
options(warn = -1)

install.packages("dplyr")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("highcharter")
install.packages("Leaflet")
install.packages("RColorBrewer")
install.packages("Plotly")
install.packages("sunburstR")
install.packages("RGL")
install.packages("dygraphs")
install.packages("outliers")
library(outliers)
install.packages("mice")
library(mice)
install.packages("corrplot")
library(corrplot)
install.packages('car')
library(car)

packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')
library(tidyr)
library(tibble)

install.packages("nortest")
library(nortest)
install.packages("ggpubr")
set.seed(31600) # устанавливаю значение рандома по умолчанию

filter_data = function(data_frame, column, value){
  dplyr::filter(data_frame, !!as.symbol(column) == value)
}


#Читаем таблицы и удаляем пустые значения 

nba_stats <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/players_stats.csv", header = TRUE)
nba_stats <- na.omit(nba_stats)

nba_seasons <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/Seasons_Stats.csv", header = TRUE)
nba_seasons <- na.omit(nba_seasons)

nba_players <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/all_players.csv", header = TRUE)
nba_players <- na.omit(nba_players)


# фильтруем таблицу
nba_stats_pg <- filter_data(nba_stats, 'Pos', 'PG')
nba_stats_pg # игроки на позиции Point Guard

data <- nba_stats_pg$Heigh # будем смотреть распределение роста


# Распределние по количеству очков у игроков разных позиций

nba_stats$factor <- factor(nba_stats$Pos)

cdplot(factor ~ nba_stats$PTS, col = c("coral", "yellow", "lightblue", "lightgreen", "purple"), data = nba_stats)


boxplot(PTS ~ Pos,
        col = rgb(0.5, 0.2, 0.7, 0.5), 
        data = nba_stats, horizontal=TRUE)


boxplot(AST ~ Pos,
        col = rgb(0.5, 0.2, 0.7, 0.5), 
        data = nba_stats, horizontal=TRUE)


boxplot(BLK ~ Pos,
        col = rgb(0.5, 0.2, 0.7, 0.5),
        data = nba_stats, horizontal=TRUE)


nba_3pt <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/3pt.csv", header = TRUE)
nba_3pt <- na.omit(nba_3pt)

stripchart(nba_3pt$X3PT ~ nba_3pt$Year,
           method = "jitter",
           jitter = 0.45,
           pch = 1, col = '#6852A3', vertical = TRUE)


data <- nba_stats$X3PM[1:500]
grubbs.test(data)


data <- nba_stats$X3PM[370:390]
dixon.test(data)


nba_stats_c <- filter_data(nba_stats, 'Pos', 'C')
data <- nba_stats_c$Height


# Примечание: в данной таблице другая нумерация, так как удалены пробелы. Поэтому удалю другие значения
nba_stats_c$Height[68]
nba_stats_c$Height[11]
nba_stats_c$Height[8]
nba_stats_c$Height[45]

#Удаляю
nba_stats_c$Height[68] <- NA
nba_stats_c$Height[11] <- NA
nba_stats_c$Height[8] <- NA
nba_stats_c$Height[45] <- NA

data <- mice(nba_stats_c, seed = 31600)
data1 <- complete(data, action = 3)

data1$Height[68]
data1$Height[11]
data1$Height[8]
data1$Height[45]


data1 <- rnorm(50, 0, 2)
data2 <- rnorm(50, 0, 1)

data3 <- rnorm(5000, 1, 4)
data4 <- rnorm(5000, 11, 3)

data5 <- rnorm(1000, 1, 1)


library(ggplot2)

norm_vec = data.frame(Factor = rep(c(50, 50,5000, 5000, 1000)),
                                   Variable = c(data1, data2, data3, data4, data5))
ggplot(norm_vec, aes(Variable, group = Factor, fill = Factor)) +
  geom_density(alpha = 0.3)

graph_distr <- function(x, pc, main_name = "")
{ 
  plot(x,pc, type = "l", col = rgb(0.5, 0.2, 0.7, 0.5), lwd = 2) 
  plot(ecdf(x), add = TRUE) 
}


#Выборки малого объема (не более 100)
data1 = rnorm(50, 0, 1)
data2 = rnorm(50, 0, 2)

#Выборки умеренного объема
data3 = rnorm(5000, 1, 4)
data4 = rnorm(5000, 11, 3)
data5 = rnorm(1000, 1, 11)


graph_distr <- function(x, pc, main_name = "")
{ 
  mn <- paste(c("Эмпирическая Функция Распределения для выборки", main_name))
  plot(x,pc, type = "l", col = "violet", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
}

data1 = sort(data1)
graph_distr(data1, pnorm(data1, mean = 0, sd = 1), "data1")

data2 = sort(data2)
graph_distr(data2, pnorm(data2, mean = 10, sd = 5), "data2")

data3 = sort(data3)
graph_distr(data3, pnorm(data3, mean = 0, sd = 1), "data3")

data4 = sort(data4)
graph_distr(data4, pnorm(data4, mean = 13, sd = 5), "data4")

data5 = sort(data5)
graph_distr(data5, pnorm(data5, mean = 5, sd = 10), "data5")


qqgraph <- function(x){
  qqnorm(x)
  qqline(x)
}

qqgraph(data1)
qqgraph(data2)
qqgraph(data3)
qqgraph(data4)
qqgraph(data5)


envelmet <- function(x){
  z <- (x - mean(x))/sqrt(var(x))  #  Стандартизация выборки
  x.qq <- qqnorm(z, plot.it = FALSE)
  x.qq <- lapply(x.qq, sort)
  plot(x.qq, ylim = c(-10, 10))
}

envelmet(data1)
envelmet(data2)
envelmet(data3)
envelmet(data4)
envelmet(data5)


ks.test(data1, "pnorm")
ks.test(data5, "pnorm")


ks.test(data1, data3)
ks.test(data4, data5)


shapiro.test(data1)
shapiro.test(data2)
shapiro.test(data3)
shapiro.test(data4)
shapiro.test(data5)


ad.test(data1)
ad.test(data2)
ad.test(data3)
ad.test(data4)
ad.test(data5)


cvm.test(data1)
cvm.test(data2)
cvm.test(data3)


lillie.test(data1)
lillie.test(data2)
lillie.test(data3)


age_small <- nba_stats$Age
age_small <- na.omit(age_small)

age_big <- nba_seasons$Age
age_big <- na.omit(age_big)


qqgraph <- function(x){
  qqnorm(x)
  qqline(x)
}

qqgraph(age_small)


envelmet(age_small)
ks.test(age_small, "pnorm")
shapiro.test(age_small)
ad.test(age_small)
cvm.test(age_small)
lillie.test(age_small)


envelmet(age_big)
ks.test(age_big, "pnorm")
ad.test(age_big)
cvm.test(age_big)
lillie.test(age_big)


norm0_4 <-  rnorm(100, sd = 4)
norm1_1 <- rnorm(500, mean = 1, sd = 11)
norm0_9 <- rnorm(3000, mean = 0, sd = 9) 


t.test(norm0_4, mu = 0, conf.level = 0.9)
t.test(norm0_4, mu = 0.7, conf.level = 0.9)
t.test(norm0_4, mu = 0.5, conf.level = 0.9)

t.test(norm1_1, mu = 0, conf.level = 0.9)
t.test(norm1_1, mu = 15, conf.level = 0.9)
t.test(norm1_1, mu = 9.5, conf.level = 0.9)

t.test(norm0_9, mu = 0.316, conf.level = 0.9)
t.test(norm0_9, mu = 0.25, conf.level = 0.9)
t.test(norm0_9, mu = 0.41, conf.level = 0.9)


t.test(norm0_4, mu = 0, conf.level = 0.95)
t.test(norm0_4, mu = 0.7, conf.level = 0.95)
t.test(norm0_4, mu = 0.5, conf.level = 0.95)

t.test(norm1_1, mu = 0, conf.level = 0.95)
t.test(norm1_1, mu = 15, conf.level = 0.95)
t.test(norm1_1, mu = 9.5, conf.level = 0.95)

t.test(norm0_9, mu = 0.316, conf.level = 0.95)
t.test(norm0_9, mu = 0.25, conf.level = 0.95)
t.test(norm0_9, mu = 0.41, conf.level = 0.95)


t.test(norm0_4, mu = 0, conf.level = 0.99)
t.test(norm0_4, mu = 0.7, conf.level = 0.99)
t.test(norm0_4, mu = 0.5, conf.level = 0.99)

t.test(norm1_1, mu = 0, conf.level = 0.99)
t.test(norm1_1, mu = 15, conf.level = 0.99)
t.test(norm1_1, mu = 9.5, conf.level = 0.99)

t.test(norm0_9, mu = 0.316, conf.level = 0.99)
t.test(norm0_9, mu = 0.25, conf.level = 0.99)
t.test(norm0_9, mu = 0.41, conf.level = 0.99)


t.test(norm0_4, norm0_9, alternative = 'two.sided')
t.test(norm0_4, norm0_9, alternative = 'greater')
t.test(norm0_4, norm0_9, alternative = 'less')

t.test(norm0_4, norm1_1, alternative = 'two.sided')
t.test(norm0_4, norm1_1, alternative = 'greater')
t.test(norm0_4, norm1_1, alternative = 'less')


pois1 <-  rpois(7, 14)
pois2 <- rpois(7, 7)
pois3 <- rpois(14, 7)

mean(pois1)
mean(pois2)
mean(pois3)

wilcox.test(pois1, mu = mean(pois1), exact = FALSE)
wilcox.test(pois2, mu = mean(pois2), exact = FALSE)
wilcox.test(pois3, mu = mean(pois3), exact = FALSE)


install.packages("ISwR")
library(ISwR)

# Возьмём все выборки размера 1000
norm0_4 <-  rnorm(1000, sd = 4)
norm1_1 <- rnorm(1000, mean = 1, sd = 11)
norm0_9 <- rnorm(1000, mean = 0, sd = 9) 

f1 <- rep(1, 1000)
f2 <- rep(2, 1000)
f3 <- rep(3, 1000)
fact <- as.factor(c(f1, f2, f3))
                    
ff1 <- rep(1, 1000)
ff2 <- rep(2, 1000)
ffact <- as.factor(c(ff1, ff2))

fff1 <- rep(1, 1000)
fff2 <- rep(3, 1000)
fffact <- as.factor(c(fff1, fff2))

vec_norm <- c(norm0_9, norm0_4, norm1_1)
vec_norm_2 <- c(norm0_9, norm0_4)
vec_norm_3 <- c(norm0_9, norm1_1)



leveneTest(vec_norm, fact)
leveneTest(vec_norm_2, ffact)
leveneTest(vec_norm_3, fffact)


bartlett.test(vec_norm, fact)
bartlett.test(vec_norm_2, ffact)
bartlett.test(vec_norm_3, fffact)


fligner.test(vec_norm, fact)
fligner.test(vec_norm_2, ffact)
fligner.test(vec_norm_3, fffact)


Height <- nba_stats$Height
BLK <- nba_stats$BLK
AST <- nba_stats$AST
STL <- nba_stats$STL


cor.test(Height, BLK, method = 'pearson')
cor.test(Height, AST, method = 'pearson')
cor.test(Height, STL, method = 'pearson')


cor.test(Height, BLK, method = 'spearman')
cor.test(Height, AST, method = 'spearman')
cor.test(Height, STL, method = 'spearman')


cor.test(Height, BLK, method = 'kendall')
cor.test(Height, AST, method = 'kendall')
cor.test(Height, STL, method = 'kendall')


nba_stats_c <- nba_stats[nba_stats$Pos == "C", ]
chisq.test(nba_stats_c$Height)
chisq.test(nba_stats_c$Weight)
chisq.test(nba_stats_c$Age)


center_pts <- nba_stats[nba_stats$Pos == 'C', ]$PTS
pg_pts <- nba_stats[nba_stats$Pos == 'PG', ]$PTS
sf_pts <- nba_stats[nba_stats$Pos == 'SF', ]$PTS

center_ast <- nba_stats[nba_stats$Pos == 'C', ]$AST
pg_ast <- nba_stats[nba_stats$Pos == 'PG', ]$AST
sf_ast <- nba_stats[nba_stats$Pos == 'SF', ]$AST

center_stl <- nba_stats[nba_stats$Pos == 'C', ]$STL
pg_stl <- nba_stats[nba_stats$Pos == 'PG', ]$STL
sf_stl <- nba_stats[nba_stats$Pos == 'SF', ]$STL

k <- matrix(c(mean(center_pts), mean(pg_pts), mean(center_ast), mean(pg_ast)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("C", "PG"), c("PTS", "AST")))

k1 <- matrix(c(mean(center_pts), mean(sf_pts), mean(center_ast), mean(sf_ast)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("C", "SF"), c("PTS", "AST")))

k2 <- matrix(c(mean(pg_pts), mean(sf_pts), mean(pg_ast), mean(sf_ast)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("PG", "SF"), c("PTS", "AST")))

k3 <- matrix(c(mean(pg_pts), mean(sf_pts), mean(pg_stl), mean(sf_stl)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("PG", "SF"), c("PTS", "STL")))

k4 <- matrix(c(mean(pg_pts), mean(center_pts), mean(pg_stl), mean(center_stl)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("PG", "C"), c("PTS", "STL")))

k5 <- matrix(c(mean(pg_pts), mean(center_pts), mean(pg_stl), mean(center_stl)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("C", "SF"), c("PTS", "STL")))


fisher.test(k)
fisher.test(k1)
fisher.test(k2)
fisher.test(k3)
fisher.test(k4)
fisher.test(k5)


mcnemar.test(k)
mcnemar.test(k1)
mcnemar.test(k2)
mcnemar.test(k3)
mcnemar.test(k4)
mcnemar.test(k5)


data3 <- nba_stats[, c(19, 25, 4, 33, 30, 17, 18)]

MATRIX <- cor(data3)
col4 <- colorRampPalette(c("white", "#3F007D")) 

corrplot(MATRIX, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "white")


summary(aov(nba_stats$PTS ~ nba_stats$Pos))


lin <- lm(nba_stats$X3PM ~ nba_stats$X3PA)
summary(lin)


polynomial = lm(nba_stats$X3PM ~ poly(nba_stats$X3PA, degree = 3, raw = T))
summary(polynomial)