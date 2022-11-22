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

# Строим гистограмму и ядерные оценки
hist(data, breaks = 20, freq = FALSE, col = rgb(0.5, 0.2, 0.7, 0.5),
     xlab = "Рост",
     ylab = "Плотность вероятности",
     main = "Распределение роста на позиции разыгрывающего защитника PG")
lines(density(data), col = "blue", lwd = 3)
lines(density(data, bw = 0.8), col = "green", lwd = 3)

# Распределние по количеству очков у игроков разных позиций

nba_stats$factor <- factor(nba_stats$Pos)

cdplot(factor ~ nba_stats$PTS, col = c("coral", "yellow", "lightblue", "lightgreen", "purple"), data = nba_stats)

boxplot(AST ~ Pos,
        col = rgb(0.5, 0.2, 0.7, 0.5), 
        data = nba_stats)

nba_3pt <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/3pt.csv", header = TRUE)
nba_3pt <- na.omit(nba_3pt)

stripchart(nba_3pt$X3PT ~ nba_3pt$Year,
           method = "jitter",
           jitter = 0.45,
           pch = 1, col = '#6852A3')


data1 = rnorm(50, 0, 2)
data2 = rnorm(50, 0, 1)

data3 = rnorm(5000, 1, 4)
data4 = rnorm(5000, 11, 3)

data5 = rnorm(1000, 1, 1)


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
# квантили
qqgraph <- function(x){
  qqnorm(x)
  qqline(x)
}

qqgraph(data1)
qqgraph(data2)
qqgraph(data3)
qqgraph(data4)
qqgraph(data5)

#метод огибающей
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

sm.qqplot(age_small)
sm.qqplot(age_big)
plt.show()


data3 <- nba_stats[, c(19, 25, 4, 33, 30, 17, 18)]

MATRIX <- cor(data3)
col4 <- colorRampPalette(c("white", "#3F007D")) 

corrplot(MATRIX, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "white")