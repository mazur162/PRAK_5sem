# Выключаю вывод предупреждающих сообщений warning
oldw <- getOption("warn")
options(warn = -1)

install.packages("dplyr")
# Проверяем установленные пакеты
installed.packages()

#Функция, фильтрующая все данные по конкретному значению столбца
filter_data = function(data_frame, column, value){
  dplyr::filter(data_frame, !!as.symbol(column) == value)
}

#Читаем файл с данными и удаляем пустые значения 
nba_seasons <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/Seasons_Stats.csv", header = TRUE)
