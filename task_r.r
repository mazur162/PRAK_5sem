# �������� ����� ��������������� ��������� warning
oldw <- getOption("warn")
options(warn = -1)

install.packages("dplyr")
# ��������� ������������� ������
installed.packages()

#�������, ����������� ��� ������ �� ����������� �������� �������
filter_data = function(data_frame, column, value){
  dplyr::filter(data_frame, !!as.symbol(column) == value)
}

#������ ���� � ������� � ������� ������ �������� 
nba_seasons <- read.csv(file = "/Users/mazur/Desktop/PRAK_5sem/nba/Seasons_Stats.csv", header = TRUE)
