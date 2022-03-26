install.packages(c('DBI','odbc','rstudioapi'))
install.packages('tidyverse')
library(DBI)
library(odbc)
library(rstudioapi)
library(lubridate)    #year()
library(tidyverse)    #(read_csv2)

##Way1
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "met-sql19.bu.edu",
                 dbname = "NYC Real Estate",
                 Port = 1433,
                 Trusted_Connection = "True",
                 uid = rstudioapi::showPrompt("Username", 
                                              "Enter your username preceded by AD\\\\", 
                                              default = ""),
                 pwd = rstudioapi::askForPassword(prompt = "Enter your BU password"))




setwd("C:/Users/10098/Desktop/AD571/AD571 a2") #Set Working Directory(设置工作地址)

BOROUGH <- read.csv("BOROUGH.csv", header=TRUE)
BUILDING_CLASS <- read.csv("BUILDING_CLASS.csv", header=TRUE)
NEIGHBORHOOD <- read.csv("NEIGHBORHOOD.csv", header=TRUE)
NYC_HISTORICAL <- read_csv2("NYC_HISTORICAL.csv")         #use read_csv2 for cases where delimiter is ; (semicolon) (文件大且用分号隔开)

#analysis: 

#Filter the data to include only residential real estate of your neighborhood. 
NYC_HISTORICAL <- mutate(NYC_HISTORICAL, Y = year(SALE_DATE))     #take out the year, Because we want to group by year(要按年份分组，取出年)
NEIGHBORHOOD = left_join(NEIGHBORHOOD,BOROUGH,  by = 'BOROUGH_ID')  #(按ID连接镇和街区)

df <- NYC_HISTORICAL %>%
  left_join(BUILDING_CLASS, by = c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID'))  %>%
  left_join(NEIGHBORHOOD, by = 'NEIGHBORHOOD_ID')  %>%
  select(NEIGHBORHOOD_ID, SALE_DATE,SALE_PRICE,GROSS_SQUARE_FEET,TYPE,Y) %>%
  filter(NEIGHBORHOOD_ID =='29', TYPE == 'RESIDENTIAL') %>%
  group_by(Y)   
#Pipe (%>%) Operator，this operator will forward a value, or the result of an expression, into the next function call/expression.
#Join the tables together, select the required columns and filter the required values
#(将表连接整合到一起，选出需要的列并筛选需要的数值)


#1 summarize the data to show the average price of 1 square foot of real estate
AVERAGE_PRICE <- summarize(df, sum(SALE_PRICE)/ sum(GROSS_SQUARE_FEET))          #已经group过了
#numbers don't show for all years, this is because the data of 2019 include null value（2019显示空值因为数据中含空值）
table(is.na(df$GROSS_SQUARE_FEET))    #TRUE:16(16个空值)

#2 remove fields with 0 for price or square feet（去掉）（空值一起去掉了）
df2 <- df %>%
  subset(SALE_PRICE != 0) %>%
  subset(GROSS_SQUARE_FEET != 0)

#3 average price of 1 square foot of real estate(after filtering)
AVEPRICE_after <- summarize(df2, sum(SALE_PRICE)/ sum(GROSS_SQUARE_FEET))
names(AVEPRICE_after) <- c('Y', 'AVEPRICE_after')

#4 select 2 nearby neighborhoods: BAY_RIDGE(16) and  SUNSET_PARK(235)。（选出另外两个街道做同样操作）
#BAY_RIDGE(16) 
df3 <- NYC_HISTORICAL %>%
  left_join(BUILDING_CLASS, by = c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID'))  %>%
  left_join(NEIGHBORHOOD, by = 'NEIGHBORHOOD_ID')  %>%
  select(NEIGHBORHOOD_ID, SALE_DATE,SALE_PRICE,GROSS_SQUARE_FEET,TYPE,Y) %>%
  filter(NEIGHBORHOOD_ID =='16', TYPE == 'RESIDENTIAL') %>%
  group_by(Y)%>%
  subset(SALE_PRICE != 0) %>%
  subset(GROSS_SQUARE_FEET != 0)

AVEPRICE_after16 <- summarize(df3, sum(SALE_PRICE)/ sum(GROSS_SQUARE_FEET))
names(AVEPRICE_after16) <- c('Y', 'AVEPRICE_after16')

#SUNSET_PARK(235)
df4 <- NYC_HISTORICAL %>%
  left_join(BUILDING_CLASS, by = c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID'))  %>%
  left_join(NEIGHBORHOOD, by = 'NEIGHBORHOOD_ID')  %>%
  select(NEIGHBORHOOD_ID, SALE_DATE,SALE_PRICE,GROSS_SQUARE_FEET,TYPE,Y) %>%
  filter(NEIGHBORHOOD_ID =='235', TYPE == 'RESIDENTIAL') %>%
  group_by(Y)%>%
  subset(SALE_PRICE != 0) %>%
  subset(GROSS_SQUARE_FEET != 0)

AVEPRICE_after235 <- summarize(df4, sum(SALE_PRICE)/ sum(GROSS_SQUARE_FEET))
names(AVEPRICE_after235) <- c('Y', 'AVEPRICE_after235')

#compare（放在一起比较每平米价格）
compare <- AVEPRICE_after %>%
  left_join(AVEPRICE_after16, by = 'Y')  %>%
  left_join(AVEPRICE_after235, by = 'Y')  


#draw（放一起画图：一张图上显示3条折线）
draw29 <- mutate(AVEPRICE_after, NEIGHBORHOOD_NAME = 'BOROUGH_PARK')
names(draw29) <- c('YEAR','AVERAGE_PRICE','NEIGHBORHOOD_NAME')
draw16 <- mutate(AVEPRICE_after16, NEIGHBORHOOD_NAME = 'BAY_RIDGE')
names(draw16) <- c('YEAR','AVERAGE_PRICE','NEIGHBORHOOD_NAME')
draw235 <- mutate(AVEPRICE_after235, NEIGHBORHOOD_NAME = 'SUNSET_PARK')
names(draw235) <- c('YEAR','AVERAGE_PRICE','NEIGHBORHOOD_NAME')
draw <- draw29 %>%
  full_join(draw16, BY= 'YEAR') %>%
  full_join(draw235, BY= 'YEAR')

#（把整理在一起的NEIGHBORHOOD_NAME作为shape参数，即同时显示三条）
chart <- ggplot(draw, aes(x=YEAR ,y=AVERAGE_PRICE, color = NEIGHBORHOOD_NAME, shape = NEIGHBORHOOD_NAME)) +
  geom_point(size = 3) +
  geom_line(lwd = 1) +
  labs(x = 'YEAR', y = 'AVERAGE PRICE OF RECIDENTIAL')
#this chart shows the aevrage price of 1 square foot of the three neighborhoods in the past 16 years
#it is suitable for showing the changes of the average price over time

  