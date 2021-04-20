####################################
##  Datamining HW1                ##
##  2021-04-01                    ##
##  implemented by Jongheon Lee   ##
####################################

setRepositories(ind =1:8)

library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(rvest)
library(robotstxt)
library(ggplot2)
library(mapproj)
library(maps)

setwd('/home/bijh/dev/Rcodes/datamining')

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)"
page <- read_html(url)

table <- page %>% 
  html_nodes('table.wikitable.sortable') %>% 
  html_table(header = TRUE)

############################    imf data    ###################################

imf <- as.data.frame(table[1])

imf <- imf[-1,-c(1,4)] # world delete
#imf <- imf %>% 
#  mutate(rank=1:nrow(imf))
#imf <- imf[c(3,1,2)] %>% 

imf<- as_tibble(imf)
imf<- imf %>% mutate(
  Country.Territory = gsub("\\[.*?\\]","",Country.Territory),
  GDP_by_IMF = gsub(",","",GDP.millions.of.current.Int..) %>% 
    as.numeric(GDP.millions.of.current.Int..)
) %>% select(-GDP.millions.of.current.Int..)

view(imf)

############################    world bank data    ###########################

worldBank <- as.data.frame(table[2])

worldBank <- worldBank[-1,-1]

worldBank <- worldBank %>% 
  as.tibble() %>% 
  mutate(GDP_by_WorldBank = gsub(",","",GDP.millions.of.current.Int..) %>% 
           as.numeric(GDP.millions.of.current.Int..)) %>% 
  select(everything(), -GDP.millions.of.current.Int..)
head(worldBank)

############################    cia data    #################################

cia <- as.data.frame(table[3])
head(cia)
cia <- cia[-1,-1]
cia$Country.Territory <- gsub("\\[.*?\\]","",cia$Country.Territory)
cia$Year <- str_remove(cia$Year,"est.")

cia <- cia %>% 
  as_tibble() %>% 
  replace(is.na(cia), 0) %>% 
  mutate(GDP.billions.of.current.Int.. = as.numeric(str_remove(GDP.billions.of.current.Int..,",")),
         GDP.billions.of.current.Int...1 =as.numeric(GDP.billions.of.current.Int...1),
         GDP_by_cia = c(GDP.billions.of.current.Int.. + GDP.billions.of.current.Int...1)*1000
  ) %>% 
  select(-GDP.billions.of.current.Int...1,-GDP.billions.of.current.Int..)
head(cia)
############################    data merge    #################################

gdpData<-c()
gdpData <- full_join(cia,worldBank, by="Country.Territory")
gdpData <- full_join(gdpData,imf, by="Country.Territory") %>% 
  select(Country.Territory, GDP_by_IMF, GDP_by_WorldBank, GDP_by_cia, Year)

gdpData <- gdpData %>% mutate(Year = ifelse(is.na(Year),2019,Year))
# following the wiki, all the estimates were from 2019 on the IMF, World Bank Table
view(gdpData)

gdp_avg <- tibble(GDP_by_IMF = gdpData$GDP_by_IMF,
                  GDP_by_WorldBank = gdpData$GDP_by_WorldBank,
                  GDP_by_CIA =gdpData$GDP_by_cia
)

new <- apply(gdp_avg,1,mean,na.rm=T)
gdp_avg <- gdp_avg %>% mutate(new = round(new))
view(gdp_avg)

gdpData<-gdpData %>% mutate(GDP_avg = new,rank = 1:nrow(gdpData))
head(gdpData)

############## Visualization ####################

graph <- gdpData %>% filter(GDP_avg>1000000) 
# filter less than 1trillion 
View(graph)

par(mar=c(5, 7, 2, 2))
bargraph <- barplot(graph$GDP_avg/1000000, 
                    names.arg = graph$Country.Territory,
                    main = "GDP in Trillion U.S. Dollar",
                    col = 'orange',
                    # ylab = "Country",
                    xlab = "GDP_Avg(trillion)",
                    #ylim =c(0,25),
                    col.lab="orange",
                    font.lab=2,
                    cex.names = 0.7,
                    space=0.5,
                    las=1,
                    horiz=TRUE,
                    width=100,
)
axis(side=1,at=y_axis_tick)
y_axis_tick = seq(0,30,by=1)
text(x = (graph$GDP_avg/1000000) * 1, y =bargraph,labels=round(graph$GDP_avg/1000000, digits = 2) ,pos=2)
gdpData$Country.Territory <- gsub("United States","USA",gdpData$Country.Territory )
#gdpData <- data.frame(Country.Territory = tolower(row.names(gdpData)), gdpData)
gdpData

#plot1 <- ggplot(gdpData, aes(x = long, y = lat, group = group, fill = GDP_avg/1000000)) + 
#  geom_polygon(colour = "black") + 
#  coord_map("gilbert")

#view(plot1)
world_map <- map_data("world")

gdp_map <- merge(world_map, gdpData, by.x = "region", by.y = "Country.Territory")
gdp_map
gdp_map <- arrange(gdp_map, group, order)


plot1 <- ggplot(gdp_map, aes(x = long, y = lat, group = group, fill = GDP_avg/1000000)) + 
  geom_polygon(colour = "black") + 
  coord_map("gilbert")

plot1 <- plot1 + 
  scale_fill_gradient2(low = "darkblue", mid = "grey", high = "red")+labs(fill="GDP_Avg(trillion)")
# scale_fill_gradient2(low = "#559999", mid = "grey", high = "#BB650B",midpoint = median(gdpData$GDP_avg/1000000))
#plot1 <- plot1 + 
#  theme_void()
plot1

mean(gdpData$GDP_avg)
median_country<-median(gdpData$GDP_avg)
median_country <- subset(gdpData, gdpData$GDP_avg == median_country)
median_country 


gdpData$GDP_avg[1]-gdpData$GDP_avg[15] # 중국 
var((gdpData$GDP_avg)/1000000)

boxplot(graph$GDP_avg/1000000,graph$GDP_by_IMF/1000000,graph$GDP_by_WorldBank/1000000,graph$GDP_by_cia/1000000,
        main = "GDP(PPP) by 3 different organizations for comparision",
        ylab="GDP Trillion",
        names = c("Average","IMF","World Bank","CIA"),
        col = "orange",
        border = "brown",
        notch = TRUE)

