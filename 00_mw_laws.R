######################################################################
## Title: Minimum Wage Laws Data
## Project: RWJF
## Author: Charysse Gibson
## Date created: April 22, 2021
######################################################################
# Purpose: Manage and analyze minimum wage laws data
######################################################################

#-----input and explore data-----

library(data.table)
mw_laws <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/mw_laws_20210413.csv")
names(mw_laws)
mw_laws <- mw_laws[,.(LOCATION,STATE,TYPE,GARE_START,DATE,GOV_LEVEL,STATUS,ACTION)]

GARE_start <- unique(mw_laws,by=c('LOCATION','STATE','TYPE','GARE_START')) # total 63 cities and counties
GARE_start <- GARE_start[,.(LOCATION,STATE,TYPE,GARE_START)]

#-----bar chart GARE start dates-----

# extract year count
year_count <- data.table(table(GARE_start[,.(GARE_START)]))
names(year_count) <- c('GARE_START_YEAR', 'N')
str(year_count)
year_count[,GARE_START_YEAR:=as.integer(GARE_START_YEAR)]

# create years dataset
years <- seq(2000,2019,1)
GARE_start_years <- as.data.table(years) 
names(GARE_start_years) <- 'GARE_START_YEAR'
# merge year count
setkey(year_count,GARE_START_YEAR)
setkey(GARE_start_years,GARE_START_YEAR)
GARE_start_years[year_count,N:=N]
# GARE_start_years[is.na(N),N:=0]

library(ggplot2)
library(ggthemes)
windows(10,5)
ggplot(data=GARE_start_years, aes(x=GARE_START_YEAR, y=N, group=1)) +
  geom_bar(stat="identity", width =1,color="white", fill="#008A84") +
  geom_text(aes(label=N), vjust=-0.3, size=3.5) +
  labs(x="Year", y = "Count") +
  scale_x_continuous(breaks=seq(2000,2020,1)) +
  scale_y_continuous(breaks=seq(0,25,5)) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color="black", size=9)) 

