###########################################################
## Title: 02_stats_and_mapping.R
## Project: RWJF
## By: Charysse Gibson
## Date: April 7, 2021
###########################################################
## Purpose: Statistics & Mapping
###########################################################

library(data.table)
library(Hmisc)
library(tableone)

#-----input & manage data-----

master_spreadsheet <- fread('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/master_spreadsheet_20210413.csv',colClasses = c(GEOID='character'))

rphc_only <- fread('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/rphc_20210413.csv',colClasses = c(GEOID='character'))

geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/geodata_state_regions_20210329.csv", 
                       colClasses = c(STATE_GEOID = "character"))

political_leanings <- fread('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/political_leanings_20210505.csv')

# Create state code & state regions
setkey(master_spreadsheet, STATE)
setkey(geodata_state, STATE_NAME)
## create state code & state regions
master_spreadsheet[geodata_state, STATE_CODE:=STATE_CODE]
master_spreadsheet[geodata_state, STATE_REGION:=STATE_REGION]
## create strata
master_spreadsheet[GARE==1&POLICYLINK==0,STRATA:="GARE Only"]
master_spreadsheet[GARE==0&POLICYLINK==1,STRATA:="PolicyLink Only"]
master_spreadsheet[GARE==1&POLICYLINK==1,STRATA:="GARE + PolicyLink"]

# create state clusters
state_clusters <- data.table(table(master_spreadsheet[,.(STATE)]))
names(state_clusters) <- c('STATE', 'N')
## merge state clusters count to region and state fips codes
state_data <- geodata_state[!is.na(STATE_REGION),]
## rename state_date variables
names(state_data) <- c('STATE','STATE_CODE','GEOID','REGION','DIVISION')
setkey(state_data,STATE)
setkey(state_clusters,STATE)
state_data[state_clusters,N:=N]
state_data[is.na(N),N:=0]

# create RPHC state clusters
rphc_state_clusters <- data.table(table(master_spreadsheet[RPHC==1,.(STATE)]))
names(rphc_state_clusters) <- c('STATE', 'RPHC_N')
## merge rphc state clusters to state clusters data
setkey(state_data,STATE)
setkey(rphc_state_clusters,STATE)
state_data[rphc_state_clusters,RPHC_N:=RPHC_N]
state_data[is.na(RPHC_N),RPHC_N:=0]

## convert regions to factor
state_data$REGION <- factor(state_data$REGION, levels = c("Northeast", "Midwest", "South", "West"))
str(state_data)

#-----descriptive statistics-----

# descriptive stats table for overall
dput(names(master_spreadsheet))
## vector of all variables
allVars <- c("STATE_REGION", "TYPE", "POLICYLINK", "GARE", "GARE_START", "CONSOLIDATED", "SURVEY_RESPONSE", "CITYHEALTH", "GEO_OVERLAP", "MW_STATE_LAW", "MW_STATE_HOURLY", "MW_PREEMPT", "MW_LOCAL_LAW", "MW_LOCAL_HOURLY", "PSL_STATE_LAW", "PSL_STATE_EMPSIZE", "PSL_STATE_RATESM", "PSL_STATE_RATELA", "PSL_STATE_SIZEEX", "PSL_PREEMPT", "PSL_PREEMPT_SIZEEX", "PSL_LOCAL_LAW", "PSL_LOCAL_EMPSIZE", "PSL_LOCAL_RATESM", "PSL_LOCAL_RATELA", "PSL_LOCAL_SIZEEX", "EC_LOCAL_FUNDING", "EC_LOCAL_AGEGRP", "ECH_LOCAL_SOURCE", "IZ_PREEMPT", "IZ_LOCAL_LAW", "IZ_APPLY_JURIS", "RE_ORD", "RE_ORD_OFFICEDEPT", "RE_ORD_TASKFORCE", "RE_ORD_TRAINING", "RE_ORD_POLICE", "RE_ORD_FUNDING", "RE_ORD_ALLPOLICIES", "RPHC", "POLICYTYPE", "POLICYSTAT", "RACE", "RACISM", "SYSTEMIC_RACISM", "WHITE_SUPREMACY", "IMPACT_AREAS", "BUDGET", "REPARATIONS", "EVAL_MEASURES", "COMM_ENGAGE", "RE_TOOLS")
## vector of categorical variables
catVars <- c("STATE_REGION", "TYPE", "POLICYLINK", "GARE", "CONSOLIDATED", "SURVEY_RESPONSE", "CITYHEALTH", "GEO_OVERLAP", "MW_STATE_LAW", "MW_PREEMPT", "MW_LOCAL_LAW", "PSL_STATE_LAW", "PSL_STATE_SIZEEX", "PSL_PREEMPT", "PSL_PREEMPT_SIZEEX", "PSL_LOCAL_LAW", "PSL_LOCAL_SIZEEX", "EC_LOCAL_FUNDING", "EC_LOCAL_AGEGRP", "ECH_LOCAL_SOURCE", "IZ_PREEMPT", "IZ_LOCAL_LAW", "IZ_APPLY_JURIS", "RE_ORD", "RE_ORD_OFFICEDEPT", "RE_ORD_TASKFORCE", "RE_ORD_TRAINING", "RE_ORD_POLICE", "RE_ORD_FUNDING", "RE_ORD_ALLPOLICIES", "RPHC", "POLICYTYPE", "POLICYSTAT", "RACE", "RACISM", "SYSTEMIC_RACISM", "WHITE_SUPREMACY", "IMPACT_AREAS", "BUDGET", "REPARATIONS", "EVAL_MEASURES", "COMM_ENGAGE", "RE_TOOLS")
## Create TableOne object
tab1 <- CreateTableOne(vars = allVars, data = master_spreadsheet, factorVars = catVars)
summary(tab1)

# descriptive stats tables for strata
stratafied_tab1 <- CreateTableOne(vars = allVars, strata = "STRATA", data = master_spreadsheet, factorVars = catVars)
stratafied_tab1
summary(stratafied_tab1)

#----RET use & RPHC mapping----

# merge RET use to state_laea spatial data
library(tidycensus)
data("state_laea", package = 'tidycensus')
## merge variables to spatial data
library(sp)
state_laea <- merge(state_laea, state_data, by='GEOID', all.x=TRUE)
## convert to spatial data
library(sf)
states_spat <- as_Spatial(state_laea)

library(tmap)
library(tmaptools)
# tmaptools::palette_explorer()

tmap_mode('plot')
# tmap_mode('view')

# State clusters
windows(45,40)
tm_shape(states_spat) +
  tm_layout(bg.color='white', frame=FALSE, inner.margins = c(.07,.0,.0,.06),
            legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
  tm_polygons(col = 'REGION', title='Region',
              border.col = 'white', lwd=1.5, palette=c('#E44D35','#FEB441','#6ABF72','#299EC1')) +
  tm_credits(text='Note: A total of 107 cites and counties have used GARE or PolicyLink racial equity tools.',
             position = c('center','bottom'), size = .75) +
  # tm_credits(text='Note: GARE = Government Alliance on Race and Equity. A total of \n107 cites and counties have used GARE or PolicyLink racial equity tools.\n \nSource: The Institute for Healing Justice and Equity, Center for Health \nLaw Studies at Saint Louis University, Are Cities and Counties Ready \nto Use Racial Equity Tools to Influence Policy? (2020).',
  #            position = c('left','bottom')) +
tm_shape(states_spat[states_spat$REGION=='Northeast',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Reds", contrast = c(.42,.6))) +
tm_shape(states_spat[states_spat$REGION=='Midwest',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("YlOrRd", contrast = c(.28,.38))) +
tm_shape(states_spat[states_spat$REGION=='South',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,10), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Greens", contrast = c(.35,.55))) +
tm_shape(states_spat[states_spat$REGION=='West',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,21), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("GnBu", contrast = c(.57,.72))) +
tm_shape(states_spat[states_spat$N!=0&states_spat$STATE_CODE!='MI'&states_spat$STATE_CODE!='LA'&states_spat$STATE_CODE!='FL'&states_spat$STATE_CODE!='MA'&states_spat$STATE_CODE!='MD'&states_spat$STATE_CODE!='DC',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='FL',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='LA',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='MI',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='MA',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='MD',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

#RPHC
windows(45,40)
tm_shape(states_spat) +
  tm_layout(main.title='Cities and counties that have used GARE or PolicyLink racial equity tools and declared \nracism as a public health crisis, by state and region, 2020, by state and region, 2020', scale=1, main.title.size = .9,
            bg.color='white', frame=FALSE, inner.margins = c(.09,.0,.04,.06),
            legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
  tm_polygons(col = 'REGION', title='Region',
              border.col = 'white', lwd=1.5, palette=c('#E44D35','#FEB441','#6ABF72','#299EC1')) +
  tm_credits(text='Note: A total of 22 cities and counties that have used GARE or PolicyLink racial equity tools have declared racism \nas a public health crisis.',
             position = c('left','bottom'), size = .75) +
tm_shape(states_spat[states_spat$REGION=='Northeast',]) +
  tm_polygons(col = 'RPHC_N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Reds", contrast = c(.42,.6))) +
tm_shape(states_spat[states_spat$REGION=='Midwest',]) +
  tm_polygons(col = 'RPHC_N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("YlOrRd", contrast = c(.28,.38))) +
tm_shape(states_spat[states_spat$REGION=='South',]) +
  tm_polygons(col = 'RPHC_N', legend.show=FALSE, title='State total', breaks=c(.5,1,10), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Greens", contrast = c(.35,.55))) +
tm_shape(states_spat[states_spat$REGION=='West',]) +
  tm_polygons(col = 'RPHC_N', legend.show=FALSE, title='State total', breaks=c(.5,1,21), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("GnBu", contrast = c(.57,.72))) +
tm_shape(states_spat[states_spat$RPHC_N!=0&states_spat$STATE_CODE!='MI'&states_spat$STATE_CODE!='LA'&states_spat$STATE_CODE!='MA'&states_spat$STATE_CODE!='DC',]) +
  tm_text(text='RPHC_N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='MI',]) +
  tm_text(text='RPHC_N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
tm_shape(states_spat[states_spat$STATE_CODE=='MA',]) +
  tm_text(text='RPHC_N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.12, xmod=-.1, remove.overlap = TRUE)

#----RET use plotting----
library(ggplot2)

# stacked barplot vertical
x <- factor(c("Northeast","Midwest","South","West"), 
            levels=c("Northeast","Midwest","South","West"))
y <- factor(c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"), 
            levels=c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"))
plot_data <- as.data.table(merge(x,y,all=TRUE)) 
z <- c(3,33,15,26,2,1,6,2,4,2,6,7)
plot_data[,z:=z]
names(plot_data) <- c('Region','RETools','N')

windows(6,5)
ggplot(plot_data, aes(fill=RETools, y=Region, x=N, label = N)) + 
  geom_bar(stat="identity", width =.5)+
  scale_fill_manual(values=c("#008A84", "#D46800", "#4F4F4F"))+
  geom_text(size = 3, color='white', position = position_stack(vjust = 0.55)) +
  labs(x = "Number of US cities/counties", y='Region', fill = "Racial Equity Tool Used") +
  theme_minimal() +
  theme(legend.position="right", legend.direction = "vertical", 
        legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  coord_flip()

# stacked barplot horizontal
x1 <- factor(c("Northeast","Midwest","South","West"), 
            levels=c("West","South","Midwest","Northeast"))
y1 <- factor(c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"), 
            levels=c("Both GARE & PolicyLink", "PolicyLink Only" , "GARE Only"))
plot_data1 <- as.data.table(merge(x1,y1,all=TRUE)) 
z1 <- c(3,33,15,26,2,1,6,2,4,2,6,7)
plot_data1[,z1:=z1]
names(plot_data1) <- c('Region','RETools','N')

windows(8,4)
ggplot(plot_data1, aes(fill=RETools, y=Region, x=N, label = N)) + 
  geom_bar(stat="identity", width =.5)+
  scale_fill_manual(values=c("#4F4F4F", "#D46800", "#008A84"))+
  geom_text(size = 3, color='white', position = position_stack(vjust = 0.5)) +
  labs(x = "Number of US cities/counties", y='Region', fill = "Racial Equity Tool Used") +
  theme_minimal() +
  theme(legend.position="bottom", axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +  guides(fill = guide_legend(reverse = TRUE))
  # coord_cartesian(xlim = c(0, 50))


# dodged barplot (overall included)
# x2 <- factor(c("Northeast","Midwest","South","West"), 
#              levels=c("Northeast","Midwest","South","West"))
# y2 <- factor(c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink",'Overall'), 
#              levels=c('Overall',"GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"))
# plot_data2 <- as.data.table(merge(x2,y2,all=TRUE)) 
# z2 <- c(3,33,15,26,2,1,6,2,4,2,6,7,9,36,27,35)
# plot_data2[,z2:=z2]
# names(plot_data2) <- c('Region','RETools','N')
# 
# windows(8,7)
# ggplot(plot_data2, aes(fill=RETools, y=Region, x=N)) + 
# geom_bar(stat="identity", position=position_dodge())+
#   geom_text(aes(label=N), vjust=-.7, color="black",
#             position = position_dodge(0.9), size=4)+
#   scale_fill_manual(values=c("#B3B3B3", "#008A84", "#D46800","#4F4F4F"))+
#   theme_minimal() +
#   labs(x = "Number of US cities/counties", y='Region', fill = "Racial Equity Tool Used") +
#   theme(legend.position="bottom", axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
#   coord_flip()

#----rphc only data & mapping----

# remove N/A location(s)
rphc_only <- rphc_only[LOCATION!='N/A']
nrow(rphc_only) #138

# create rphc only state clusters
rphc_only_state_clusters <- data.table(table(rphc_only[,.(STATE)]))
names(rphc_only_state_clusters) <- c('STATE', 'N')
sum(rphc_only_state_clusters$N)
## merge state clusters count to region and state fips codes
rphc_only_state_data <- geodata_state[!is.na(STATE_REGION),]
## rename state_date variables
names(rphc_only_state_data) <- c('STATE','STATE_CODE','GEOID','REGION','DIVISION')
setkey(rphc_only_state_data,STATE)
setkey(rphc_only_state_clusters,STATE)
rphc_only_state_data[rphc_only_state_clusters,N:=N]
rphc_only_state_data[is.na(N),N:=0]
sum(rphc_only_state_data$N)

# convert regions to factor
rphc_only_state_data$REGION <- factor(rphc_only_state_data$REGION, levels = c("Northeast", "Midwest", "South", "West"))
str(rphc_only_state_data)

# merge rphc only to state_laea spatial data
library(tidycensus)
data("state_laea", package = 'tidycensus')
## merge variables to spatial data
library(sp)
rphc_state_laea <- merge(state_laea, rphc_only_state_data, by='GEOID', all.x=TRUE)
## convert to spatial data
library(sf)
rphc_states_spat <- as_Spatial(rphc_state_laea)

# rphc only mapping
library(tmap)
library(tmaptools)
# tmaptools::palette_explorer()
tmap_mode('plot')
# tmap_mode('view')

windows(45,40)
tm_shape(rphc_states_spat) +
  tm_layout(main.title='Cities and counties that have declared racism as a public health crisis, by state and region, 2020', scale=1, main.title.size = .9,
            bg.color='white', frame=FALSE, inner.margins = c(.07,.0,.04,.06),
            legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
  tm_polygons(col = 'REGION', title='Region',
              border.col = 'white', lwd=1.5, palette=c('#E44D35','#FEB441','#6ABF72','#299EC1')) +
  tm_credits(text='Note: A total of 138 cities and counties have declared racism as public health crisis.',
             position = c('left','bottom'), size = .75) +
  tm_shape(rphc_states_spat[rphc_states_spat$REGION=='Northeast',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Reds", contrast = c(.42,.6))) +
  tm_shape(rphc_states_spat[rphc_states_spat$REGION=='Midwest',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("YlOrRd", contrast = c(.28,.38))) +
  tm_shape(rphc_states_spat[rphc_states_spat$REGION=='South',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,10), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Greens", contrast = c(.35,.55))) +
  tm_shape(rphc_states_spat[rphc_states_spat$REGION=='West',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,21), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("GnBu", contrast = c(.57,.72))) +
  tm_shape(rphc_states_spat[rphc_states_spat$N!=0&rphc_states_spat$STATE_CODE!='MI'&rphc_states_spat$STATE_CODE!='RI'&rphc_states_spat$STATE_CODE!='NJ'&rphc_states_spat$STATE_CODE!='CT'&rphc_states_spat$STATE_CODE!='FL'&rphc_states_spat$STATE_CODE!='MA'&rphc_states_spat$STATE_CODE!='MD'&rphc_states_spat$STATE_CODE!='DC',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='FL',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='NJ',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.2, xmod=.15, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='MI',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.75, xmod=.45, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='MA',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.14, xmod=-.15, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='CT',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=0, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='RI',]) +
  tm_text(text='N', size=.7, col='black', shadow=FALSE, bg.alpha = 0, remove.overlap = TRUE) +
  # tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='RI',]) +
  #   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.3, xmod=.4, remove.overlap = TRUE) +
  tm_shape(rphc_states_spat[rphc_states_spat$STATE_CODE=='MD',]) +
  tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)


# stacked barplot vertical
x3 <- factor(c("Northeast","Midwest","South","West"), 
            levels=c("Northeast","Midwest","South","West"))
y3 <- factor(c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"), 
            levels=c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"))
plot_data3 <- as.data.table(merge(x3,y3,all=TRUE)) 
z3 <- c(1,8,3,2,NA,NA,NA,NA,4,1,1,2)
plot_data3[,z3:=z3]
names(plot_data3) <- c('Region','RETools','N')

windows(4.5,5)
ggplot(plot_data3, aes(fill=RETools, y=Region, x=N, label = N)) + 
  geom_bar(stat="identity", width =.5)+
  scale_fill_manual(values=c("#008A84", "#4F4F4F", "#4F4F4F"))+
  geom_text(size = 3, color='white', position = position_stack(vjust = 0.50)) +
  labs(x = "Number of cities/counties", y='Region', fill = "Racial Equity Tool Used") +
  theme_minimal() +
  theme(legend.position = c(.75,.85), legend.direction = "vertical", 
        legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="black"), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  coord_flip()

# stacked barplot horizontal
x4 <- factor(c("Northeast","Midwest","South","West"), 
             levels=c("West","South","Midwest","Northeast"))
y4 <- factor(c("GARE Only" , "PolicyLink Only" , "Both GARE & PolicyLink"), 
             levels=c("Both GARE & PolicyLink", "PolicyLink Only" , "GARE Only"))
plot_data4 <- as.data.table(merge(x4,y4,all=TRUE)) 
z4 <- c(4,1,1,2,NA,NA,NA,NA,1,8,3,2)
plot_data4[,z4:=z4]
names(plot_data4) <- c('Region','RETools','N')

windows(8,4)
ggplot(plot_data4, aes(fill=RETools, y=Region, x=N, label = N)) + 
  geom_bar(stat="identity", width =.5)+
  scale_fill_manual(values=c("#4F4F4F", "#008A84", "#008A84"))+
  geom_text(size = 3, color='white', position = position_stack(vjust = 0.50)) +
  labs(x = "Number of cities/counties", y='Region', fill = "Racial Equity Tool Used") +
  theme_minimal() +
  theme(legend.position = c(.8,.26), legend.direction = "vertical", 
        legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="black"), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  guides(fill = guide_legend(reverse = TRUE))

#----governor political leanings maps----

# create political leanings state data
state_data
## merge political leanings state clusters to state clusters data
setkey(state_data,STATE_CODE)
setkey(political_leanings,STATE_CODE)
pol_state_data <- merge(state_data,political_leanings, all.x=TRUE)

# merge political leanings to state_laea spatial data
library(tidycensus)
data("state_laea", package = 'tidycensus')
## merge variables to spatial data
library(sp)
pol_state_laea <- merge(state_laea, pol_state_data, by='GEOID', all.x=TRUE)
## convert to spatial data
library(sf)
pol_states_spat <- as_Spatial(pol_state_laea)

# political leanings maps
library(tmap)
library(tmaptools)
# tmaptools::palette_explorer()
tmap_mode('plot')
# tmap_mode('view')

# describe(pol_state_data)
# sum(pol_state_data[GOV_2013_2016=='D',N]) #69
# sum(pol_state_data[GOV_2013_2016=='R',N]) #37
# DC was NA for 2013-2017

# Political Party of Governor 2013-2016 
# gov_2013_2016 <-
# windows(50,40)
# tm_shape(pol_states_spat) +
#   tm_layout(main.title='2013-2016', scale=1, main.title.size = .9, main.title.position = 'center',
#             bg.color='white', frame=FALSE, inner.margins = c(.1,.0,.04,.0), legend.show = FALSE,
#             legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
#   tm_polygons(col = 'GOV_2013_2016', title='Political Party', showNA=FALSE, 
#               border.col = 'white', lwd=1.5, palette=c('#2E7EBB','#D92522','#747474'),
#               labels = c("Democrat", 
#                          "Republican", 
#                          "Independent")) +
#   tm_credits(text='Totals: Democratic = 69, Republican = 37.',
#              position = c('center','bottom'), size = .75) +
# tm_shape(pol_states_spat[pol_states_spat$GOV_2013_2016=='D'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Blues", contrast = c(.4,.7))) +
# tm_shape(pol_states_spat[pol_states_spat$GOV_2013_2016=='R'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Reds", contrast = c(.4,.7))) +
# tm_shape(pol_states_spat[pol_states_spat$GOV_2013_2016=='I'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,10), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Greys", contrast = c(.35,.55))) +
# tm_shape(pol_states_spat[pol_states_spat$N!=0&pol_states_spat$STATE_CODE!='MI'&pol_states_spat$STATE_CODE!='LA'&pol_states_spat$STATE_CODE!='FL'&pol_states_spat$STATE_CODE!='MA'&pol_states_spat$STATE_CODE!='MD'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='FL',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='LA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MI',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MD',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

sum(pol_state_data[GOV_2017_2020=='D',N]) #83
sum(pol_state_data[GOV_2017_2020=='R',N]) #23
# DC was NA for 2017-2020

# Political Party of Governor 2017-2020 
# gov_2017_2020 <-
windows(50,40)
tm_shape(pol_states_spat) +
  tm_layout(bg.color='white', frame=FALSE, inner.margins = c(.09,.0,.0,.05),
            legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .35), legend.text.size = .85) +
  tm_polygons(col = 'GOV_2017_2020', title='Political Party', showNA=FALSE, 
              border.col = 'white', lwd=1.5, palette=c('#2E7EBB','#D92522','#747474'),
              labels = c("Democrat", 
                         "Republican", 
                         "Independent")) +
  tm_credits(text='Note: A total of 83 jurisdictions with Democratic govenors and 23 jurisdictions with Republican governors \nhave used GARE or PolicyLink racial equity tools. The District of Columbia does not have a governor.',
             position = c('center','bottom'), size = .85) +
tm_shape(pol_states_spat[pol_states_spat$GOV_2017_2020=='D'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Blues", contrast = c(.4,.7))) +
tm_shape(pol_states_spat[pol_states_spat$GOV_2017_2020=='R'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Reds", contrast = c(.4,.7))) +
tm_shape(pol_states_spat[pol_states_spat$N!=0&pol_states_spat$STATE_CODE!='MI'&pol_states_spat$STATE_CODE!='LA'&pol_states_spat$STATE_CODE!='FL'&pol_states_spat$STATE_CODE!='MA'&pol_states_spat$STATE_CODE!='MD'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='FL',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='LA',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MI',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MA',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MD',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

library(grid)
windows(90,40)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(gov_2013_2016, vp=viewport(layout.pos.col = 1))
print(gov_2017_2020, vp=viewport(layout.pos.col = 2))

#----president political leanings maps----

# create political leanings state data
state_data
## merge political leanings state clusters to state clusters data
setkey(state_data,STATE_CODE)
setkey(political_leanings,STATE_CODE)
pol_state_data <- merge(state_data,political_leanings, all.x=TRUE)

# merge political leanings to state_laea spatial data
library(tidycensus)
data("state_laea", package = 'tidycensus')
## merge variables to spatial data
library(sp)
pol_state_laea <- merge(state_laea, pol_state_data, by='GEOID', all.x=TRUE)
## convert to spatial data
library(sf)
pol_states_spat <- as_Spatial(pol_state_laea)

# political leanings maps
library(tmap)
library(tmaptools)
# tmaptools::palette_explorer()
tmap_mode('plot')
# tmap_mode('view')

# describe(pol_state_data)
# sum(pol_state_data[PRES_2012=='D',N]) #84
# sum(pol_state_data[PRES_2012=='R',N]) #23

# Political Party of President 2012
# windows(50,40)
# pres_2012 <-
# tm_shape(pol_states_spat) +
#   tm_layout(main.title='2012', scale=1, main.title.size = 1.1, main.title.position = 'center',
#             bg.color='white', frame=FALSE, inner.margins = c(.1,.0,.04,.0), legend.show = FALSE,
#             legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
#   tm_polygons(col = 'PRES_2012', title='Political Party', showNA=FALSE, 
#               border.col = 'white', lwd=1.5, palette=c('#2E7EBB','#D92522','#747474'),
#               labels = c("Democrat", 
#                          "Republican", 
#                          "Independent")) +
#   tm_credits(text='Totals: Democratic = 84, Republican = 23.',
#              position = c('center','bottom'), size = .9) +
# tm_shape(pol_states_spat[pol_states_spat$PRES_2012=='D'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Blues", contrast = c(.4,.7))) +
# tm_shape(pol_states_spat[pol_states_spat$PRES_2012=='R'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Reds", contrast = c(.4,.7))) +
# tm_shape(pol_states_spat[pol_states_spat$N!=0&pol_states_spat$STATE_CODE!='MI'&pol_states_spat$STATE_CODE!='LA'&pol_states_spat$STATE_CODE!='FL'&pol_states_spat$STATE_CODE!='MA'&pol_states_spat$STATE_CODE!='MD'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='FL',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='LA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MI',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
# tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MD',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

# sum(pol_state_data[PRES_2016=='D',N]) #67
# sum(pol_state_data[PRES_2016=='R',N]) #40

# Political Party of President 2016
# windows(50,40)
# pres_2016 <-
# tm_shape(pol_states_spat) +
#   tm_layout(main.title='2016', scale=1, main.title.size = 1.1, main.title.position = 'center',
#             bg.color='white', frame=FALSE, inner.margins = c(.1,.0,.04,.0), legend.show = FALSE,
#             legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .3), legend.text.size = .8) +
#   tm_polygons(col = 'PRES_2016', title='Political Party', showNA=FALSE, 
#               border.col = 'white', lwd=1.5, palette=c('#2E7EBB','#D92522','#747474'),
#               labels = c("Democrat", 
#                          "Republican", 
#                          "Independent")) +
#   tm_credits(text='Totals: Democratic = 67, Republican = 40.',
#              position = c('center','bottom'), size = .9) +
#   tm_shape(pol_states_spat[pol_states_spat$PRES_2016=='D'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Blues", contrast = c(.4,.7))) +
#   tm_shape(pol_states_spat[pol_states_spat$PRES_2016=='R'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
#               palette=get_brewer_pal("Reds", contrast = c(.4,.7))) +
#   tm_shape(pol_states_spat[pol_states_spat$N!=0&pol_states_spat$STATE_CODE!='MI'&pol_states_spat$STATE_CODE!='LA'&pol_states_spat$STATE_CODE!='FL'&pol_states_spat$STATE_CODE!='MA'&pol_states_spat$STATE_CODE!='MD'&pol_states_spat$STATE_CODE!='DC',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
#   tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='FL',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
#   tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='LA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
#   tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MI',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
#   tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MA',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
#   tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MD',]) +
#   tm_text(text='N', size=.8, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

sum(pol_state_data[PRES_2020=='D',N]) #80
sum(pol_state_data[PRES_2020=='R',N]) #27

# Political Party of President 2020
windows(50,40)
# pres_2020 <-
tm_shape(pol_states_spat) +
  tm_layout(bg.color='white', frame=FALSE, inner.margins = c(.09,.0,.0,.05),
            legend.frame=FALSE, legend.frame.lwd = .1, legend.position = c(.86, .35), legend.text.size = .85) +
  tm_polygons(col = 'PRES_2020', title='Political Party', showNA=FALSE, 
              border.col = 'white', lwd=1.5, palette=c('#2E7EBB','#D92522','#747474'),
              labels = c("Democrat", 
                         "Republican", 
                         "Independent")) +
  tm_credits(text='Note: A total of 80 jurisdictions that voted Democrat and 27 jurisdictions that voted Republican \nin the 2020 presidential election have used GARE or PolicyLink racial equity tools.',
             position = c('center','bottom'), size = .85) +
tm_shape(pol_states_spat[pol_states_spat$PRES_2020=='D'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,5), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Blues", contrast = c(.4,.7))) +
tm_shape(pol_states_spat[pol_states_spat$PRES_2020=='R'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_polygons(col = 'N', legend.show=FALSE, title='State total', breaks=c(.5,1,20), showNA=FALSE, border.col = 'white', lwd=1.5,
              palette=get_brewer_pal("Reds", contrast = c(.4,.7))) +
tm_shape(pol_states_spat[pol_states_spat$N!=0&pol_states_spat$STATE_CODE!='MI'&pol_states_spat$STATE_CODE!='LA'&pol_states_spat$STATE_CODE!='FL'&pol_states_spat$STATE_CODE!='MA'&pol_states_spat$STATE_CODE!='MD'&pol_states_spat$STATE_CODE!='DC',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=0, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='FL',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, xmod=.6, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='LA',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.2, xmod=-.3, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MI',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=-.6, xmod=.5, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MA',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.1, xmod=-.1, remove.overlap = TRUE) +
tm_shape(pol_states_spat[pol_states_spat$STATE_CODE=='MD',]) +
  tm_text(text='N', size=.85, col='black', shadow=FALSE, bg.alpha = 0, ymod=.28, xmod=-.1, remove.overlap = TRUE)

library(grid)
windows(90,30)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(pres_2012, vp=viewport(layout.pos.col = 1))
print(pres_2016, vp=viewport(layout.pos.col = 2))
print(pres_2020, vp=viewport(layout.pos.col = 3))

##----OUTPUTS----
