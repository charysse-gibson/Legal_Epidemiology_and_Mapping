######################################################################
## Title: RPHC Data
## Project: RWJF
## Author: Charysse Gibson
## Date created: February 17, 2021
######################################################################
# Purpose: Manage and clean racism as a public health crisis data
######################################################################

#-----input and explore data-----

library(data.table)
rphc <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data Collection/Master Spreadsheet/master_sheet_20201223.csv",
              colClasses = c(GEOID='character'))
names(rphc)
paste(as.character(names(rphc)), collapse=", ")
str(rphc)

# datetime as posix
rphc[,datetime:=as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]
str(rphc$datetime)

# rename columns
# setnames(rphc, old=c('',''), new=c('',''))

#-----manage indicator variables-----
rphc[,':=' (race=ifelse(race=='Yes',1,0),
            racism=ifelse(racism=='Yes',1,0),
            systemic_racism=ifelse(systemic_racism=='Yes',1,0),
            white_supremacy=ifelse(white_supremacy=='Yes',1,0),
            impact_areas=ifelse(impact_areas=='Yes',1,0),
            budget=ifelse(budget=='Yes',1,0),
            reparations=ifelse(reparations=='Yes',1,0),
            eval_measures=ifelse(eval_measures=='Yes',1,0),
            comm_engage=ifelse(comm_engage=='Yes',1,0),
            re_tools=ifelse(re_tools=='Yes',1,0))]

#-----manage duplicates-----

setkey(rphc,state,location,datetime)
rphc[,list(.N,unique_locations=nrow(rphc[,unique(location),by=state]))]
#      N unique_locations
# 1: 154              144

rphc[,newest:=ifelse(datetime==max(datetime),1,0),by=c('state','location')]

rphc_unique <- rphc[newest==1&policystat=='Passed']

#-----standardize variable names-----

# standardize capitalization of variable names
names(rphc_unique) <- toupper(names(rphc_unique))

#-----export data-----

write.csv(rphc_unique[,NEWEST:=NULL], paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/rphc_', 
                          gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)
