######################################################################
## Title: Master Tracking Data
## Project: RWJF
## Author: Charysse Gibson
## Date created: March 9, 2021
######################################################################
# Purpose: Manage and clean Master Tracking data
######################################################################

#-----input and explore data-----

library(data.table)
MT <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/master_tracking_20201116.csv")
nrow(MT) # 107 total

names(MT)
str(MT)

#-----convert variable names-----

# import codebook
paste(as.character(names(MT)), collapse=", ")
MT_codebook <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Pre_MonQcle_20210301/legal_epi_codebook.csv")

# pull variable names
names(MT_codebook)
MT_codebook <- MT_codebook[Variable_Name!='GEOID'&Variable_Name!='INTPTLAT'&Variable_Name!='INTPTLONG']

# convert variable names
names(MT) <- MT_codebook$Variable_Name
names(MT)

#-----manage data-----

library(Hmisc)
describe(MT$type)
# 27 states total

## type variable

# check type count
library(Hmisc)
describe(MT$type)
    #    n  missing distinct 
    #  107        0        4 
    # Value          City   County District     Town
    # Frequency        76       27        1        3

# change type (Town to City)
MT[type=='Town',type:='City']
describe(MT$type)
    # Value          City   County District
    # Frequency        79       27        1

## indicator variables

# convert Y/N to numeric indicator
describe(MT)
MT[,':=' (racial_equity=ifelse(racial_equity=='Y',1,0),
          prek_funding=ifelse(prek_funding=='Y',1,0),
          paid_sick_leave=ifelse(paid_sick_leave=='Y',1,0),
          preempt_psl=ifelse(preempt_psl=='Y',1,0),
          inclusionary_zoning=ifelse(inclusionary_zoning=='Y',1,0),
          preempt_iz =ifelse(preempt_iz =='Y',1,0),
          min_wage_private=ifelse(min_wage_private=='Y',1,0),
          minimum_wage=ifelse(minimum_wage=='Y',1,0),
          geo_overlap=ifelse(geo_overlap=='Y',1,0),
          cityhealth=ifelse(cityhealth=='Y',1,0))]

# convert survey response indicator
MT[survey_response=='Y',survey_response:=1]
MT[survey_response=='N',survey_response:=0]
MT[survey_response=='Z',survey_response:=NA]

# convert overlapping_geo missing to NA
MT[overlapping_geo=='',overlapping_geo:=NA]
# verify overlap
MT[geo_overlap==1]#47
MT[geo_overlap==1&!is.na(overlapping_geo)]
MT[geo_overlap==1&is.na(overlapping_geo)]
MT[geo_overlap==0&!is.na(overlapping_geo)]

#-----manage geodata-----

# import geodata
geodata <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_20201110.csv", 
                 colClasses = c(GEOID = "character"))

# import state codes data
geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_state_20201110.csv", 
                       colClasses = c(STATE_GEOID = "character"))

# change state codes to full state names
setkey(MT, state)
names(geodata_state)
setkey(geodata_state, STATE_CODE)
MT[geodata_state, state:=STATE_NAME]
describe(MT$state)

# change special location names
MT[location=='Council of the District of Columbia',location:='District of Columbia']
MT[location=='Nashville',location:='Nashville-Davidson']
MT[location=='Nassau (Long Island)',location:='Nassau']
MT[location=='Saint Paul',location:='St. Paul']

# merge geodata
setkey(geodata,NAME,STATE_NAME)
setkey(MT,location,state)
names(geodata)
MT[geodata,':=' 
   (GEOID=GEOID,
     INTPTLAT=INTPTLAT,
     INTPTLONG=INTPTLONG)]

# check for merging errors
errors <- MT[is.na(GEOID),]
errors <- tibble::as_tibble(errors)

# change names back
# RETools[Location=='District of Columbia',Location:='Council of the District of Columbia']
MT[location=='Nashville-Davidson',location:='Nashville']
# RETools[Location=='Nassau',Location:='Nassau (Long Island)']
MT[location=='St. Paul',location:='Saint Paul']

##-----extract needed variables-----

# convert variables to uppercase
names(MT) <- toupper(names(MT))
paste(as.character(names(MT)), collapse=", ")

MT_vars <- MT[,.(LOCATION, 
                 STATE, 
                 GARE_START, 
                 SURVEY_RESPONSE, 
                 CONSOLIDATED, 
                 GEO_OVERLAP, 
                 OVERLAPPING_GEO, 
                 CITYHEALTH, 
                 GEOID)]

##-----export data-----

write.csv(MT_vars, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/master_tracking_', 
                          gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)

