######################################################################
## Title: Survey Responses Data        
## Project: RWJF
## Author: Charysse Gibson
## Date created: March 2, 2021
######################################################################
# Purpose: Manage and clean survey responses data, 
#          identify survey response locations
######################################################################

#-----input and explore data-----

library(data.table)
survey_responses <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/survey_responses_20210226.csv")
nrow(survey_responses) # 37 total

names(survey_responses)
str(survey_responses)

#-----manage state codes-----

# import state codes data
geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_state_20201110.csv", 
                       colClasses = c(STATE_GEOID = "character"))

# change state codes to full state names
setkey(survey_responses, STATE)
names(geodata_state)
setkey(geodata_state, STATE_CODE)
survey_responses[geodata_state, STATE:=STATE_NAME]

##-----survey response indicator variable-----

# unique locations
names(survey_responses)
survey_unique <- unique(survey_responses,by=c('LOCATION','STATE'))

# extract location and state only and sort
survey_response_locations <- survey_unique[,.(LOCATION,STATE)]
setkey(survey_response_locations,STATE,LOCATION)

##-----export data-----

write.csv(survey_responses, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/survey_responses_', 
                          gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)

##-----cross check with old master tracking data-----

# write.csv(survey_response_locations, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/survey_response_locations_', 
#                                    gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)
#
# mastertracking <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/pre_monqcle_20210301/master_spreadsheet_20210302.csv",
#                         colClasses = c(GEOID = "character"))
# 
# survey_response_locations[,srl:=1]
# 
# names(mastertracking)
# setkey(mastertracking,LOCATION,STATE)
# names(survey_response_locations)
# setkey(survey_response_locations,LOCATION,STATE)
# 
# mastertracking[survey_response_locations,srl:=srl]
# nrow(mastertracking[!is.na(srl)])
# survey_response_locations[mastertracking,SURVEY_RESPONSE:=SURVEY_RESPONSE]
# survey_response_locations[is.na(SURVEY_RESPONSE)]
