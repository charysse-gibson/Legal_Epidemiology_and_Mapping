######################################################################
## Title: Citations        
## Project: RWJF
## Author: Charysse Gibson
## Date created: March 22, 2021
######################################################################
# Purpose: Export citations for verification purposes
######################################################################

library(data.table)
library(Hmisc)
library(tableone)

# 1 - input data
# 2 - subset needed variables by law/ordinance (include citations)
# 3 - export dataset by law/ordinance

#-----input data-----

df1 <- fread('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/master_spreadsheet_20210315.csv',colClasses = c(GEOID='character'))

# geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_state_20201110.csv", 
#                        colClasses = c(STATE_GEOID = "character"))
# 
# setkey(df1, STATE)
# setkey(geodata_state, STATE_NAME)
# df1[geodata_state, STATE_CODE:=STATE_CODE]

#-----minimum wage-----
MW <- df1[MW_PREEMPT==1|MW_STATE_LAW==1|MW_LOCAL_LAW,
          list(LOCATION,STATE,MW_PREEMPT,MW_PREEMPT_CITATION,MW_STATE_LAW,MW_STATE_LAW_CITATION, MW_LOCAL_LAW, MW_LOCAL_LAW_CITATION)]

write.csv(MW, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/MonQcle/Citations/minimum_wage_citations_', 
                                   gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)

#-----paid sick leave-----
PSL <- df1[PSL_PREEMPT==1|PSL_STATE_LAW==1|PSL_LOCAL_LAW==1,
           list(LOCATION,STATE,PSL_PREEMPT,PSL_PREEMPT_CITATION,PSL_STATE_LAW,PSL_STATE_LAW_CITATION,PSL_LOCAL_LAW,PSL_LOCAL_LAW_CITATION)]

write.csv(PSL, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/MonQcle/Citations/paid_sick_leave_citations_', 
                                   gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)

#-----inclusionary zoning-----
IZ <- df1[IZ_PREEMPT==1|IZ_LOCAL_LAW==1,
          list(LOCATION,STATE,IZ_PREEMPT,IZ_PREEMPT_CITATION,IZ_LOCAL_LAW,IZ_LOCAL_LAW_CITATION)]

write.csv(IZ, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/MonQcle/Citations/inclusionary_zoning_citations_', 
                      gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)


#-----racial equity-----
RE <- df1[RE_ORD==1,
          list(LOCATION,STATE,RE_ORD,RE_ORD_CITATION)]

write.csv(RE, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/MonQcle/Citations/racial_equity_citations_', 
                     gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)


#-----early childhood-----
EC <- df1[EC_LOCAL_FUNDING==1,
          list(LOCATION,STATE,EC_LOCAL_FUNDING,EC_LOCAL_FUNDING_CITATION,EC_LOCAL_AGEGRP,ECH_LOCAL_SOURCE)]

write.csv(EC, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/MonQcle/Citations/early_childhood_citations_', 
                     gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)

