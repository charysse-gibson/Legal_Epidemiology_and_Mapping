######################################################################
## Title: Master Spreadsheet
## Project: RWJF
## Author: Charysse Gibson
## Date created: March 11, 2021
######################################################################
# Purpose: Merge datasets and extracted variables
######################################################################

#-----input datasets-----

library(data.table)

RETools <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/re_tools_20210315.csv",
                 colClasses = c(GEOID='character'))

MT <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/master_tracking_20210315.csv",
                  colClasses = c(GEOID='character'))

MonQcle <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/monqcle_20210315.csv",
                 colClasses = c(GEOID='character'))

RPHC <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/rphc_20210413.csv",
              colClasses = c(GEOID='character'))

#-----merge datasets-----

# organize RETools variables
paste(as.character(names(RETools)), collapse=", ")
RETools <- RETools[,.(LOCATION, STATE, TYPE, GEOID, INTPTLAT, INTPTLONG, POLICYLINK, GARE)]

## merge master tracking to RETools
setkey(RETools,STATE,LOCATION)
setkey(MT,STATE,LOCATION)
paste(as.character(names(MT)), collapse=", ")
RETools[MT,':=' (GARE_START=GARE_START, 
                 CONSOLIDATED=CONSOLIDATED, 
                 SURVEY_RESPONSE=SURVEY_RESPONSE, 
                 CITYHEALTH=CITYHEALTH, 
                 GEO_OVERLAP=GEO_OVERLAP, 
                 OVERLAPPING_GEO=OVERLAPPING_GEO)]

## merge RPHC to RETools
setkey(RETools,STATE,LOCATION)
setkey(MonQcle,STATE,LOCATION)
paste(as.character(names(MonQcle)), collapse=", ")
RETools[MonQcle,':=' (MW_STATE_LAW=MW_STATE_LAW, 
                      MW_STATE_LAW_CITATION=MW_STATE_LAW_CITATION, 
                      MW_STATE_HOURLY=MW_STATE_HOURLY, 
                      MW_PREEMPT=MW_PREEMPT, 
                      MW_PREEMPT_CITATION=MW_PREEMPT_CITATION, 
                      MW_LOCAL_LAW=MW_LOCAL_LAW, 
                      MW_LOCAL_LAW_CITATION=MW_LOCAL_LAW_CITATION, 
                      MW_LOCAL_HOURLY=MW_LOCAL_HOURLY, 
                      PSL_STATE_LAW=PSL_STATE_LAW, 
                      PSL_STATE_LAW_CITATION=PSL_STATE_LAW_CITATION, 
                      PSL_STATE_EMPSIZE=PSL_STATE_EMPSIZE, 
                      PSL_STATE_RATESM=PSL_STATE_RATESM, 
                      PSL_STATE_RATELA=PSL_STATE_RATELA, 
                      PSL_STATE_SIZEEX=PSL_STATE_SIZEEX, 
                      PSL_PREEMPT=PSL_PREEMPT, 
                      PSL_PREEMPT_CITATION=PSL_PREEMPT_CITATION, 
                      PSL_PREEMPT_SIZEEX=PSL_PREEMPT_SIZEEX, 
                      PSL_LOCAL_LAW=PSL_LOCAL_LAW, 
                      PSL_LOCAL_LAW_CITATION=PSL_LOCAL_LAW_CITATION, 
                      PSL_LOCAL_EMPSIZE=PSL_LOCAL_EMPSIZE, 
                      PSL_LOCAL_RATESM=PSL_LOCAL_RATESM, 
                      PSL_LOCAL_RATELA=PSL_LOCAL_RATELA, 
                      PSL_LOCAL_SIZEEX=PSL_LOCAL_SIZEEX, 
                      EC_LOCAL_FUNDING=EC_LOCAL_FUNDING, 
                      EC_LOCAL_FUNDING_CITATION=EC_LOCAL_FUNDING_CITATION, 
                      EC_LOCAL_AGEGRP=EC_LOCAL_AGEGRP, 
                      ECH_LOCAL_SOURCE=ECH_LOCAL_SOURCE, 
                      IZ_PREEMPT=IZ_PREEMPT, 
                      IZ_PREEMPT_CITATION=IZ_PREEMPT_CITATION, 
                      IZ_LOCAL_LAW=IZ_LOCAL_LAW, 
                      IZ_LOCAL_LAW_CITATION=IZ_LOCAL_LAW_CITATION, 
                      IZ_APPLY_JURIS=IZ_APPLY_JURIS, 
                      RE_ORD=RE_ORD, 
                      RE_ORD_CITATION=RE_ORD_CITATION, 
                      RE_ORD_OFFICEDEPT=RE_ORD_OFFICEDEPT, 
                      RE_ORD_TASKFORCE=RE_ORD_TASKFORCE, 
                      RE_ORD_TRAINING=RE_ORD_TRAINING, 
                      RE_ORD_POLICE=RE_ORD_POLICE, 
                      RE_ORD_FUNDING=RE_ORD_FUNDING, 
                      RE_ORD_ALLPOLICIES=RE_ORD_ALLPOLICIES)]

## merge RPHC to RETools
setkey(RETools,STATE,LOCATION)
setkey(RPHC,STATE,LOCATION) 
RPHC[,RPHC:=1] # create RPHC dataset indicator
paste(as.character(names(RPHC)), collapse=", ")
RETools[RPHC, ':='(RPHC=RPHC, 
                   GOVBODY=GOVBODY, 
                   POLICYNUM=POLICYNUM, 
                   POLICYDATE=POLICYDATE, 
                   POLICYTYPE=POLICYTYPE, 
                   POLICYSTAT=POLICYSTAT, 
                   RACE=RACE, 
                   RACISM=RACISM, 
                   SYSTEMIC_RACISM=SYSTEMIC_RACISM, 
                   WHITE_SUPREMACY=WHITE_SUPREMACY, 
                   RACE_DEF=RACE_DEF, 
                   RACISM_DEF=RACISM_DEF, 
                   SYSTEMIC_RACISM_DEF=SYSTEMIC_RACISM_DEF, 
                   WHITE_SUPREMACY_DEF=WHITE_SUPREMACY_DEF, 
                   IMPACT_AREAS=IMPACT_AREAS, 
                   BUDGET=BUDGET, 
                   REPARATIONS=REPARATIONS, 
                   EVAL_MEASURES=EVAL_MEASURES, 
                   COMM_ENGAGE=COMM_ENGAGE, 
                   RE_TOOLS=RE_TOOLS, 
                   IMPACT_AREAS_ID=IMPACT_AREAS_ID, 
                   BUDGET_ID=BUDGET_ID, 
                   REPARATIONS_ID=REPARATIONS_ID, 
                   EVAL_MEASURES_ID=EVAL_MEASURES_ID, 
                   COMM_ENGAGE_ID=COMM_ENGAGE_ID, 
                   RE_TOOLS_ID=RE_TOOLS_ID, 
                   OTHER_INFO=OTHER_INFO, 
                   RESEARCH_DATETIME=DATETIME,
                   RESEARCHER=RESEARCHER)]

#-----standardize variable names-----

# standardize capitalization of variable names
names(RETools) <- toupper(names(RETools))

#-----quick explore-----

nrow(RETools[RPHC==1,]) #22

RETools[RPHC==1,.(LOCATION,STATE)]
RETools[RPHC==1,.(LOCATION,STATE,GARE,POLICYLINK,GARE_START,POLICYDATE)]

#-----export data----
write.csv(RETools, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/master_spreadsheet_', 
                     gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)
