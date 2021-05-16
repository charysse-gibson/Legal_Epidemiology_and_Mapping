######################################################################
## Title: RE Tools Data
## Project: RWJF
## Author: Charysse Gibson
## Date created: February 17, 2021
######################################################################
# Purpose: Manage and clean racial equity tools data
######################################################################

#-----input and explore data-----

library(data.table)
jurisdictions <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Jurisdictions_Census_Geography_107_20200813.csv")
# total 107 cities and counties
jurisdictions[Type=='City'] #76 cities
jurisdictions[Type=='County'] #27 counties
jurisdictions[Type!='City'&Type!='County'] #4 not city or county (3 town, 1 district)

#-----manage data-----

# extract important variables (Jurisdiction, State, Census_Type, PolicyLink, GARE, Org_Partner)
RETools <- jurisdictions[,.(Location=Jurisdiction, State, Type, PolicyLink, GARE)]

# change type (Town to City)
RETools[Type=='Town',Type:='City']
RETools[Type=='City'] #79 cities
RETools[Type=='County'] #27 counties
RETools[Type!='City'&Type!='County'] #1 District

#-----manage geodata-----

# import geodata
geodata <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_20201110.csv", 
                 colClasses = c(GEOID = "character"))

# import state codes data
geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_state_20201110.csv", 
                 colClasses = c(STATE_GEOID = "character"))

# change state codes to full state names
setkey(RETools, State)
names(geodata_state)
setkey(geodata_state, STATE_CODE)
RETools[geodata_state, State:=STATE_NAME]

# change special location names
RETools[Location=='Council of the District of Columbia',Location:='District of Columbia']
RETools[Location=='Nashville',Location:='Nashville-Davidson']
RETools[Location=='Nassau (Long Island)',Location:='Nassau']
RETools[Location=='Saint Paul',Location:='St. Paul']

# merge geodata
setkey(geodata,NAME,STATE_NAME)
setkey(RETools,Location,State)
names(geodata)
RETools[geodata,':=' 
        (GEOID=GEOID,
          INTPTLAT=INTPTLAT,
          INTPTLONG=INTPTLONG)]

# check for merging errors
errors <- RETools[is.na(GEOID),]
errors <- tibble::as_tibble(errors)

# change names back
# RETools[Location=='District of Columbia',Location:='Council of the District of Columbia']
RETools[Location=='Nashville-Davidson',Location:='Nashville']
# RETools[Location=='Nassau',Location:='Nassau (Long Island)']
RETools[Location=='St. Paul',Location:='Saint Paul']

#-----standardize variable names-----

# standardize capitalization of variable names
names(RETools) <- toupper(names(RETools))

##-----export data-----

write.csv(RETools, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/re_tools_', 
                        gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)
