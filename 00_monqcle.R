######################################################################
## Title: MonQcle Data
## Project: RWJF
## Author: Charysse Gibson
## Date created: March 9, 2021
######################################################################
# Purpose: Manage and clean MonQcle data
######################################################################

#-----input and explore data-----

library(data.table)
monqcle <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/extracted_monqcle_variables_20210308.csv")
nrow(monqcle) # total 107 cities and counties

#-----explore data-----

library(Hmisc)
str(monqcle)
# summary(monqcle)
# describe(monqcle)

# convert variable names to uppercase
names(monqcle) <- toupper(names(monqcle))

names(monqcle)
# paste(as.character(names(monqcle)), collapse=", ")
view(monqcle)

#-----manage geodata-----

monqcle[1]

# import geodata
geodata <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_20201110.csv", 
                 colClasses = c(GEOID = "character"))

# import state codes data
geodata_state <- fread(file = "C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RPHC/Data/Geodata/geodata_state_20201110.csv", 
                       colClasses = c(STATE_GEOID = "character"))

# change state codes to full state names
setkey(monqcle, STATE)
names(geodata_state)
setkey(geodata_state, STATE_CODE)
monqcle[geodata_state, STATE:=STATE_NAME]

# change special location names
monqcle[LOCATION=='Nashville',LOCATION:='Nashville-Davidson']
monqcle[LOCATION=='Saint Paul',LOCATION:='St. Paul']

# merge geodata
setkey(geodata,NAME,STATE_NAME)
setkey(monqcle,LOCATION,STATE)
names(geodata)
monqcle[geodata,GEOID:=GEOID]

# check for merging errors
monqcle[is.na(GEOID),]

# change names back
monqcle[LOCATION=='Nashville-Davidson',LOCATION:='Nashville']
monqcle[LOCATION=='St. Paul',LOCATION:='Saint Paul']

##-----export data-----

write.csv(monqcle, paste0('C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Final/monqcle_', 
                          gsub('-','',Sys.Date()),'.csv'), row.names=FALSE)
