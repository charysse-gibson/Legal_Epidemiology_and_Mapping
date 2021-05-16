######################################################################
## Title: criteria_flowchart_translated                                   
## Author: Charysse Gibson
## Date created: April 16, 2019
######################################################################
# Purpose: Create eligibility criteria flowchart
######################################################################

##-----Find GARE and PolicyLink Numbers-----

library(data.table)
allcontacts <- fread("C:/Users/gibsonck/Google Drive/Institute for Healing Justice and Equity/RWJF/Data/Jurisdictions_Census_Geography_107_20200813.csv")

# all contacts (n=107)
nrow(allcontacts[GARE==1]) #95
nrow(allcontacts[PolicyLink==1]) #30
nrow(allcontacts[GARE==1&PolicyLink==1]) #19
95+30-18 #107
nrow(allcontacts[GARE==1&PolicyLink==0]) #77 Only GARE
nrow(allcontacts[GARE==0&PolicyLink==1]) #11 Only PolicyLink
77+12+18 #107

#-----107 Locations Flowchart-----

library(DiagrammeR)

# Create subsets
GARE_referenced_partners <- 170
PL_referenced_partners <- 33
referenced_partners <- GARE_referenced_partners+PL_referenced_partners
unique_jurisdictions <- 146
exclude_levels <- referenced_partners-unique_jurisdictions
contact_info <- 112
exclude_nocontact <- unique_jurisdictions-contact_info
cities_counties <- 107
exclude_deptdup <- contact_info-cities_counties
direct_contact <- 87
exclude_nodirectcontact <- cities_counties-direct_contact

# Define data
data <- c(a=GARE_referenced_partners,
          b=PL_referenced_partners,
          c=referenced_partners, 
          d=exclude_levels, 
          e=unique_jurisdictions, 
          f=exclude_nocontact, 
          g=contact_info,
          h=exclude_deptdup,
          i=cities_counties)

# Generate graphic
DiagrammeR::grViz("
                  digraph graph2 {
                  
                  graph [layout = dot]
                  
                  # node definitions with substituted label text
                  node [shape = rectangle, fontname=Calibri, fontsize=24, fillcolor = Biege, width=3]
                  a [label = '@@1']
                  b [label = '@@2']
                  c [label = '@@3']
                  d [label = '@@4']
                  e [label = '@@5']
                  f [label = '@@6']
                  g [label = '@@7']
                  h [label = '@@8']
                  i [label = '@@9']
                  blank1[label = '', width = 0.01, height = 0.01]
                  blank2[label = '', width = 0.01, height = 0.01]
                  blank3[label = '', width = 0.01, height = 0.01]
                  blank4[label = '', width = 0.01, height = 0.01]
                  
                  a -> c;
                  b -> c;
                  c -> blank1 [arrowhead = none];
                  blank1 -> e;
                  e -> blank2 [arrowhead = none];
                  blank2 -> g;
                  g -> blank3 [arrowhead = none];
                  blank3 -> i;
                  {rank=same ; blank1 -> d};
                  {rank=same ; blank2 -> f};
                  {rank=same ; blank3 -> h};
                  }
                  
                  [1]: paste0('', data[1], ' Jurisdictions \\nreferenced on GARE website')
                  [2]: paste0('', data[2], ' Jurisdictions \\nreferenced on PolicyLink website')
                  [3]: paste0('', data[3], ' referenced jurisdictions')
                  [4]: paste0('Exclude ', data[4], ' GARE references not with \\nlocal government jurisdictions \\n (includes states, agencies, provinces, ports)')
                  [5]: paste0('', data[5], ' Unique jurisdictions at city, county, town, village,\\n department, district, and housing authority levels; \\n GARE only = 113, PolicyLink only = 14, Both = 19')
                  [6]: paste0('Exclude ',data[6],' Jursidictions where unable to find local \\n contact via web search of GARE and PolicyLink \\n jurisdictions and general web search')
                  [7]: paste0('', data[7], ' Jurisdictions  with at least one contact; \\n GARE only = 82, PolicyLink only = 11, Both = 19')
                  [8]: paste0('', data[8], ' Duplicates removed (department level jurisdictions \\n converted to corresponding city or county level)')
                  [9]: paste0('', data[9], ' Local jursidictions (cities and counties); \\n GARE only = 77, PolicyLink only = 11, Both = 19')
                  ")

#-----Survey Flowchart-----

# Create subsets
cities_counties <- 107
direct_contact <- 87
exclude_nodirectcontact <- cities_counties-direct_contact
survey_response <- 27
exclude_nosurveyresponse <- direct_contact-survey_response

# Define data
data <- c(a=cities_counties,
          b=exclude_nodirectcontact,
          c=direct_contact, 
          d=exclude_nosurveyresponse, 
          e=survey_response)

# Generate graphic
DiagrammeR::grViz("
                  digraph graph2 {
                  
                  graph [layout = dot]
                  
                  # node definitions with substituted label text
                  node [shape = rectangle, fontname=Calibri, fontsize=24, fillcolor = Biege, width=3]
                  a [label = '@@1']
                  b [label = '@@2']
                  c [label = '@@3']
                  d [label = '@@4']
                  e [label = '@@5']
                  blank1[label = '', width = 0.01, height = 0.01]
                  blank2[label = '', width = 0.01, height = 0.01]
                  
                  a -> blank1 [arrowhead = none];
                  blank1 -> c;
                  c -> blank2 [arrowhead = none];
                  blank2 -> e;
                  {rank=same ; blank1 -> b};
                  {rank=same ; blank2 -> d};
                  }
                  
                  [1]: paste0('', data[1], ' Local jursidictions (cities and counties); \\n GARE only = 77, PolicyLink only = 11, Both = 19')
                  [2]: paste0('', data[2],' Jurisdictions without personalized \\nemail contact information')
                  [3]: paste0('', data[3], ' Jursidictions with personalized email contact information; \\n GARE only = 60, PolicyLink only = 9, Both = 18')
                  [4]: paste0('', data[4],' Jurisdictions that did not complete \\n racial equity tools use survey')
                  [5]: paste0('', data[5],' Jurisdictions completed racial equity tools use survey')
                  ")

#-----Survey & Case studies Flowchart-----

# Create subsets
cities_counties <- 107
direct_contact <- 87
exclude_nodirectcontact <- cities_counties-direct_contact
survey_response <- 27
exclude_nosurveyresponse <- direct_contact-survey_response
case_study <- 3
exclude_nocasestudy <- survey_response-case_study

# Define data
data <- c(a=cities_counties,
          b=exclude_nodirectcontact,
          c=direct_contact, 
          d=exclude_nosurveyresponse, 
          e=survey_response, 
          f=exclude_nocasestudy, 
          g=case_study)

# Generate graphic
DiagrammeR::grViz("
                  digraph graph2 {
                  
                  graph [layout = dot]
                  
                  # node definitions with substituted label text
                  node [shape = rectangle, fontname=Calibri, fontsize=24, fillcolor = Biege, width=3]
                  a [label = '@@1']
                  b [label = '@@2']
                  c [label = '@@3']
                  d [label = '@@4']
                  e [label = '@@5']
                  f [label = '@@6']
                  g [label = '@@7']
                  blank1[label = '', width = 0.01, height = 0.01]
                  blank2[label = '', width = 0.01, height = 0.01]
                  blank3[label = '', width = 0.01, height = 0.01]
                  
                  a -> blank1 [arrowhead = none];
                  blank1 -> c;
                  c -> blank2 [arrowhead = none];
                  blank2 -> e;
                  e -> blank3 [arrowhead = none];
                  blank3 -> g;
                  {rank=same ; blank1 -> b};
                  {rank=same ; blank2 -> d};
                  {rank=same ; blank3 -> f};
                  }
                  
                  [1]: paste0('', data[1], ' Local jursidictions (cities and counties); \\n GARE only = 77, PolicyLink only = 11, Both = 19')
                  [2]: paste0('', data[2],' Jurisdictions without personalized \\nemail contact information')
                  [3]: paste0('', data[3], ' Jursidictions with personalized email contact information; \\n GARE only = 60, PolicyLink only = 9, Both = 18')
                  [4]: paste0('', data[4],' Jurisdictions that did not complete \\n racial equity tools use survey')
                  [5]: paste0('', data[5],' Jurisdictions completed racial equity tools use survey')
                  [6]: paste0('', data[6],' Jurisdictions not interviewed for case studies')
                  [7]: paste0('', data[7], ' Case study jurisdictions interviewed')
                  ")
