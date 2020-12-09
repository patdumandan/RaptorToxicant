#Preliminary Analysis

tox=read.csv("prax_sheet.csv")
str(tox)
require(dplyr)
tox$reference=tox$Reference..don.t.copy.
tox$proportion=tox$OR.if.given..proportion.exposed
tox$concentration=tox$concentration..ND.for.non.detects.


#toxicant-specific analysis
#Q1: p,p?-DDT and p,p?-DDE, are they same as dieldrin and DDE?

#Analysis Plan:

#Step 1: identify the top identified specific toxicant per main toxicant group

####heavy metals
metal=c("metal", "heavy metals")

met_tox=tox%>%
  filter(toxicant.group %in% metal)%>%
  group_by(toxicant.specific)%>%
  summarise(count=n())%>%
  arrange(count)
length(unique(met_tox$toxicant.specific)) #35 specific types of heavy metals


#Step 2: standardize values across all studies identifying/citing specific toxicant
mercury_conc=tox%>%
  filter(toxicant.specific=="Hg", !is.na(concentration))
length(unique(mercury$Reference..don.t.copy.)) #3 studies
#all seem to be in same unit of measure

# Step 3: create mixed effects model for each top specific toxicant of each main tox group
