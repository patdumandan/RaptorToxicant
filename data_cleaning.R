#Preliminary Analysis
require(dplyr)
require(rstan)
require(rstanarm)

options(mc.cores = parallel::detectCores())

tox=read.csv("prax_sheet.csv")
str(tox)

tox_conc=tox%>%
rename(proportion=OR.if.given..proportion.exposed,concentration=concentration..ND.for.non.detects. )%>%
filter(!is.na(concentration))%>%
arrange(ID)

unique(tox_conc$toxicant.group)

#what are PFAs, Os, unknowns?


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
str(mercury_conc)

#all seem to be in same unit of measure

# Step 3: create mixed effects model for each top specific toxicant of each main tox group
#see concentration stan code script
dat_list=list(N=length(mercury_conc$concentration), concentration=as.numeric(mercury_conc$concentration), 
              species=as.integer(mercury_conc$Species..scientific.name.), 
              study=as.integer(mercury_conc$reference),
              country=as.integer(mercury_conc$country),
              Nsp=length(unique(mercury_conc$Species..scientific.name.)),
              Nst=length(unique(mercury_conc$reference)),
              Ncoun=length(unique(mercury_conc$country)))
