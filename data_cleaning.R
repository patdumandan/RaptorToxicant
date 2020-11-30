#Preliminary Analysis

tox=read.csv("toxicant_clean.csv")
require(dplyr)
tox$proportion=tox$OR.if.given..proportion.exposed
tox$concentration=tox$concentration..ND.for.non.detects.
tox$reference=tox$C

#toxicant-specific analysis

#organochlorine insecticides
length(unique(oc_tox$toxicant.specific)) #47 specific types of OCs
oc_tox=tox%>%
  filter(toxicant.group=="organochlorine insecticides", !is.na(proportion))%>%
  group_by(toxicant.specific)%>%
  summarise(count=n())%>%
  arrange(count)
#issue: there is a blank cell

  select (reference, country, scientific.name, toxicant.group, toxicant.specific, sample.size,
          sample.type, proportion, concentration, count)

head(oc_tox)
