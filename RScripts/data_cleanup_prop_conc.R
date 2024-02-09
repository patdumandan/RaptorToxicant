#PROPORTION DATASET####

#create subset of data to calculate proportion for those with no.exposed and sample size

full_dat_1=full_dat%>%
  mutate(proportion=as.numeric(proportion))%>%
  mutate(proportion=exposed/sample.size)

#create subset of data with proportions only
full_dat_2=full_dat_1%>%
  filter(!is.na(proportion))%>%
  mutate(exposed=proportion*sample.size)

#create full dataset for proportion analyses only####
full_dat_3=rbind(full_dat_1,full_dat_2)%>%
  select(ID, Title, country, common_name,sci_name,subject.type,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, proportion, Age.Class,concentration,
         unit.of.measure)

full_dat_4=left_join(full_dat_3, trait_dat, by="common_name")%>%
  select(ID,Title, country, BLFamilyLatin, common_name,subject.type, Age.Class,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, proportion, BodyMass.Value, Diet.Inv, Diet.Scav,
         concentration, unit.of.measure)%>%
  distinct()

prop_data=full_dat_4%>%filter(!is.na(proportion))

prop_data=left_join(prop_data, cnt, by="country")

write.csv(prop_data, "full_proportion_dataset.csv")

prop_data=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/full_proportion_dataset.csv")

#CONCENTRATION DATASET####

conc_traits=left_join(full_data, trait_dat, by=c("common_name"))%>%
  select(ID,Title, country, BLFamilyLatin, common_name,subject.type, Age.Class,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, BodyMass.Value, Diet.Inv, Diet.Scav,
         concentration,concentration.level.estimate.type, Detection.limit..if.reported.,
         unit.of.measure, sublethal.effects.assessed., sublethal.specific,sublethal.type)

write.csv(conc_traits, "full_concentration_dataset.csv")

conc_traits=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/full_concentration_dataset.csv")
