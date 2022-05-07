full_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/FINAL_full_data.csv")
trait_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/raptor_traits.csv")

conc_traits=left_join(full_dat, trait_dat, by=c("common_name"))%>%
  select(ID,Title, country, BLFamilyLatin, common_name,subject.type, Age.Class,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, BodyMass.Value, Diet.Inv, Diet.Scav,
         concentration..ND.for.non.detects., unit.of.measure)%>%
  mutate(concentration=as.numeric(concentration..ND.for.non.detects.),
         spcode=as.integer(as.factor(common_name)),
         ctcode=as.integer(as.factor(country)))%>%
  select(-concentration..ND.for.non.detects.)

str(conc_traits)

#24 studies if we use only ng/g data

ng_dat=conc_traits%>%group_by(unit.of.measure)%>%mutate(total=n())%>%
  filter(total==1128)%>%select(-total)

plot(ng_dat$concentration~ng_dat$BodyMass.Value)
plot(ng_dat$concentration~ng_dat$Diet.Scav)
plot(ng_dat$concentration~ng_dat$Diet.Inv)
#smaller species, higher concentration
#less invertebrate diet, less concentration
#less scavenging, less concentration

require(lme4)
summary(glm(concentration~BodyMass.Value+Diet.Inv+Diet.Scav, data=ng_dat, family=Gamma(link = "inverse")))
#no statistically significant relationship between traits and concentration

#standardize values to ng/g?

require(brms)

m1=brm(concentration~mass+inv+scav+(1|spcode)+(1|ctcode)+(1|ID), data=conc_traits, chains=3, iter=1000)
#no statistically significant relationship between traits and concentration
#large Rhat 1.67, need to run for longer
summary(m1)
