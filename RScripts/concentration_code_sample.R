conc_traits=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/full_concentration_dataset.csv")

#standardize trait data####

conc_traits$mass= scale(conc_traits$BodyMass.Value, center=T, scale=T)
conc_traits$inv= scale(conc_traits$Diet.Inv, center=T, scale=T)
conc_traits$scav= scale(conc_traits$Diet.Scav, center=T, scale=T)


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
