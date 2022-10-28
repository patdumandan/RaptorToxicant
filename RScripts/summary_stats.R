#geography####
cntry=full_dat%>%
  select(ID, country, toxicant.group)%>%
group_by(country,ID, toxicant.group)%>%
 summarise(std=n_distinct(ID, country, toxicant.group))%>%
  distinct(country,ID)%>%
  mutate(stdy=n())%>%
  distinct(country, stdy)

#country-toxicant
cntry=full_data%>%
  select(ID, country, Continent, toxicant.group)%>%
  group_by(toxicant.group, Continent)%>%
  summarise(Records=n_distinct(Continent, ID, toxicant.group))%>%
  rename(Toxicant=toxicant.group, continent=Continent)%>%
  arrange(Records)%>%na.omit()

#to locate studies with multiple countries
cntry%>%group_by(ID)%>%filter(n()>1)%>%arrange()

#species###
sp=full_data%>%
  group_by(common_name, ID)%>%
  summarise(std=n_distinct(common_name, ID))%>%

  distinct(common_name, ID)%>%
  mutate(stdy=n())%>%
  distinct(common_name,stdy)%>% # distinct(common_name,ID,stdy)%>% if want to locate studies
  arrange(stdy)

#to locate studies with multiple species
repsp=sp%>%group_by(ID)%>%filter(n()>1)%>%arrange(ID)

#toxicant####
tox=full_data%>%
  group_by(toxicant.group, ID)%>%
  summarise(std=n_distinct(toxicant.group, ID))%>%
  distinct(toxicant.group, ID)%>%
  mutate(stdy=n())%>%
  distinct(toxicant.group,ID,stdy)%>% # distinct(common_name,ID,stdy)%>% if want to locate studies
  arrange(stdy)

#to locate studies with multiple toxicants
toxsp=tox%>%group_by(ID)%>%filter(n()>1)%>%arrange(ID)
unique(toxsp$ID)

#toxicant X raptor

arsp=full_data%>%filter(toxicant.group=="anticoagulant rodenticides")%>%
  distinct(ID, common_name)%>%
  group_by(common_name)%>%
  summarise(total=n())%>%
  arrange(total)

frsp=full_dataa%>%filter(toxicant.group=="flame retardants")%>%
  distinct(ID, common_name)%>%
  group_by(common_name)%>%
  summarise(total=n())%>%
  arrange(total)

hmsp=full_data%>%filter(toxicant.group=="heavy metals")%>%
  distinct(ID, common_name)%>%
  group_by(common_name)%>%
  summarise(total=n())%>%
  arrange(total)

pcbsp=full_data%>%filter(toxicant.group=="PCBs")%>%
  distinct(ID, common_name)%>%
  group_by(common_name)%>%
  summarise(total=n())%>%
  arrange(total)

ocsp=full_data%>%filter(toxicant.group=="organochlorine insecticides")%>%
  distinct(ID, common_name)%>%
  group_by(common_name)%>%
  summarise(total=n())%>%
  arrange(total)

#toxicant X sample

tox_sam=full_data%>%
  distinct(ID, common_name, sample.type, toxicant.group)%>%
  group_by(sample.type, toxicant.group)%>%
  mutate(count=n())%>%
  arrange(count)

#toxicant specific

tox_spec=full_data%>%
  distinct(toxicant.specific, toxicant.group)%>%
  arrange(toxicant.group)

write.csv(tox_spec, "toxspec.csv")
