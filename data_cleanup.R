library("readxl")
#Read in separate xls sheets####
Christine=read_excel("Ref_2021.xlsx", sheet="Christine")
write.csv(Christine, "christine.csv")

Georgia=read_excel("Ref_2021.xlsx", sheet="Georgia")
write.csv(Georgia, "georgia.csv")

James=read_excel("Ref_2021.xlsx", sheet="James")
write.csv(Christine, "james.csv")

Sharon=read_excel("Ref_2021.xlsx", sheet="Sharon")
write.csv(Sharon, "sharon.csv")

Tara=read_excel("Ref_2021.xlsx", sheet="Tara")
write.csv(Tara, "tara.csv")

Tricia=read_excel("Ref_2021.xlsx", sheet="Tricia")
write.csv(Tricia, "tricia.csv")

christine=read.csv("christine.csv")
georgia=read.csv("georgia.csv")
tara=read.csv("tara.csv")
james=read.csv("james.csv")
sharon=read.csv("sharon.csv")
tricia=read.csv("tricia.csv")

#missing: Todd, Vince, Ariana, Pat
#reformat orig reference sheet to teams database to add data in there?

#make sure that the sheets have the same column names####

christine=christine%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

georgia=georgia%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

james=james%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

tara=tara%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

sharon=sharon%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

tricia=tricia%>%
  filter(!is.na(ID))%>%
  select(ID, Title, study.period,  country, Species..English.name., subject.type,toxicant.specific,toxicant.group,sample.type,
         sample.size, no..exposed, OR.if.given..proportion.exposed, Age.Class, Sex)

#create full dataset####

full_dat=rbind(christine,georgia,james,sharon,tara,tricia)%>%
        arrange(ID)%>%
  filter(!is.na(no..exposed) | !is.na(OR.if.given..proportion.exposed))

write.csv(full_dat, "full_data.csv")

full_dat=read.csv("full_data.csv")

#create subset of data to calculate proportion for those with no.exposed and sample size####
full_dat_1=full_dat%>%
  filter(!is.na(no..exposed))%>%
  mutate(proportion=no..exposed/sample.size)

#create subset of data with proportions only
full_dat_2=full_dat%>%
  filter(!is.na(OR.if.given..proportion.exposed))%>%
  mutate(proportion=OR.if.given..proportion.exposed, no..exposed=proportion*sample.size)

#create full dataset
full_dat_3=rbind(full_dat_1,full_dat_2)%>%
  mutate(exposed=no..exposed)%>%
  select(ID, Title, study.period, country, Species..English.name., subject.type,
         toxicant.specific, toxicant.group, sample.type,sample.size,exposed,proportion,
         Age.Class,Sex)
write.csv(full_dat_3, "clean_prop_data.csv")

prop_data=read.csv("clean_prop_data.csv")

#data exploration

tox_sam=prop_data%>%
  group_by(sample.type, toxicant.group)%>%
  summarise(count=n())%>%
  arrange(count)
#blood plasma commonest for OCs
#eggs for FRs
#liver for PCBs and ARs
#tail feathers for heavy metals

prop_data=prop_data%>%
  mutate(toxicant.group=recode(toxicant.group, "pesticides"="organochlorine insecticides"))%>%
  filter(!(toxicant.group=="unknown"))%>%
  mutate(Age=recode(Age.Class,"adult"="2", "Adult"="2", "SY"="2", "ASY"="2", "HY"="1", "egg(s)"="1", 
                    "juvenile"="1", "unknown"="2", "unknown/mixed"="2"))

graph <- ggplot(prop_data, aes(x = toxicant.group, y = proportion)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph
=