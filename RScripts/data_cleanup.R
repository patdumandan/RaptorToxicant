#data clean up for proportion data as of May 2021####
#missing: Todd, Vince, Ariana, Pat
#reformat orig reference sheet to teams database to add data in there?

#Notes####
#1. for now, I only have records of 112 studies
#2. the inconsistencies in data entry is making it a pain to clean the data
#ex. use of na in the proportions instead of NA. and adding ND. just leave it blank
#ex. adding asterisks next to values converts the entire column to a factor so that's problematic
#but these are fixed, just a general rule

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)

#Read in separate xls sheets####
Christine=read_excel("main_27May21.xlsx", sheet="Christine")

Georgia=read_excel("main_27May21.xlsx", sheet="Georgia")

James=read_excel("main_27May21.xlsx", sheet="James")

Sharon=read_excel("main_27May21.xlsx", sheet="Sharon")

Tara=read_excel("main_27May21.xlsx", sheet="Tara")

Tricia=read_excel("main_27May21.xlsx", sheet="Tricia")

#convert xlsx files to csv so we can manipulate using dplyr####
write.csv(Christine, "christine.csv")
write.csv(Georgia, "georgia.csv")
write.csv(James, "james.csv")
write.csv(Tara, "tara.csv")
write.csv(Sharon, "sharon.csv")
write.csv(Tricia, "tricia.csv")

#read in the files####
christine=read.csv("christine.csv")
georgia=read.csv("georgia.csv")
tara=read.csv("tara.csv")
james=read.csv("james.csv")
sharon=read.csv("sharon.csv")
tricia=read.csv("tricia.csv")


#make sure that the sheets have the same column names####

christine=christine%>%
  filter(!is.na(ID))%>%
  select(-X, -Reference..don.t.copy.,-Notes)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

georgia=georgia%>%
  filter(!is.na(ID))%>%
  select(-X, -Notes)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

james=james%>%
  filter(!is.na(ID))%>%
  select(-X, -Notes)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

tara=tara%>%
  filter(!is.na(ID))%>%
  select(-X,-DELETE__FOR.REFERENCE.ONLY__AUTHOR.LIST_ , -Notes)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

sharon=sharon%>%
  filter(!is.na(ID))%>%
  select(-X, -Notes, -...29,-...30)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

tricia=tricia%>%
  filter(!is.na(ID))%>%
  select(-X, -Notes)%>%
  rename(sci_name= Species..scientific.name., common_name=Species..English.name.,proportion=OR.if.given..proportion.exposed)

#create full dataset####
full_dat=rbind(christine,georgia,james,sharon,tara,tricia)%>%
        arrange(ID)%>%rename(exposed=no..exposed)%>%
        filter(!(sample.size %in%c("1 homogenate (see notes)")),
               !(exposed %in% c("na", "ND")))%>%
        mutate(sample.size=as.numeric(sample.size), exposed=as.integer(exposed),
         proportion=stringr::str_replace(full_dat$proportion, '\\*', '')) %>%
        filter(!(proportion %in%c(">0.5")))

#create subset of data to calculate proportion for those with no.exposed and sample size####
full_dat_1=full_dat%>%
  mutate(proportion=as.numeric(proportion))%>%
  mutate(proportion=exposed/sample.size)

#create subset of data with proportions only
full_dat_2=full_dat_1%>%
  filter(!is.na(proportion))%>%
  mutate(exposed=proportion*sample.size)

#create full dataset for proportion analyses only####
full_dat_3=rbind(full_dat_1,full_dat_2)%>%
  select(ID, Title, study.period,country, common_name,sci_name,subject.type,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, proportion, Age.Class)
#read in trait dataset####
trait_dat=read.csv("raptor_traits.csv")

full_dat_traits=left_join(full_dat_3, trait_dat, by=c("sci_name", "common_name"))%>%
  select(ID, Title, study.period,country, common_name,sci_name,subject.type, Age.Class,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, proportion, BodyMass.Value, Diet.Inv, Diet.Scav)

#data exploration
prop_data=full_dat_traits%>%filter(!is.na(proportion))

tox_sam=prop_data%>%
  group_by(ID,sample.type, toxicant.group)%>%
  summarise(count=n())%>%
  arrange(count)

prop_data=prop_data%>%
  mutate(toxicant.group=recode(toxicant.group, "pesticides"="organochlorine insecticides"))%>%
  filter(!(toxicant.group %in%c("unknown", "0")), !is.na(toxicant.group))%>%
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
