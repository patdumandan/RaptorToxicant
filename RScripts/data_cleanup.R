#data clean up for toxicant exposure data as of June 2021####

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

#Read in separate xls sheets####
#can skip this part and just load full dataset from GitHub repo
Christine=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Christine")

Georgia=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Georgia")

James=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="James")

Sharon=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Sharon")

Tara=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Tara")

Tricia=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Tricia")

Old_dat=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Datasheet_Master_old")

#convert xlsx files to csv so we can manipulate using dplyr####
write.csv(Christine, "christine.csv")
write.csv(Georgia, "georgia.csv")
write.csv(James, "james.csv")
write.csv(Tara, "tara.csv")
write.csv(Sharon, "sharon.csv")
write.csv(Tricia, "tricia.csv")

#old data reorganization####
write.csv(Old_dat, "old_dat.csv")
write.csv(ref, "ref_sheet.csv")

ref=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Reference Sheet")
ref=ref%>%rename(ID=ID...23, Title=`Article Title`)%>%select(ID,Title )
old_dat=read.csv("./data/old_dat.csv", stringsAsFactors = FALSE)
ref_sheet=read.csv("./data/ref_sheet.csv", stringsAsFactors = FALSE)
orig_sheet=left_join(old_dat, ref_sheet, by="ID")%>%
  select(-X.x, -Reference, -Notes, -X.y, -publication.year)

#read in the files####
christine=read.csv("./data/christine.csv", stringsAsFactors = FALSE)
georgia=read.csv("./data/georgia.csv", stringsAsFactors = FALSE)
tara=read.csv("./data/tara.csv", stringsAsFactors = FALSE)
james=read.csv("./data/james.csv", stringsAsFactors = FALSE)
sharon=read.csv("./data/sharon.csv", stringsAsFactors = FALSE)
tricia=read.csv("./data/tricia.csv", stringsAsFactors = FALSE)

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

orig_sheet=orig_sheet%>%
  rename(sci_name=scientific.name, common_name=English.name,proportion=OR.if.given..proportion.exposed)

#create full dataset####
full_dat=rbind(christine,georgia,james,sharon,tara,tricia, orig_sheet)%>%
        arrange(ID)%>%rename(exposed=no..exposed)%>%
        filter(!(sample.size %in%c("1 homogenate (see notes)")),
               !(exposed %in% c("na", "ND")))%>%
        mutate(sample.size=as.numeric(sample.size), exposed=as.integer(exposed),
         proportion=stringr::str_replace(proportion, '\\*', ''),
         toxicant.group=recode(toxicant.group, 
                               "pesticides"="organochlorine insecticides", "PCB"="PCBs", 
                               "metal"="heavy metals")) %>%
        filter(!(proportion %in%c(">0.5")), !(toxicant.group %in% c("metaloid", "non-metal", 
                                                                  "unknown"  ,"0", "PFAS")),
                                              !is.na(toxicant.group))%>%
        arrange(ID)

write.csv(full_dat, "FINAL_full_data.csv")
#Note: ID starts at 89 because first 88 have been excluded during initial review

#FULL DATASET############
full_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/FINAL_full_data.csv", stringsAsFactors = FALSE)

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
         sample.type, sample.size, exposed, proportion, Age.Class)%>%
  mutate(common_name=str_to_title(common_name))
#read in trait dataset####
trait_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/raptor_traits.csv")%>%
  mutate(common_name=str_to_title(common_name))

full_dat_traits=left_join(full_dat_3, trait_dat, by=c("common_name"))%>%
  select(ID, Title, study.period,country, common_name,subject.type, Age.Class,
         toxicant.specific, toxicant.group,
         sample.type, sample.size, exposed, proportion, BodyMass.Value, Diet.Inv, Diet.Scav)

#data exploration####
prop_data=full_dat_traits%>%filter(!is.na(proportion))

#write.csv(prop_data, "full_data_proportion.csv")

tox_sam=prop_data%>%
  group_by(ID,sample.type, toxicant.group)%>%
  summarise(count=n())%>%
  arrange(count)

prop_data=prop_data%>%
  mutate(toxicant.group=recode(toxicant.group, "pesticides"="organochlorine insecticides", "PCB"="PCBs"))%>%
  filter(!(toxicant.group %in%c("unknown", "0")), !is.na(toxicant.group))%>%
  mutate(Age=recode(Age.Class,"adult"="2", "Adult"="2", "SY"="2", "ASY"="2", "HY"="1", "egg(s)"="1", 
                    "juvenile"="1", "unknown"="2", "unknown/mixed"="2"))

write.csv(prop_data, "proportion_data.csv")

#data visualization for proportion####

#proportion~tox.group
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


#proportion~body mass
graph <- ggplot(prop_data, aes(x = BodyMass.Value, y = proportion)) +
  geom_point() +facet_wrap(~toxicant.group)+
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  theme_classic() + stat_smooth(method='lm')+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph

#proportion~Diet.Scav

graph <- ggplot(prop_data, aes(x = Diet.Scav, y = proportion)) +
  geom_point() +facet_wrap(~toxicant.group)+
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  theme_classic() + stat_smooth(method='lm')+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph

#proportion~Diet.Inv
graph <- ggplot(prop_data, aes(x = Diet.Inv, y = proportion)) +
  geom_point() +facet_wrap(~toxicant.group)+
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  theme_classic() + stat_smooth(method='lm')+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph
