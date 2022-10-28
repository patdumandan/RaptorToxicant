#data clean up for toxicant exposure data as of June 2021####
#Note: all steps from lines 4 to 122 are not needed if you just want to access the cleaned data
#line 131 is what you need to run to get the "clean" full dataset used for the analyses
#line 170 to get "clean" proportion data with traits
#line 184 to get "clean" concentration data with traits

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

#######can skip this part (lines 12-117) and just load full dataset from GitHub repo

#Read in separate xls sheets####

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

#read in the files####
christine=read.csv("./data/christine.csv", stringsAsFactors = FALSE)
georgia=read.csv("./data/georgia.csv", stringsAsFactors = FALSE)
tara=read.csv("./data/tara.csv", stringsAsFactors = FALSE)
james=read.csv("./data/james.csv", stringsAsFactors = FALSE)
sharon=read.csv("./data/sharon.csv", stringsAsFactors = FALSE)
tricia=read.csv("./data/tricia.csv", stringsAsFactors = FALSE)

####removed this because of issues in titles being different

#old data reorganization####

#ref=read_excel("./data/FINAL_datsheet_25Jun21.xlsx", sheet="Reference Sheet")
#ref=ref%>%rename(ID=ID...23, Title=`Article Title`)%>%select(ID,Title )
#old_dat=read.csv("./data/old_dat.csv", stringsAsFactors = FALSE)
ref_sheet=read.csv("./data/ref_sheet.csv", stringsAsFactors = FALSE)
#orig_sheet=left_join(old_dat, ref_sheet, by="ID")%>%
#  select(-X.x, -Reference, -Notes, -X.y, -publication.year)

#write.csv(Old_dat, "old_dat.csv")
#write.csv(ref, "ref_sheet.csv")


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

#orig_sheet=orig_sheet%>%
#  rename(sci_name=scientific.name, common_name=English.name,proportion=OR.if.given..proportion.exposed)

#create full dataset####
full_dat=rbind(christine,georgia,james,sharon,tara,tricia)%>%
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
       distinct_all()%>%
  arrange(ID)

full_dat=full_dat%>%separate(study.period, c("start", "end"),extra = "drop", fill = "right")
full_dat=as.data.frame(full_dat)
full_dat=right_join(ref_sheet, full_data, by="ID")%>%select(-X.x, -X.y, -Title.y)%>%
  rename("Title"="Title.x")
#Note: Need to manually check the sci.and common names so they would match
write.csv(full_dat, "FINAL_full_data.csv", row.names = F)
 

#Note: ID starts at 89 because first 88 have been excluded during initial review

#FULL DATASET############
full_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/FINAL_full_data.csv")

full_dat=full_dat%>%
  mutate(concentration=as.numeric(concentration..ND.for.non.detects.),
       spcode=as.integer(as.factor(common_name)),
       ctcode=as.integer(as.factor(country)))%>%
  select(-concentration..ND.for.non.detects.)

write.csv(full_dat, "full_dataset.csv")

full_data=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/full_dataset.csv")

#TRAIT DATASET####
trait_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/raptor_traits.csv")


#PROPORTION DATASET####

#create subset of data to calculate proportion for those with no.exposed and sample size

full_dat_1=full_data%>%
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
