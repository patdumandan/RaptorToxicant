#KS-test####

tox_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/FINAL_full_dataset_mass.csv", stringsAsFactors = FALSE)

global_sp_dat=tox_dat%>%select(sci_name, BodyMass.Value)%>%unique()

sp_tox_dat=tox_dat%>%select(sci_name,common_name, toxicant.group, BodyMass.Value)%>%unique()

ar_tox_dat=sp_tox_dat%>%filter(toxicant.group=="anticoagulant rodenticides")
oc_tox_dat=sp_tox_dat%>%filter(toxicant.group=="organochlorine insecticides")
fr_tox_dat=sp_tox_dat%>%filter(toxicant.group=="flame retardants")
pcb_tox_dat=sp_tox_dat%>%filter(toxicant.group=="PCBs")
hm_tox_dat=sp_tox_dat%>%filter(toxicant.group=="heavy metals")

ks.test(ar_tox_dat$BodyMass.Value, global_sp_dat$BodyMass.Value) #D=0.11, p=0.98
ks.test (oc_tox_dat$BodyMass.Value, global_sp_dat$BodyMass.Value)  #D=0.087, p=0.89
ks.test(gfr_tox_dat$BodyMass.Value, global_sp_dat$BodyMass.Value)  #D=0.10, p=0.97
ks.test(pcb_tox_dat$BodyMass.Value, global_sp_dat$BodyMass.Value) #D=0.09, p=0.91
ks.test(hm_tox_dat$BodyMass.Value, global_sp_dat$BodyMass.Value) #D=0.08, p=0.96
