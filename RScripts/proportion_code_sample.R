prop_data=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/full_proportion_dataset.csv")
#data exploration####

tox_sam=full_data%>%
  distinct(ID, common_name, sample.type, toxicant.group)%>%
  group_by(sample.type, toxicant.group)%>%
  summarise(count=n())%>%
  arrange(count)

prop_data=prop_data%>%
  mutate(toxicant.group=recode(toxicant.group, "pesticides"="organochlorine insecticides", "PCB"="PCBs"))%>%
  filter(!(toxicant.group %in%c("unknown", "0")), !is.na(toxicant.group))%>%
  mutate(Age=recode(Age.Class,"adult"="2", "Adult"="2", "SY"="2", "ASY"="2", "HY"="1", "egg(s)"="1", 
                    "juvenile"="1", "unknown"="2", "unknown/mixed"="2"))

write.csv(prop_data, "proportion_data.csv")



AR_dat=prop_data%>%filter(toxicant.group=="anticoagulant rodenticides")
OC_dat=prop_data%>%filter(toxicant.group=="organochlorine insecticides")
MET_dat=prop_data%>%filter(toxicant.group=="heavy metals")
FR_dat=prop_data%>%filter(toxicant.group=="flame retardants")
PCB_dat=prop_data%>%filter(toxicant.group=="PCBs")

prop_data$mass= scale(prop_data$BodyMass.Value, center=T, scale=T)
prop_data$inv= scale(prop_data$Diet.Inv, center=T, scale=T)
prop_data$scav= scale(prop_data$Diet.Scav, center=T, scale=T)

#34 studies if we only use proportions
#sample: OC pesticides in liver
require(brms)
m2=brm(exposed | trials(sample.size)~ mass+inv+scav+(1|species), data=prop_data, family=binomial, chains=3, iter=100)
#largest rhat=1.13
# need to increase max_treedepth > 10
summary(m2)

dat_list=list(
  N=length(OC_liver$ID),
  y=OC_liver$exposed,
  n=OC_liver$sample.size,
  country=OC_liver$Country,
  species=OC_liver$species,
  study=OC_liver$study,
  Nsp=length(unique(OC_liver$species)),
  Nst=length(unique(OC_liver$study)),
  Nct=length(unique(OC_liver$Country)))

#random intercept only to vary per species
#must add other fixed effects, but what?
#must add generated quantities block for posterior predictive checks (log-lik)
#account for year?
#BROKEN. UGH. NEED TO FIX THIS

OC_mod=stan(model_code="

data{
  int<lower=0> N; // no.of observations/rows in dataset
  int <lower=0> y[N];       // exposed
  int <lower=0>  n[N];       // sample size 
  int country [N];// country
  int species[N]; //ID of each species
  int study [N]; //ID of study
  int Nsp; //no.of species
  int Nst; //no.of studies
  int Nct; //no.of country
}

parameters {
  real alpha;// global intercept
 
  real<lower=0> sigma_sp;//errors for random effects
  real<lower=0> sigma_st;//errors for random effects
  real<lower=0> sigma_ct;//errors for random effects
  
  real <lower=0> phi; //overdispersion param for beta
  
  real <lower=0> sp_non;//non-centered error term for species
  real <lower=0> st_non;//non-centered error term for study
  real <lower=0> ct_non;//non-centered error term for country
  
  real <lower=0, upper=1> pred_exp[N] ;//exposure per observation
}

transformed parameters{
  vector <lower=0, upper=1> [N] exp_mu; //mean estimated exposure 
 
  vector <lower=0> [N] A;
  vector <lower=0> [N] B;
 
  vector [Nsp] alpha_sp; //random intercept per species
 // vector [Nst] alpha_st;// random intercept per study
//  vector [Nct] alpha_ct;// random intercept per country

//for NCP of intercept
  for (j in 1:Nsp) {
  
  alpha_sp[j]= sp_non*sigma_sp[j];
  }

  // for (k in 1:Nst) {
  
  //alpha_st[k]= st_non*sigma_st;
   //}
 
   //for (m in 1:Nct) {
  
 //alpha_ct[m]= ct_non*sigma_ct;
 //}
  
  //model:
  
  for (i in 1:N){
  
  exp_mu[i]= inv_logit(alpha+alpha_sp[species[i]]);
  }
  
  A = exp_mu * phi;
  B = (1 - exp_mu)* phi;
  
  }

 model {
  //priors
  alpha~normal(0,1);
  sp_non~ normal(0,1);
  st_non~ normal(0,1);
  ct_non~ normal(0,1);
  sigma_sp~ normal(0,1);
  sigma_st~ normal(0,1);
  sigma_ct~ normal(0,1);
  
  phi ~normal(0,1);// maybe set priors?
  
  //model likelihood:
  
  pred_exp ~ beta(A, B); // predicted exposure/proportion, beta dist.
  y~binomial(n, pred_exp); //no.of exposed drawn from binomial dist; based on sample size and reported survival estimate
 
  }
",data=dat_list, chains=4, iter=300)

#data visualization####
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

#proportion~phylogeny

graph <- ggplot(prop_data, aes(x = BLFamilyLatin, y = proportion)) +
  geom_point() +facet_wrap(~toxicant.group)+
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black')
graph
