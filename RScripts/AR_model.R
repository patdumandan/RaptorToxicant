#model for AR exposure
#intercept allowed to vary per species, per country, per study, per sample.type

AR_dat=AR_dat%>%filter(toxicant.group=="anticoagulant rodenticides")
AR_dat$spcode=as.integer(as.factor(AR_dat$common_name))
AR_dat$stcode=as.integer(as.factor(AR_dat$Title))
AR_dat$ctcode=as.integer(as.factor(AR_dat$country))
AR_dat$samcode=as.integer(as.factor(AR_dat$sample.type))

dat_list=list( N=length(AR_dat$Title),
               y=AR_dat$exposed,    
               n=AR_dat$sample.size,     
               mass=AR_dat$BodyMass.Value,
               sample=AR_dat$samcode,
               species=AR_dat$spcode,
               country=AR_dat$ctcode,
               study=AR_dat$stcode,
               Nsp=length(unique(AR_dat$spcode)),
               Nst=length(unique(AR_dat$stcode)),
               Nct=length(unique(AR_dat$ctcode)),
               Nsam=length(unique(AR_dat$samcode)))

AR_mod=stan(model_code="

data{
  int<lower=0> N; // no.of observations/rows in dataset
  int <lower=0> y[N];       // exposed
  int <lower=0>  n[N];       // sample size 
  int country [N];// country
  int species[N]; //ID of each species
  int study [N]; //ID of study
  int sample [N]; //ID of sample type
  int Nsp; //no.of species
  int Nst; //no.of studies
  int Nct; //no.of country
  int Nsam;// no.of samples
  real mass[N];
}

parameters {
  real alpha;// global intercept
 
  real<lower=0> sigma_sp[Nsp];//errors for random effects
  real<lower=0> sigma_st[Nst];//errors for random effects
  real<lower=0> sigma_ct[Nct];//errors for random effects
  real<lower=0> sigma_sam[Nsam];//errors for random effects
  
  real <lower=0> phi; //overdispersion param for beta
  
  real <lower=0> sp_non;//non-centered error term for species
  real <lower=0> st_non;//non-centered error term for study
  real <lower=0> ct_non;//non-centered error term for country
  real <lower=0> sam_non;//non-centered error term for country
  
  real <lower=0, upper=1> pred_exp[N] ;//exposure per observation
  real mass_eff;
}

transformed parameters{
  vector <lower=0, upper=1> [N] exp_mu; //mean estimated exposure 
 
  vector <lower=0> [N] A;
  vector <lower=0> [N] B;
 
  vector [Nsp] alpha_sp; //random intercept per species
  vector [Nst] alpha_st;// random intercept per study
  vector [Nct] alpha_ct;// random intercept per country
  vector [Nsam] alpha_sam;// random intercept per country

//for NCP of intercept
  for (j in 1:Nsp) {
  
  alpha_sp[j]= sp_non*sigma_sp[j];
  }

   for (k in 1:Nst) {
  
  alpha_st[k]= st_non*sigma_st[k];
   }
 
   for (m in 1:Nct) {
  
 alpha_ct[m]= ct_non*sigma_ct[m];
   }
 
  for (z in 1:Nsam) {
  
 alpha_sam[z]= ct_non*sigma_sam[z];
 }
  //model:
  
  for (i in 1:N){
  
  exp_mu[i]= inv_logit(alpha+alpha_sp[species[i]]+alpha_st[study[i]]+alpha_ct[country[i]]+
  alpha_sam[sample[i]]+ mass_eff*mass[i]);
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
  sam_non~ normal(0,1);
  sigma_sp~ normal(0,1);
  sigma_st~ normal(0,1);
  sigma_ct~ normal(0,1);
  sigma_sam~ normal(0,1);
   
  phi ~normal(0,1);// maybe set priors?
  
  //model likelihood:
  
  pred_exp ~ beta(A, B); // predicted exposure/proportion, beta dist.
  y~binomial(n, pred_exp); //no.of exposed drawn from binomial dist; based on sample size and reported survival estimate
 
  }
",data=dat_list, chains=4, iter=300, control=list(max_treedepth=12))
