dat_list=list(
  N=length(tox_liver$ID),
  y=tox_liver$no..exposed,
  n=tox_liver$sample.size,
  country=as.integer(tox_liver$country),
  species=as.integer(tox_liver$species),
  study=as.integer(tox_liver$ID),
  Nsp=length(unique(tox_liver$species)),
  Nst=length(unique(tox_liver$ID)),
  Nct=length(unique(tox_liver$country)))

#random intercept only
#must add other fixed effects, but what?
#must add generated quantities block
#account for year?

AR_mod=stan(model_code="

data{
  int<lower=0> N; // no.of obs
  int <lower=0> y[N];       // exposed
  int <lower=0>  n[N];       // sample size 
 // real <lower=0, upper=1> proportion[N]; //proportion exposed
  int country [N];// country
  int species[N]; //ID of each species
  int study [N]; //ID of study
  int Nsp; //no.of species
  int Nst; //no.of studies
  int Nct; //no.of country
}

parameters {
  real alpha;// global intercept
  real<lower=0> sigma_sp[Nsp];//errors for random effects
  real<lower=0> sigma_st[Nst];//errors for random effects
  real<lower=0> sigma_ct[Nct];//errors for random effects
  real <lower=0> phi;
  real <lower=0> sp_non;//non-centered error term for species
  real <lower=0> st_non;//non-centered error term for study
  real <lower=0> ct_non;//non-centered error term for country
  real <lower=0, upper=1> pred_exp[N] ;//exposure per observation
}

transformed parameters{
  vector <lower=0, upper=1> [N] prop_mu; //mean estimated exposure 
  vector <lower=0> [N] A;
  vector <lower=0> [N] B;
  vector [Nsp] alpha_sp; //random intercept per species
  vector [Nst] alpha_st;// random intercept per study
  vector [Nct] alpha_ct;// random intercept per country
  //vector [Nfam] mass_fam; //random slope per family for mass effect
  //vector [Nfam] diet_fam;//random slope per family for diet effect
  //vector [Nfam] for_fam;//random slope per family for foraging effect
  
  for (j in 1:Nsp) {
  
  alpha_sp[j]= sp_non*sigma_sp[j];
  }
   for (k in 1:Nst) {
  
  alpha_st[k]= st_non*sigma_st[k];
   }
 
   for (m in 1:Nct) {
  
 alpha_ct[m]= ct_non*sigma_ct[m];
 
  }
  
  //model:
  
  for (i in 1:N){
  
  prop_mu[i]= inv_logit(alpha+alpha_sp[Nsp]+alpha_st[Nst]+alpha_ct[Nct]);
  
  //alternate:
   prop_mu[i]= inv_logit(alpha+alpha_sp[Nsp]+alpha_st[Nst]+mass[i]+diet[i]+latitude[i]);
  
  }
  
  A = prop_mu * phi;
  B = (1 - prop_mu)* phi;
  
  }

 model {
  //priors

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
