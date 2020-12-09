N=length()

hg_conc=stan(model_code="
  
  data{

  int<lower=0> N; // no.of obs
  real <lower=0, upper=1> concentration[N]; //survival estimate
 // real <lower=0> mass[N];// ave.mass in kg
  int species[N]; //ID of each species
  int study [N]; //ID of study
  int Nsp; //no.of species
  int Nst; //no.of studies
  int country [N];  // ID of country
                }
                
  parameters {

  real <lower=0> alpha;// global intercept
  real <lower=0> alpha_sp[Nsp]; //random intercept per species
  real <lower=0> alpha_st [Nst];// random intercept per study
  real <lower=0> beta2; //slope region effect
  real<lower=0> sigma_sp;//errors for random effects
   real<lower=0> sigma_st;//errors for random effects
  real <lower=0> phi; // global variance term
              }
   
     
  transformed parameters{
  vector <lower=0, upper=1> [N] conc_mu; //estimated survival 
  vector <lower=0> [N] A;
  vector <lower=0> [N] B;
  
  for (i in 1:N){
  
  conc_mu[i]= inv_logit(alpha+beta2*country[i]+alpha_sp[Nsp]+alpha_st[Nst]);
  }
  

  A = surv_mu * phi;
  B = (1 - surv_mu )* phi;
  
  }
  
  model {
  //priors
  
  alpha~ normal (0,1);
  beta1~ normal (0,1);
  beta2~ normal (0,1);
  sigma_sp ~normal(0,1);
  sigma_st~ normal(0,1);
  phi~ normal(0,1);


  
  for (i in 1:N){
  
  survival[i]~ beta(A, B);

  }

  for(j in 1:Nsp){
           alpha_sp[j]~normal(0, sigma_sp);
  }
  
  for (f in 1: Nst){
  
          alpha_st[f]~normal(0, sigma_st);
  }
  }
  generated quantities {
  
  real log_lik [N];//predictions
  

    log_lik = beta_rng(A, B);
   
  }", data=list(N=N, survival=survival, mass=mass,death_type=death_type,
                species=species,study=surv$stcode, Nst=77, Nsp=40), chains=2, iter=100)