#random intercept only, no other predictors

hg_conc=stan(model_code="
  
  data{

  int<lower=0> N; // no.of obs
  real concentration[N]; //survival estimate
 // real <lower=0> mass[N];// ave.mass in kg
  int species[N]; //ID of each species
  int study [N]; //ID of study
  int Nsp; //no.of species
  int Nst; //no.of studies
  int country [N];  // ID of country
  int Ncoun;
                }
                
  parameters {

  real <lower=0> alpha;// global intercept
  real <lower=0> beta2; //slope region effect
  real<lower=0> sigma_sp;//errors for random effects
  real<lower=0> sigma_st;//errors for random effects
  real<lower=0> sigma_co;//errors for random effects
  real <lower=0> phi; // global variance term
  real <lower=0> sp_non;//non-centered error term for species
  real <lower=0> st_non;//non-centered error term for study
  real <lower=0> co_non;//non-centered error term for family
  
  
              }
   
     
  transformed parameters{
  vector <lower=0, upper=1> [N] conc_mu; //estimated survival 
  vector <lower=0> [N] A;
  vector <lower=0> [N] B;
  vector [Nsp] alpha_sp; //random intercept per species
  vector [Nst] alpha_st; //random intercept per species
  vector [Ncoun] alpha_coun; //random intercept per species
  
  for (i in 1:N){
  
  conc_mu[i]= inv_logit(alpha+alpha_sp[species[i]]+alpha_st[study[i]]+alpha_coun[country[i]]);
  }
  
  A = conc_mu * phi;
  B = (1 - conc_mu )* phi;
  
   for(j in 1:Nsp){
           alpha_sp[j]= sp_non*sigma_sp[j];
  }
  
  for (f in 1: Nst){
  
          alpha_st[f]= st_non*sigma_st[f];
  }
  
  for (v in 1:Ncoun){
        alpha_coun[v]=co_non*sigma_co[v];
  }
  
  }
  
  model {
  //priors
  
  alpha~ normal (0,1);
  beta2~ normal (0,1);
  sigma_sp ~normal(0,1);
  sigma_st~ normal(0,1);
  sigma_co~ normal(0,1);
  phi~ normal(0,1);
  sp_non~ normal(0,1);
  st_non~ normal(0,1);
  co_non~ normal(0,1);


  for (i in 1:N){
  
  concentration[i]~ beta(A, B);
  }

  }
  
  generated quantities {
  
  real log_lik [N];//predictions
  

    log_lik = beta_rng(A, B);
   
  }", data=dat_list, chains=2, iter=100)
