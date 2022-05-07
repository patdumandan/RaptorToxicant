#my attempt to model the concentration data####
#NEED TO RECHECK MODEL SPECS

#Analysis Plan:

# build a Gamma/Gaussian model to assess how the detected level of exposure (i.e.,concentration)
# varies per species? trait? year? (IDK)

#NEED TO THINK ABOUT####

#1. do we want to do it this way or just do descriptive statistics on the concentration data?
#(e.g., what is the most common specific type of flame retardant? which species are most studied
#for exposure to each toxicant type, what is the range of the concentration? what types of samples are used?)

#2. how do we want to deal with differences in the reporting of concentrations (correction factors)--

#3. do we want to use Stan for this or we could just use packages if we really model concentration
#(something like Vince did with his eagle work?)


#read data####

conc_dat=read.csv("https://raw.githubusercontent.com/patdumandan/RaptorToxicant/main/data/full_data.csv")

#sample model code for mercury concentration####
#random intercept only, no other predictors
dat_list=list(
  N=length(conc_traits$ID),
  mass=conc_traits$BodyMass.Value,
  species=conc_traits$spcode,
  Nsp=length(unique(conc_traits$spcode)),
  concentration=conc_traits$concentration
)

hg_conc=stan(model_code="
  
  data{

  int<lower=0> N; // no.of obs
  real concentration[N]; //
 // real <lower=0> mass[N];// ave.mass in kg
  int species[N]; //ID of each species
 // int study [N]; //ID of study
  int Nsp; //no.of species
 // int Nst; //no.of studies
  //int country [N];  // ID of country
  //int Ncoun;
                }
                
  parameters {

  real <lower=0> alpha;// global intercept
  real <lower=0> beta2; //slope region effect
  real<lower=0> sigma_sp;//errors for random effects
  //real<lower=0> sigma_st;//errors for random effects
  //real<lower=0> sigma_co;//errors for random effects
  real <lower=0> phi; // global variance term
  real <lower=0> sp_non;//non-centered error term for species
  //real <lower=0> st_non;//non-centered error term for study
  //real <lower=0> co_non;//non-centered error term for family
  
  
              }
   
     
  transformed parameters{
  vector <lower=0, upper=1> [N] conc_mu; //estimated survival 
  vector [Nsp] alpha_sp; //random intercept per species
  //vector [Nst] alpha_st; //random intercept per species
  //vector [Ncoun] alpha_coun; //random intercept per species
  
  for (i in 1:N){
  
  conc_mu[i]= alpha+alpha_sp[Nsp[i]];
  }
  
   for(j in 1:Nsp){
           alpha_sp[j]= sp_non*sigma_sp;
  }
  
 // for (f in 1: Nst){
  
   //       alpha_st[f]= st_non*sigma_st[f];
  //}
  
  //for (v in 1:Ncoun){
    //    alpha_coun[v]=co_non*sigma_co[v];
  //}
  
  }
  
  model {
  //priors
  
  alpha~ normal (0,1);
  beta2~ normal (0,1);
  sigma_sp ~normal(0,1);
  //sigma_st~ normal(0,1);
  //sigma_co~ normal(0,1);
  
  phi~ normal(0,1);//global variance
  
  sp_non~ normal(0,1);
  //st_non~ normal(0,1);
  //co_non~ normal(0,1);
  
  concentration ~ normal (conc_mu, phi);
  }
  
 // generated quantities {
  
  //vector [N] log_lik;//for LOOIC: log-likelihood
  //real <lower=0> y_mu_pred [N]; //predicted concentration
  
  //for (n in 1:N){
  //log_lik[n] = normal_lpmf(concentration [n]| conc_mu[n], phi);
  //y_mu_pred= normal_rng (conc_mu[n], phi);
  //}
  
  
  ", data=dat_list, chains=2, iter=100)
