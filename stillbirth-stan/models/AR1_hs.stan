data {

  int <lower=0> totalObs_SBR; //number of observations
  int <lower=0> totalIndex_covar; //number of country
  int <lower=0> totalRegion; //number of region
  int <lower=0> yearLength; //number of est year

  int estyears[yearLength]; //vector of est years  
  real y_i[totalObs_SBR]; //vector of obs
  int <lower=0,upper=totalIndex_covar> getc_i[totalObs_SBR]; // country for given obs
  int <lower=0,upper=totalRegion> getr_c[totalIndex_covar];  // vector of region given country
  int <lower=0,upper=yearLength> gett_i[totalObs_SBR];       // time for given obs   
  int <lower=0,upper=5> getj_i[totalObs_SBR];                // source type for given obs

  int <lower=0,upper=1> dummy_datatype2_i[totalObs_SBR];
  int <lower=0,upper=1> dummy_datatype3_i[totalObs_SBR];
  int <lower=0,upper=1> dummy_datatype4_i[totalObs_SBR];
  int <lower=0,upper=1> dummy_datatype5_i[totalObs_SBR];
  
  matrix[totalIndex_covar,yearLength] edu_matrix;
  matrix[totalIndex_covar,yearLength] gni_matrix;
  matrix[totalIndex_covar,yearLength] lbw_matrix;
  matrix[totalIndex_covar,yearLength] nmr_matrix;
  matrix[totalIndex_covar,yearLength] anc_matrix;
}

parameters {
  
  real beta_edu_tilde;
  real beta_gni_tilde;
  real beta_lbw_tilde;
  real beta_nmr_tilde;
  real beta_anc_tilde;
  
  real<lower=0> lambda_edu;
  real<lower=0> lambda_gni;
  real<lower=0> lambda_lbw;
  real<lower=0> lambda_nmr;
  real<lower=0> lambda_anc;
  
  real<lower=0> tau_tilde;
  //deviance part
  real beta_dt2_tilde;
  real beta_dt3_tilde;
  real beta_dt4_tilde;
  real beta_dt5_tilde;
  
  
  real<lower=0,upper=20> sigma_j[5];

  real<lower=0> sigma_r;
  real<lower=0> sigma_c;
  real<lower=0> sigma;
  
  real beta_w;

  vector[totalRegion] eps_r;
  vector[totalIndex_covar] eps_c;
  

//AR1
  real<lower=-1,upper=1> rho;
  matrix[totalIndex_covar,yearLength] epsilon_star;
  real<lower=0> sigma_ar;
  }
  

transformed parameters {

  real beta_edu = beta_edu_tilde * lambda_edu * sigma *tau_tilde;
  real beta_gni = beta_gni_tilde * lambda_gni * sigma *tau_tilde;
  real beta_lbw = beta_lbw_tilde * lambda_lbw * sigma *tau_tilde;
  real beta_nmr = beta_nmr_tilde * lambda_nmr * sigma *tau_tilde;
  real beta_anc = beta_anc_tilde * lambda_anc * sigma *tau_tilde;
  
   matrix[totalIndex_covar,yearLength] mu_ct;
   real z_i[totalObs_SBR];
   matrix[totalIndex_covar,yearLength] delta_ct;
     
   vector[totalRegion] beta_r; 
   vector[totalIndex_covar] beta_c;
//intercept   
   
   for(r in 1:totalRegion){
    beta_r[r]= beta_w + sigma_r* eps_r[r];
   }
   
  for(c in 1:totalIndex_covar){
   beta_c[c] = beta_r[getr_c[c]] + sigma_c* eps_c[c];
   }
//mu_ct
  for(c in 1:totalIndex_covar){
    for(t in 1:yearLength){
  mu_ct[c,t] = beta_c[c] +
            beta_edu*edu_matrix[c,t] +
            beta_gni*gni_matrix[c,t] +
            beta_lbw*lbw_matrix[c,t] +
            beta_nmr*nmr_matrix[c,t] +
            beta_anc*anc_matrix[c,t];
  }}
//bias
    for(i in 1:totalObs_SBR){
    z_i[i] = beta_dt2_tilde*dummy_datatype2_i[i]+
             beta_dt3_tilde*dummy_datatype3_i[i]+
             beta_dt4_tilde*dummy_datatype4_i[i]+
             beta_dt5_tilde*dummy_datatype5_i[i];
           
  }
    delta_ct[,1] = (sigma_ar^2)/(1-rho^2)*epsilon_star[,1];
  for(t in 2:yearLength){
    delta_ct[,t]=rho * delta_ct[,t-1] + sigma_ar * epsilon_star[,t];
  }
}




model {
  

// prior
target += normal_lpdf(beta_w| 0, 1);

target += normal_lpdf(eps_r| 0, 1);

target += normal_lpdf(eps_c| 0, 1);

beta_edu_tilde ~ normal(0,1);
beta_gni_tilde ~ normal(0,1);
beta_lbw_tilde ~ normal(0,1);
beta_nmr_tilde ~ normal(0,1);
beta_anc_tilde ~ normal(0,1);

beta_dt2_tilde ~ normal(0,1);
beta_dt3_tilde ~ normal(0,1);
beta_dt4_tilde ~ normal(0,1);
beta_dt5_tilde ~ normal(0,1);



lambda_edu ~ cauchy(0,1);
lambda_gni ~ cauchy(0,1);
lambda_lbw ~ cauchy(0,1);
lambda_nmr ~ cauchy(0,1);
lambda_anc ~ cauchy(0,1);

tau_tilde ~ cauchy(0,1);
sigma ~ normal(0,10);

//ar1 deviance part
for(t in 1:yearLength){
epsilon_star[,t]~ normal(0,1);}

//likelihood
for(i in 1:totalObs_SBR){
            y_i[i] ~ normal(mu_ct[getc_i[i],gett_i[i]]+z_i[i]+delta_ct[getc_i[i],gett_i[i]],sigma_j[getj_i[i]]);
   }

}


