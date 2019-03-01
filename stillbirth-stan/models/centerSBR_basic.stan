
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
  int <lower=0,upper=5> getj_i[totalObs_SBR];      

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

  real beta_edu;
  real beta_gni;
  real beta_lbw;
  real beta_nmr;
  real beta_anc;
  
  //deviance part
  real beta_dt2;
  real beta_dt3;
  real beta_dt4;
  real beta_dt5;
  
  real<lower=0,upper=20> sigma_j[5];

  real<lower=0,upper=100> sigma_r;
  real<lower=0,upper=100> sigma_c;
  
  real eps_w;
  vector[totalRegion] eps_r;
  vector[totalIndex_covar] eps_c;
}
transformed parameters {

   matrix[totalIndex_covar,yearLength] mu_ct;
   real z_i[totalObs_SBR];
   real beta_w = sigma_w * eps_w;
   vector[totalRegion] beta_r; 
   vector[totalIndex_covar] beta_c;
//intercept   
   for(c in 1:totalIndex_covar){
   beta_c[c] = beta_r[getr_c[c]] + sigma_c* eps_c[c];
   }
   
   for(r in 1:totalRegion){
    beta_r[r]= beta_w + sigma_r* eps_r[r];
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
    z_i[i] = beta_dt2*dummy_datatype2_i[i]+
             beta_dt3*dummy_datatype3_i[i]+
             beta_dt4*dummy_datatype4_i[i]+
             beta_dt5*dummy_datatype5_i[i];
           
  }
}

model {
// prior
eps_w ~ normal(0,1);

for(r in 1:totalRegion){
eps_r[r] ~ normal(0,1);
}
for(c in 1:totalIndex_covar){
eps_c[c] ~ normal(0,1);
}
beta_edu ~ normal(0,1);
beta_gni ~ normal(0,1);
beta_lbw ~ normal(0,1);
beta_nmr ~ normal(0,1);
beta_anc ~ normal(0,1);

beta_dt2 ~ normal(0,1);
beta_dt3 ~ normal(0,1);
beta_dt4 ~ normal(0,1);
beta_dt5 ~ normal(0,1);

//likelihood
for(i in 1:totalObs_SBR){
            y_i[i] ~ normal(mu_ct[getc_i[i],gett_i[i]]+z_i[i],sigma_j[getj_i[i]]);
   }

}
