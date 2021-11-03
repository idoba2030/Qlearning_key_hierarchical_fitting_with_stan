transformed data{
  int<lower = 1> Nparameters=5; //number of parameters
  real beta_range = 10;
  real w_range = 4;

}

parameters {
// Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] mu_aux;                    //vector with the population level mean for each model parameter
}


model {
  
  // population level priors (hyper-parameters)
mu_aux  ~ normal(0, 1);  
}
generated quantities {
  real mu_alpha;
  real mu_beta;
  real mu1_w_rel;
  real mu0_w_irrel;
  real mu1_w_irrel;
  //population parameters
  mu_alpha=Phi_approx(mu_aux[1]);
  mu_beta=Phi_approx(mu_aux[2])*beta_range;
  mu1_w_rel=Phi_approx(mu_aux[3])*w_range-w_range/2;
  mu0_w_irrel=Phi_approx(mu_aux[4])*w_range-w_range/2;
  mu1_w_irrel=Phi_approx(mu_aux[5])*w_range-w_range/2;
}
