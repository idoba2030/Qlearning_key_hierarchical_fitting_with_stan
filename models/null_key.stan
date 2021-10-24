data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of offers per trial



  //Behavioral data:
  //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> ch_card[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> ch_key[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> offer_left[Nsubjects,Ntrials];            //offered card in left bandit
  int<lower = 0> offer_right[Nsubjects,Ntrials];            //offered card in right bandit
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; 
  real capacity[Nsubjects,Ntrials];            //capacity of each subject
}

transformed data{
  int<lower = 1> Nparameters=5; //number of parameters
  vector[Narms] Q_cards_initial;     // initial values for Qcards (defined here to avoid doing this many times across iterations)
  vector[Nraffle] Q_keys_initial;     // initial values for Qkeys
  Q_cards_initial = rep_vector(0.5, Narms);
  Q_keys_initial = rep_vector(0.5, Nraffle);
}

parameters {
// Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] mu_aux;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] sigma_aux;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_individual_aux;
  vector[Nsubjects] beta_individual_aux;
  vector[Nsubjects] slope_rel_individual_aux;
  vector[Nsubjects] intercept_irrel_individual_aux;
  vector[Nsubjects] slope_irrel_individual_aux;
}


transformed parameters {
//declare variables and parameters
  real  alpha[Nsubjects];
  real  beta[Nsubjects];
  real  slope_rel[Nsubjects];
  real  intercept_irrel[Nsubjects];
  real  slope_irrel[Nsubjects];
  real  beta_range = 10;
  real  w_range = 4;
  
  for (subject in 1:Nsubjects) {
    alpha[subject]   = Phi_approx(mu_aux[1]  + sigma_aux[1]  * alpha_individual_aux[subject]);//
    beta[subject]    = Phi_approx(mu_aux[2] + sigma_aux[2] * beta_individual_aux[subject])*beta_range ;//
    slope_rel[subject]   = Phi_approx(mu_aux[3]  + sigma_aux[3]  * slope_rel_individual_aux[subject])*w_range-w_range/2;//
    intercept_irrel[subject]    = Phi_approx(mu_aux[4] + sigma_aux[4] * intercept_irrel_individual_aux[subject])*w_range-w_range/2 ;//
    slope_irrel[subject]   = Phi_approx(mu_aux[5]  + sigma_aux[5]  * slope_irrel_individual_aux[subject])*w_range-w_range/2;//
  }

}


model {
  
  // population level priors (hyper-parameters)
  mu_aux  ~ normal(0, 1);            
  sigma_aux ~ normal(0, 0.5);        

  // individual level priors (subjects' parameters)
  alpha_individual_aux~ normal(0, 1);
  beta_individual_aux ~ normal(0, 1);
  slope_rel_individual_aux ~ normal(0, 1);
  intercept_irrel_individual_aux ~ normal(0, 1);
  slope_irrel_individual_aux ~ normal(0, 1);
 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector [Narms] Q_cards;
    vector [Nraffle] Q_keys;
    vector[Nraffle] Qnet;
    for (trial in 1:Ntrials_per_subject[subject]){
    if (first_trial_in_block[subject,trial] == 1) {
      Q_cards=Q_cards_initial;
      Q_keys=Q_keys_initial;
    }
          Qnet[1]=(1 + slope_rel[subject] * capacity[subject,trial])*Q_cards[offer_left[subject,trial]]+(intercept_irrel[subject] +
          slope_irrel[subject]*capacity[subject,trial])*Q_keys[1] ;
          Qnet[2]=(1 + slope_rel[subject] * capacity[subject,trial])*Q_cards[offer_right[subject,trial]]+(intercept_irrel[subject] +
          slope_irrel[subject]*capacity[subject,trial])*Q_keys[2];

        //liklihood function
         target +=log_softmax(beta[subject] * Qnet)[ch_key[subject, trial]];

        //Qvalues update
        Q_cards[ch_card[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_cards[ch_card[subject,trial]]);
        Q_keys[ch_key[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_keys[ch_key[subject,trial]]);
      } 
  }
  }

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


generated quantities {
  real  mu_alpha;
  real  mu_beta;
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
