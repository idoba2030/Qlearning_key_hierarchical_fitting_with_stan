data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of cards per trial
  
  
  //Behavioral data:
    //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> ch_card[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> ch_key[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> card_left[Nsubjects,Ntrials];            //offered card in left bandit
  int<lower = 0> card_right[Nsubjects,Ntrials];            //offered card in right bandit
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; 

}

transformed data{
  int<lower = 1> Nparameters=6; //number of parameters
  vector[Narms] Q_cards_initial;     // initial values for Qcards (defined here to avoid doing this many times across iterations)
  vector[Nraffle] Q_keys_initial;     // initial values for Qkeys
  vector[Nraffle] Pers_val_initial;     // initial values for keys perseveration
  Q_cards_initial = rep_vector(0, Narms);
  Q_keys_initial = rep_vector(0, Nraffle);
  Pers_val_initial = rep_vector(0.5, Nraffle); // it should be 1/Nraffle, but Nraffle is "int" and not "real"
}

parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_rel_random_effect;
  vector[Nsubjects] beta_irrel_random_effect;
  vector[Nsubjects] alpha_pers_random_effect;
  vector[Nsubjects] beta_pers_random_effect;
  vector[Nsubjects] key_bias_random_effect;
}


transformed parameters {
  //declare variables and parameters
  real  alpha [Nsubjects]; //cards learning rate
  real  beta_rel[Nsubjects]; // card choice parameter
  real  beta_irrel[Nsubjects]; //key choice parameter
  real  alpha_pers [Nsubjects]; //alpha_perseveration
  real  beta_pers[Nsubjects]; // beta_perseveration
  real  key_bias[Nsubjects]; //key_bias
  
  for (subject in 1:Nsubjects) {
    alpha[subject]          = inv_logit(
      population_locations[1]  + population_scales[1] *
        alpha_random_effect[subject]
    );
    beta_rel[subject]          = exp(
      population_locations[2] + population_scales[2] *
        beta_rel_random_effect[subject]
    );
    beta_irrel[subject]          = exp(
      population_locations[3] + population_scales[3] *
        beta_irrel_random_effect[subject]
    );
    alpha_pers[subject]          = inv_logit(
      population_locations[4]  + population_scales[4] *
        alpha_pers_random_effect[subject]
    );
    beta_pers[subject]          = exp(
      population_locations[5] + population_scales[5] *
        beta_pers_random_effect[subject]
    );
    key_bias[subject]          = exp(
      population_locations[6] + population_scales[6] *
        key_bias_random_effect[subject]
    );

}
}

model {
  
  // population level priors (hyper-parameters)
  population_locations   ~ normal(0,2);
  population_scales      ~ cauchy(0,2);    
  
  // individual level priors (subjects' parameters)
  alpha_random_effect ~ std_normal();
  beta_rel_random_effect ~ std_normal();
  beta_irrel_random_effect ~ std_normal();
  alpha_pers_random_effect ~ std_normal();
  beta_pers_random_effect ~ std_normal();
  key_bias_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector [Narms] Q_cards;
    vector [Nraffle] Q_keys;
    vector[Nraffle] Qnet;
    vector[Nraffle] Pers_val;
    for (trial in 1:Ntrials_per_subject[subject]){
    if (first_trial_in_block[subject,trial] == 1) {
      Q_cards=Q_cards_initial;
      Q_keys=Q_keys_initial;
      Pers_val=Pers_val_initial;
    }
          Qnet[1]=beta_rel[subject]*Q_cards[card_right[subject,trial]]+
          beta_irrel[subject]*Q_keys[1]+
          beta_pers[subject]*Pers_val[1]+
          key_bias[subject]; //We compound the value of the card appearing on the right, the value of the right key,
          //a general tendency to repeat the right key, and a bias towards the right key.
          Qnet[2]=beta_rel[subject]*Q_cards[card_left[subject,trial]]+
          beta_irrel[subject]*Q_keys[2]+
          beta_pers[subject]*Pers_val[2]; //We don't need a key bias here as it could be a pos/neg number.

        //likelihood function
         target +=log_softmax(Qnet)[ch_key[subject, trial]]; //We transpose the compounded Qnet to a probability and
         //take the chosen probability. 
         //We aim to find the parameters which maximize the likelihood which is P(Data|Parameters). 

        //Qvalues update
        Q_cards[ch_card[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_cards[ch_card[subject,trial]]); //update card_value according to reward
        Q_keys[ch_key[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_keys[ch_key[subject,trial]]); //update key value according to reward

        //Perseveration update
        Pers_val*= (1-alpha_pers[subject]); //step 1 of perseveration update
        Pers_val[ch_key[subject,trial]] += alpha_pers[subject]; //step 2 of perseveration update

      } 
  }
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

