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
  int<lower = 1> Nparameters=3; //number of parameters
  vector[Narms] Q_cards_initial;     // initial values for Qcards (defined here to avoid doing this many times across iterations)
  vector[Nraffle] Q_keys_initial;     // initial values for Qkeys
  Q_cards_initial = rep_vector(0.5, Narms);
  Q_keys_initial = rep_vector(0.5, Nraffle);
  
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
}


transformed parameters {
  //declare variables and parameters
  real  alpha [Nsubjects]; //cards learning rate
  real  beta_rel[Nsubjects]; // card learning
  real  beta_irrel[Nsubjects]; //key learning
  
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
          Qnet[1]=beta_rel[subject]*Q_cards[card_right[subject,trial]]+beta_irrel[subject]*Q_keys[1];
          Qnet[2]=beta_rel[subject]*Q_cards[card_left[subject,trial]]+beta_irrel[subject]*Q_keys[2];

        //likelihood function
         target +=log_softmax(Qnet)[ch_key[subject, trial]];

        //Qvalues update
        Q_cards[ch_card[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_cards[ch_card[subject,trial]]);
        Q_keys[ch_key[subject,trial]] += alpha[subject] * (reward[subject,trial] - Q_keys[ch_key[subject,trial]]);
      } 
  }
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

