#aim: Generated simulated data for Narmed bandit task using hierarchical group parameters and individual parameters
#author: Nitzan Shahar, 2021

rm(list = ls())
source('./models/N_armed_bnd_key_param_recovey_forIDO.R') #get sim.block
library(tidyverse)

model_name = c('null')

Nsubjects           = 100
Nblocks             = 6
Ntrials_perblock    = 50
Narms               = 4
rndwlk              = read.csv('./functions/rndwlk_4frc_1000trials.csv', header =
                                 F)[, 1:Ntrials_perblock]
Nraffle             = 2


# generate population and subject level parameters -----------------------------------------------------------

#population location parameters

#true population level parameters
mu_alpha   = 0.5
mu_beta    = 4
mu1_w_rel = 0
mu0_w_irrel = 0.5
mu1_w_irrel = -0.5
#transform to aux scale
beta_range = 10
w_range= 4
mu_aux     = c(
  qnorm(mu_alpha),
  qnorm(mu_beta / beta_range),
  qnorm((mu1_w_rel+w_range/2)/w_range),
  qnorm((mu0_w_irrel+w_range/2)/w_range),
  qnorm((mu1_w_irrel+w_range/2)/w_range)
)

#transform back to natural scale (just for practice)
mu_alpha   = pnorm(mu_aux[1], 0, 1)

mu_beta    = pnorm(mu_aux[2], 0, 1) * beta_range

mu1_w_rel = pnorm(mu_aux[3], 0, 1)*w_range-w_range/2

mu0_w_irrel = pnorm(mu_aux[4], 0, 1)*w_range-w_range/2

mu1_w_irrel = pnorm(mu_aux[5], 0, 1)*w_range-w_range/2


#scale parameter for the random effect
sigma_aux  = c(0.75, 1, 0.5,0.5,0.5) 


#individual level parameters

#random effect for each individual
alpha_individual_aux = rnorm(Nsubjects, 0, 1)

beta_individual_aux = rnorm(Nsubjects, 0, 1)

slope_rel_individual_aux = rnorm(Nsubjects, 0, 1)

intercept_irrel_individual_aux = rnorm(Nsubjects, 0, 1)

slope_irrel_individual_aux = rnorm(Nsubjects, 0, 1)

#RL parameters per individual given population and group effects
alpha          = pnorm(mu_aux[1]  + sigma_aux[1] * alpha_individual_aux)

beta           = pnorm(mu_aux[2]  + sigma_aux [2] * beta_individual_aux) * beta_range

slope_rel = pnorm(mu_aux[3]  + sigma_aux [3] * slope_rel_individual_aux)*w_range-w_range/2

intercept_irrel = pnorm(mu_aux[4]  + sigma_aux [4] * intercept_irrel_individual_aux)*w_range-w_range/2

slope_irrel = pnorm(mu_aux[5]  + sigma_aux [5] * slope_irrel_individual_aux)*w_range-w_range/2

#plot true parameters
true.parameters = data.frame(
  subject = seq(1, Nsubjects),
  alpha  = alpha,
  beta   = beta,
  slope_rel = slope_rel,
  intercept_irrel = intercept_irrel,
  slope_irrel = slope_irrel
)
psych::multi.hist(true.parameters)

#check that sample means is near true population means
print(paste(round(mean(alpha), 2), round(mean(beta), 2), round(mean(slope_rel), 2),
            round(mean(intercept_irrel), 2),round(mean(slope_irrel), 2)))

# generate data -----------------------------------------------------------

cfg = list(
  Nblocks         = Nblocks,
  Ntrials_perblock = Ntrials_perblock,
  Narms           = Narms,
  Nraffle         = Nraffle, 
  #(i.e., offer Nraffle arms each trial from a deck of Narms)
  rndwlk          = rndwlk 
)

df <- lapply(1:Nsubjects, function(subject) {
  print(paste('subject', subject))
  sim.block(subject,cfg, true.parameters[subject, ])
})

df = do.call(rbind,df)


#save-------------------------------------------------------------------
save(
  df,
  file = paste(
    'data/',
    model_name,
    '_',
    Nsubjects,
    'subjects_',
    Nblocks,
    'blocks_',
    Ntrials_perblock,
    'trials_',
    Narms,
    'arms_simdata.Rdata',
    sep = ""
  )
)
save(
  true.parameters,
  file = paste(
    './data/',
    model_name,
    '_',
    Nsubjects,
    'subjects_',
    Nblocks,
    'blocks_',
    Ntrials_perblock,
    'trials_',
    Narms,
    'arms_parameters.Rdata',
    sep = ""
  )
)

#convert to standata format-------------------------------------------------

# add abort column to simulate missing trials
max_precent_of_aborted_trials = 0
df$abort <- 0
Nsubjects = max(df$subject)
Ntrials  = df %>% group_by(subject) %>% summarise(Ntrials_max = (length(trial))) %>%
  summarise(max(Ntrials_max)) %>% as.numeric()

for (subject in seq(1:max(df$subject))) {
  index_abort           = sample(
    which(df$subject == subject),
    runif(1, min = 0, max = max_precent_of_aborted_trials) * Ntrials
  )  #index of rows to abort
  df$abort[index_abort] = 1
}

df %>% group_by(subject) %>% summarise(mean(abort)) #count and omit aborted trials
df <- df[df$abort == 0,]
df %>% group_by(subject) %>% summarise(mean(abort))
df = df %>% mutate(first_trial_in_block = (block != lag(block, default = 0)) *
                     1) %>% as.data.frame()
#some housekeeping
library(dplyr)

# df$action        = df$choice
# df$unchosen      = df$offer1
# df$unchosen[df$choice == df$offer1] = df$offer2[df$choice == df$offer1]
# df$selected_offer = (df$choice == df$offer2) * 1 + 1
# df$fold = df$block

#make standata
source('./functions/make_mystandata.R')
data_for_stan <- make_mystandata(
  data = df,
  subject_column     = df$subject,
  block_column       = df$block,
  var_toinclude      = c(
    'trial',
    'subtrial',
    'card_left',
    'card_right',
    'ch_card',
    'ch_key',
    'reward',
    'first_trial_in_block'),
  additional_arguments = list(Narms = 4, Nraffle =
                                2, capacity = unique(df$capacity))
)

save(
  data_for_stan,
  file = paste(
    'data/',
    model_name,
    '_',
    Nsubjects,
    'subjects_',
    Nblocks,
    'blocks_',
    Ntrials_perblock,
    'trials_',
    Narms,
    'arms_standata.Rdata',
    sep = ""
  )
)



#check simulated data -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)
library(tidyverse)
#prepare data for sanity checks
df = df %>%
  mutate(
    reward_oneback = lag(reward) * 1,
    stay_key       = (ch_key == lag(ch_key)) * 1
  )

#sanity check1: model effect of reward on stay_key
m_sanity=glmer(formula=stay_key~1+reward_oneback+(1+reward_oneback|subject),family = binomial(link = "logit"),nAGQ=0,data=df)
plot(effect("reward_oneback",m_sanity))
#plot(effect("reward_oneback:capacity",m_sanity))

#model correlation of reward individual effect and capacity
a=df%>%group_by(subject)%>%summarise(capacity=mean(capacity))%>%select(capacity)
a=a$capacity
cor(a,coef(m_sanity)$subject[,2])
plot(a,coef(m_sanity)$subject[,2])
#sanity check 2: plot mean reward vs expected value
model <-
  glmer(reward ~ exp_val_ch + (1 |
                                subject),
        data = df,
        family = binomial)
plot(effect('exp_val_ch', model))


#sanity check 3: plot mean reward vs parameters
model <- glmer(
  reward ~ capacity+poly(alpha, 2) + beta + (1 | subject),
  data = merge(df, as.data.frame(true.parameters), by = c('subject')),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 0
)
plot(effect('alpha', model)) #(if you use uniform alpha and fixed beta - you will be able to see a nice hyperbolic)
plot(effect('beta', model)) #(if you use uniform alpha - you will be able to see a nice hyperbolic)
plot(effect('capacity', model)) #(if you use uniform alpha - you will be able to see a nice hyperbolic)
