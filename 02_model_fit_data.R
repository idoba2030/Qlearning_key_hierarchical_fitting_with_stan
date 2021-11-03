# #make standata ----------------------------------------------------------
source('functions/make_mystandata.R')
library(tidyverse)
library(dplyr)
load('data/data_cards.Rdata')
cards$first_trial_in_block = (cards$block != lag(cards$block, default =
                                                   0)) * 1
Nsubjects = 20
Narms = 4
Nblocks = length(unique(cards$block))
Ntrials_perblock = max(cards$trial) + 1
df = cards %>% mutate(subj = as.numeric(subj)) %>% filter(subj <= Nsubjects) %>% select(
  subj,
  block,
  trial,
  subtrial,
  card_left,
  card_right,
  ch_card,
  ch_key,
  reward,
  first_trial_in_block,
  avg_capacity
)
df=as.data.frame.data.frame(df)
data_for_stan <- make_mystandata(
  data = df,
  subject_column     = df$subj,
  block_column       = df$block,
  var_toinclude      = c(
    'trial',
    'subtrial',
    'card_left',
    'card_right',
    'ch_card',
    'ch_key',
    'reward',
    'first_trial_in_block'
  ),
  additional_arguments = list(
    Narms = 4,
    Nraffle =
      2,
    capacity = unique(df$avg_capacity)
  )
)


save(
  data_for_stan,
  file = paste(
    'data/',
    'real_data',
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

rm(list = ls())

# fit stan model  --------------------------------------------
library(rstan)
load('data/real_data_20subjects_3blocks_50trials_4arms_standata.Rdata')
library(parallel)
num_cores = detectCores()
{
  start_time <- Sys.time()
  stanmodel <- stan_model(file = 'models/null_key.stan')
  rl_fit <- sampling(
    stanmodel,
    data = data_for_stan,
    iter = 20,
    warmup = 10,
    chains = 4,
    seed = 123,
    cores = 4
  )
  gqs_model = stan_model(file = 'models/try.stan')
  gen <-
    gqs(gqs_model, draws = as.matrix(rl_fit), data = data_for_stan)
  end_time <- Sys.time()
  end_time - start_time
}

#save
saveRDS(rl_fit,
        'data/real_data_20subjects_3blocks_50trials_4arms_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file = 'data/real_data_20subjects_3blocks_50trials_4arms_extracted_parameters.rdata')
