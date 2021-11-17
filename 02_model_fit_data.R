# #make standata ----------------------------------------------------------

rm(list = ls())
source('functions/make_mystandata.R')
library(tidyverse)
library(dplyr)
load('data/data_cards.Rdata')
cards$first_trial_in_block = (cards$block != lag(cards$block, default =
                                                   0)) * 1
Nsubjects = 169
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
  first_trial_in_block
)
df = as.data.frame.data.frame(df)
#change data from zero index to one index
df = df %>% mutate(
  card_left = card_left + 1,
  card_right = card_right + 1,
  ch_card = ch_card + 1,
  ch_key = ch_key + 1
)
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
  additional_arguments = list(Narms = 4,
                              Nraffle =
                                2)
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
load('data/real_data_169subjects_3blocks_50trials_4arms_standata.Rdata')
library(parallel)
num_cores = detectCores()
{
  start_time <- Sys.time()
  rl_fit <- stan(
    file =  'models/null_key.stan',
    data = data_for_stan,
    iter = 2,
    warmup = 1,
    chains = 1,
    seed = 123,
    cores = 1
  )
}

#save
saveRDS(rl_fit,
        'data/real_data_169subjects_3blocks_50trials_4arms_RDSfile.rds')
pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file = 'data/real_data_169subjects_3blocks_50trials_4arms_extracted_parameters.rdata')
