#aim: Hierarchical fit Stan 
#contributor: Nitzan Shahar, 2021


rm(list=ls())

# fit stan model  --------------------------------------------
library(rstan) 
load('data/null_100subjects_6blocks_50trials_4arms_standata.Rdata')
library(parallel)
num_cores=detectCores()
{
  start_time <- Sys.time()

    rl_fit<- stan(file = './models/null_key.stan', 
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=num_cores,
                seed = 123,
                cores =num_cores) 

  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/null_100subjects_6blocks_50trials_4arms_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
save(pars, file='./data/null_100subjects_6blocks_50trials_4arms_extracted_parameters.rdata')


