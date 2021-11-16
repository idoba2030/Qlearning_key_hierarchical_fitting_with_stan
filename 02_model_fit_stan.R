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
  stanmodel <- stan_model(file = 'models/null_key.stan')
    rl_fit<- sampling(stanmodel,
                data=data_for_stan, 
                iter=2000,
                warmup = 1000,
                chains=4,
                seed = 123,
                cores =4) 
  gqs_model=stan_model(file = 'models/try.stan')
  gen <- gqs(gqs_model, draws = as.matrix(rl_fit),data=data_for_stan)
  end_time <- Sys.time()
  end_time-start_time
}

#save
saveRDS(rl_fit, './data/null_100subjects_6blocks_50trials_4arms_RDSfile.rds')

pars <- rstan::extract(rl_fit, permuted = TRUE)
pars_gen=rstan::extract(gen, permuted = TRUE)
save(pars, file='./data/null_100subjects_6blocks_50trials_4arms_extracted_parameters.rdata')
save(pars_gen, file='./data/null_100subjects_6blocks_50trials_4arms_extracted_gen_parameters.rdata')


