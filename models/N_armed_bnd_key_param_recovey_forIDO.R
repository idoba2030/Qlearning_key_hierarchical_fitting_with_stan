##############################################################################################################
######## This code generates parameters recovery for N armed bandit task #####################################
##############################################################################################################
rm(list = ls())
library(parallel)
source('functions/subtrial.R')
source('functions/func_logit.R')
rndwlk <-
  read.csv('functions/rndwlk_4frc_1000trials.csv', header = F)


num_cores = detectCores()

#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,cfg, parameters) {
  capacity=rnorm(1,mean=0,sd=1)
  #set parameters
  alpha = parameters$alpha
  beta  = parameters$beta
  slope_rel= parameters$slope_rel
  intercept_irrel = parameters$intercept_irrel
  slope_irrel = parameters$slope_irrel
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  Ntrl = cfg$Ntrl
  
  df_all = data.frame()
  
  for (block in 1:Nblocks) {
    Q_cards = rep(0.5, Narms)
    Q_keys = rep(0.5, Nraffle)
    for (t in 1:Ntrials_perblock) {
      subtrial1 = subtrial(subject,
                           block,
                           t,
                           1,
                           Q_cards,
                           Q_keys,
                           capacity,
                           cfg,
                           parameters)
      Q_cards = unlist(subtrial1[1])
      Q_keys = unlist(subtrial1[2])
      df1 = do.call(rbind, subtrial1[3])
      offered = c(df1$offer_left, df1$offer_right)
      subtrial2 = subtrial(subject,
                           block,
                           t,
                           2,
                           Q_cards,
                           Q_keys,
                           capacity,
cfg,
parameters,
offered)
      Q_cards = unlist(subtrial2[1])
      Q_keys = unlist(subtrial2[2])
      df2 = do.call(rbind, subtrial2[3])
      df_subtrl = rbind(df1, df2)
      df_all = rbind(df_all, df_subtrl)
    }
  }
  
  return (df_all)
}

#### fit Rescorla-Wagner block for participant ----
# fit.block = function(par, df, Narms, Nraffle) {
#   Q_cards      = rep(0, Narms)
#   Q_keys = rep(0, Nraffle)
#   trials = dim(df)[1]
#   offer_left = df$offer_left
#   offer_right = df$offer_right
#   action = df$action
#   key = df$key
#   reward = df$reward
#   pAction = rep(NA, trials)
#   alpha = mylogit(par[1])
#   beta   = exp(par[2])
#   w = mylogit(par[3])
#   for (t in 1:trials) {
#     Q_cards_offered = Q_cards[c(offer_left[t], offer_right[t])]
#     Qnet = w * Q_keys + Q_cards_offered
#     pAction[t]   = exp(beta * Qnet[key[t]]) / sum(exp(beta * Qnet))
#     Q_cards[action[t]] = Q_cards[action[t]] + alpha * (reward[t] - Q_cards[action[t]])
#     Q_keys[key[t]] = Q_keys[key[t]] + alpha * (reward[t] - Q_keys[key[t]])
#   }
#   return (-sum(log(pAction)))
# }
#
#
#
# ##### Exercise 2 -----
# #generate parameters and data for N agents.
# N   <- 100                  #number of agents
# Narms <- 4 #number of alternatives
# Nraffle<-  2
# Ntrl <- 200  #number of trls
#
# true.parms <-
#   data.frame(alpha = runif(N, min = 0, max = 1),
#              beta = runif(N, min = 1, max = 8),
#              w = runif(N, min = 0, max = 1)) #parameter of learning for key
#
# #sim data for N agents, with Narms=2,3 or 4, Ntrl=25,50,100,200 or 1000
# df <- lapply(1:N, function(s) {
#   sim.block(Ntrl[1],Nraffle[1], Narms[1], true.parms$alpha[s], true.parms$beta[s],true.parms$w[s], rndwlk)
# })
# #mc.cores = num_cores)
#
# #recover param from data
# rec.parms <-
#
#   lapply(1:N, function(s) {
#     if (s %% 10 == T) {
#       print(paste('Narms=', Narms[1], ' Ntrl=', Ntrl[1], ' subj=', s, sep = ''))
#     }
#
#     optim(
#       par = c(runif(1, -5, 5), runif(1, -3, 3),runif(1, -5, 5)),
#       fn = fit.block,
#       df = df[[s]],
#       Narms = Narms[1],
#       Nraffle = Nraffle[1],
#       lower = c(-5, -3),
#       upper = c(5, 3),
#       method = "L-BFGS-B"
#     )$par
#   })
# rec.parms = do.call(rbind, rec.parms)
# library(gtools)
# plot(true.parms[, 1], gtools::inv.logit(rec.parms[, 1]))
# plot(true.parms[, 2], exp(rec.parms[, 2]))
# plot(true.parms[, 3], gtools::inv.logit(rec.parms[, 3]))
