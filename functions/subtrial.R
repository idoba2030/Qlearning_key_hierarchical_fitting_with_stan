subtrial = function(subject,
                    block,
                    t,
                    subtrial,
                    Q_cards,
                    Q_keys,
                    capacity,
                    cfg,
                    parameters,
                    offered = NULL) {
  #set parameters
  alpha = parameters$alpha
  beta  = parameters$beta
  slope_rel = parameters$slope_rel
  intercept_irrel = parameters$intercept_irrel
  slope_irrel = parameters$slope_irrel
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  
  if (subtrial %% 2 == 1) {
    #draw two cards if in first offer
    options = sample(1:Narms, Nraffle)
  }
  else{
    options = c(1:Narms)[-offered]
  }
  Q_cards_offered = Q_cards[options] #use their Q values
  Qnet = (1 + slope_rel * capacity) *
    Q_cards_offered + (intercept_irrel + slope_irrel *
                         capacity) * Q_keys
  #sum Q values for probabilities from both keys and cards
  p         = exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
  ch_card = sample(options, 1, prob = p) #chose a card according to probs
  ch_key = which(options == ch_card) #get key of chosen card
  reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, t], expvalues[ch_card, t])) #reward according to card
  #update Q_keys and Q_cards
  Q_cards[ch_card] = Q_cards[ch_card] + alpha * (reward - Q_cards[ch_card])
  Q_keys[ch_key] = Q_keys[ch_key] + alpha * (reward - Q_keys[ch_key])
  return (list(
    Q_cards,
    Q_keys,
    data.frame(
      subject,
      block,
      trial = t,
      subtrial,
      card_left = options[1],
      card_right = options[2],
      ch_card,
      ch_key,
      reward,
      Q_ch_card = Q_cards[ch_card],
      Q_unch_card = Q_cards[options[which(options != ch_card)]],
      exp_val_ch = expvalues[ch_card, t],
      exp_val_unch = expvalues[options[which(options != ch_card)], t],
      Q_ch_key = Q_keys[ch_key],
      Q_unch_key = Q_keys[which(options != ch_card)],
      capacity
    )
  ))
}
