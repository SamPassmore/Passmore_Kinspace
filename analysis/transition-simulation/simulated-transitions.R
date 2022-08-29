library(ape)
library(bayestraitr)

modelstring_frequencies = function(results){
  # get column names
  cn_results = colnames(results)
  # find transition columns
  #q = results[,str_detect(cn_results, "q")] 
  
  # get model string and seperate it out
  ms = results$`Model string` %>% 
    stringr::str_remove('[\']') %>% 
    trimws() %>% 
    stringr::str_split('\\s') %>% 
    do.call(rbind, .) 
  # then get counts of estimates for each parameter
  ms2 = list()
  for(i in 1:ncol(ms)){
    ms2[[i]] = table(ms[,i])
  }
  
  # possible states for this lang-fam
  states = lapply(ms2, names) %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  # build a matrix of posible states
  occurances = matrix(NA, nrow = length(ms2), ncol = length(states))
  dimnames(occurances) = list(cn_results[stringr::str_detect(cn_results, "q")], states)
  
  # fill matrix
  for(i in seq_along(ms2)){
    row = ms2[[i]]
    occurances[i,names(row)] = row
  }
  occurances  
}

set.seed(1234)
## simulate a tree w/ 60 taxa (a smaller example of what we use)
sim.tree = rtree(60)

## simulate a 5-state variable
### where transitions from state 1->2 are ten times more likely that 2->1
m = matrix(c(0, 1, 1, 1, 1,
             1, 0, 1, 1, 1,
             1, 1, 0, 1, 1,
             1, 1, 1, 0, 1,
             10, 1, 1, 1, 0), ncol = 5, nrow = 5)
m
sim.trait = rTraitDisc(sim.tree, model = m, k = 5)
table(sim.trait)
sim.df = data.frame(sim.trait = sim.trait, taxa = names(sim.trait))

plot(sim.tree)
tiplabels(pch = 19, col = sim.trait)

bt_write(sim.tree, sim.df, 'sim.trait', dir = 'code/analysis/transition-simulation/')

### run in bt
## 1 2 RevJump exp 10 Iterations 50000 Sample 500 Burnin 2000

log = bt_read.log('code/analysis/transition-simulation/sim.trait.btdata.Log.txt')
plot(log$Lh, type = 'l')
freq = modelstring_frequencies(log)
rates = freq[,1] / rowSums(freq) 
sort(rates)
