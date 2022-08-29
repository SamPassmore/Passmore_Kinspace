# calculate Cramer's V between generation clusters

library(lsr)
library(DescTools)
library(rcompanion)
library(dplyr)

## Function
est_chisq = function(a, b){
  cs = chisq.test(x = a, y = b)$statistics
  
  B = 4000
  dum = vector (mode = "numeric", length = B)
  for(i in 1:B){
    tb = table(a, b)
    rtot = rowSums(tb)
    ctot = colSums(tb)
    dum[i] <- chisq.test(r2dtable(1, rtot, ctot)[[1]])$statistic
  } 
  MC.p.value <- length(dum[dum > cs])/B
  MC.p.value
  #dum
}


condition1 <- c(30, 20, 50) 
condition2 <- c(35, 30, 35)
X <- cbind( condition1, condition2 )
rownames(X) <- c( 'choice1', 'choice2', 'choice3' )

# To test the null hypothesis that the distribution of choices
# is identical in the two conditions, we would run a chi-square
# test:
chisq.test(X)
# To estimate the effect size we can use Cramer's V:
cramersV( X )  # returns a value of 0.159
DescTools::CramerV(X) # returns a value of 0.159

g0 = read.csv('results/hdbscan/g0.csv')
g1 = read.csv('results/hdbscan/g1.csv')
g2 = read.csv('results/hdbscan/g2.csv')

# -- Correlation between g0 & g1 -- #

g0g1 = dplyr::full_join(g0, g1, by = "Glottocode", suffix = c(".g0", ".g1")) %>%
  filter(label_.g0 > -1 & label_.g1 > -1)

# Chi-sq Null: Two variables are independent
g0g1.cs = chisq.test(x = g0g1$label_.g0, y = g0g1$label_.g1, simulate.p.value = TRUE, B=10000)
# p-value ~0.0004 says there is dependence

g0g1.v = DescTools::CramerV(x = g0g1$label_.g0, y = g0g1$label_.g1)
est_chisq(g0g1$label_.g0, g0g1$label_.g1)
# -- Correlation between g1 & g2 -- #
g1g2 = dplyr::full_join(g1, g2, by = "Glottocode", suffix = c(".g1", ".g2")) %>%
  filter(label_.g1 > -1 & label_.g2 > -1)

g1g2.cs = chisq.test(x = g1g2$label_.g1, y = g1g2$label_.g2, simulate.p.value = TRUE, B=10000)
# p-value ~0.0004 says there is dependence
g1g2.v = DescTools::CramerV(x = g1g2$label_.g1, y = g1g2$label_.g2)

# Siblings to niblings
siblings = read.csv('results/hdbscan/siblings.csv')
niblings = read.csv('results/hdbscan/niblings.csv')

sibnibs = inner_join(siblings, niblings, "Glottocode", suffix = c(".sib", ".nib")) %>% 
  filter(label_.sib > -1, label_.nib > -1)

sibnib.cs = chisq.test(x = sibnibs$label_.sib, y = sibnibs$label_.nib, simulate.p.value = TRUE, B=10000)
sibnib.v = DescTools::CramerV(x = sibnibs$label_.sib, y = sibnibs$label_.nib)

# G2 to Niblings
nibsg2 = inner_join(niblings, g2, "Glottocode", suffix = c(".nib", ".g2")) %>% 
  filter(label_.nib > -1, label_.g2 > -1)

nibg2.cs = chisq.test(x = nibsg2$label_.nib, y = nibsg2$label_.g2, simulate.p.value = TRUE, B=10000)
nibg2.v = DescTools::CramerV(x = nibsg2$label_.nib, y = nibsg2$label_.g2)

# -- create output table -- # 

cs = list(g0g1.cs, g1g2.cs, sibnib.cs, nibg2.cs)
v = list(g0g1.v, g1g2.v, sibnib.v, nibg2.v)

purrr::map2(cs, v, function(x, y) c(x$statistic, x$p.value, y)) %>% 
  do.call(rbind, .)
