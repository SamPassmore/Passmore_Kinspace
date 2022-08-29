# /usr/bin/Rscript

library(rethinking)
library(dplyr)

# model the effect of networks on frequency and on diversity

## Import data
files = list.files('results/global/data/', full.names = TRUE)

data = files %>% 
  lapply(., read.csv) %>%
  do.call(rbind, .)

# centralise data
data$diversity.c = data$diversity - mean(data$diversity)
data$frequency.c = data$frequency - mean(data$frequency)
data$centrality.c = data$centrality - mean(data$centrality)
data$strength.c = data$strength - mean(data$strength)

# log then centralise data
data$diversity.lc = log(data$diversity) - mean(log(data$diversity))
data$frequency.lc = log(data$frequency) - mean(log(data$frequency))
data$centrality.lc = log(data$centrality) - mean(log(data$centrality))
data$strength.lc = log(data$strength) - mean(log(data$strength))

# plot raw data

# centrality
ggplot(data, aes(group = type, col = type, x = centrality, y = log(frequency))) + 
  geom_point() +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

ggplot(data, aes(group = type, col = type, x = centrality, y = log(diversity))) + 
  geom_point() +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

# strength
ggplot(data, aes(group = type, col = type, x = strength, y = log(diversity))) + 
  geom_point() +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

ggplot(data, aes(group = type, col = type, x = strength, y = log(diversity))) + 
  geom_point() +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

# plot centralised
## centrality
ggplot(data, aes(x = centrality.c, y = frequency.c, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

ggplot(data, aes(x = centrality.c, y = diversity.c, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA)

# strength
ggplot(data, aes(x = strength.c, y = frequency.c, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

ggplot(data, aes(x = strength.c, y = diversity.c, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA)

ggplot(data, aes(x = strength.lc, y = frequency.lc, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA, lwd = 0.2)

ggplot(data, aes(x = strength.lc, y = diversity.lc, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA)


#------ Diversity models -------- #
#--- Centrality ---#

# null model 
fit_null.d = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
prior <- extract.prior( fit_null.d , n=1e4 )
dens( prior$a , adj=0.1 )

# posterior check
precis(fit_null.d)

# centrality model
fit_diversity = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a + ba*centrality.lc,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
prior <- extract.prior( fit_diversity , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior check
precis(fit_diversity)

# centrality model w random ints
fit_diversity2 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba*centrality.lc,
    a[type] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

prior <- extract.prior( fit_diversity2 , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

precis(fit_diversity2, depth = 2)

# centrality model w random slopes & ints
fit_diversity3 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba[type]*centrality.lc,
    a[type] ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

prior <- extract.prior( fit_diversity3 , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior
precis(fit_diversity3, depth = 2)

fit_diversity4 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a + ba[type]*centrality.lc,
    a ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# model comparison
c1 = compare(fit_null.d, fit_diversity, fit_diversity2, fit_diversity3, fit_diversity4)

#####--- Strength ---#####

# strength model
fit_ds = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a + ba*strength.lc,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
prior <- extract.prior( fit_ds , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior check
precis(fit_ds)

# centrality model w random ints
fit_ds2 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

prior <- extract.prior( fit_ds2 , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

precis(fit_ds2, depth = 2)

# centrality model w random slopes & ints
fit_ds3 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba[type]*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

prior <- extract.prior( fit_ds3 , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior
precis(fit_ds3, depth = 2)

fit_ds4 = quap(
  alist(
    diversity.lc ~ dnorm(mu, sigma),
    mu <- a + ba[type]*strength.lc,
    a ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# model comparison
c1s = compare(fit_null.d, fit_ds, fit_ds2, fit_ds3, fit_ds4)


#------ Frequency models -------- #
# null model 
fit_null.f = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
prior <- extract.prior( fit_null , n=1e4 )
dens( prior$a , adj=0.1 )

# posterior check
precis(fit_null)

# centrality model
fit_frequency = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a + ba*centrality.lc,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
set.seed(1999)
prior <- extract.prior( fit_diversity , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior check
precis(fit_diversity)

# centrality model w random ints
fit_frequency2 = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba*centrality.lc,
    a[type] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_diversity2, depth = 2)

# centrality model w random slopes & ints
fit_frequency3 = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba[type]*centrality.lc,
    a[type] ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_diversity3, depth = 2)

# model comparison
c2 = compare(fit_null.f, fit_frequency, fit_frequency2, fit_frequency3)

#---- Strength ----#

# frequeny model
fit_fs = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a + ba*strength.lc,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
set.seed(1999)
prior <- extract.prior( fit_fs , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior check
precis(fit_diversity)

# centrality model w random ints
fit_fs2 = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_fs2, depth = 2)

# centrality model w random slopes & ints
fit_fs3 = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba[type]*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_diversity3, depth = 2)

fit_fs4 = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a + ba[type]*strength.lc,
    a ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# model comparison
c2s = compare(fit_null.f, fit_fs, fit_fs2, fit_fs3, fit_fs4)

# -- Predicting centrality with strength -- #

fit_cs = quap(
  alist(
    centrality.lc ~ dnorm(mu, sigma),
    mu <- a + ba*strength.lc,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# prior check
set.seed(1999)
prior <- extract.prior( fit_cs , n=1e4 )
dens( prior$a , adj=0.1 )
dens( prior$ba , adj=0.1 )

# posterior check
precis(fit_cs)

# centrality model w random ints
fit_fcs2 = quap(
  alist(
    centrality.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_fcs2, depth = 2)

# centrality model w random slopes & ints
fit_fcs3 = quap(
  alist(
    centrality.lc ~ dnorm(mu, sigma),
    mu <- a[type] + ba[type]*strength.lc,
    a[type] ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_fcs3, depth = 2)

fit_fcs4 = quap(
  alist(
    centrality.lc ~ dnorm(mu, sigma),
    mu <- a + ba[type]*strength.lc,
    a ~ dnorm(0, 1),
    ba[type] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

precis(fit_fcs4, depth = 2)


# model comparison
c2s = compare(fit_cs, fit_fcs2, fit_fcs3, fit_fcs4)


c_out = rbind(c1, c2)
c_out$response = rep(c("Diversity", "Strength"), each = 4)

write.csv(c_out, file = "results/global/global_model_results.csv")
