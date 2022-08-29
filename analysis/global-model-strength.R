library(rethinking)
library(ggplot2)
library(patchwork)
library(dplyr)

# colours
cols = c("#3a4a7b", "#57B5ED","#ED5C4D", "#FF9438", "#FBBE4B")


## Import data
files = list.files('results/global/data/', full.names = TRUE)

data = files %>% 
  lapply(., read.csv, stringsAsFactors = TRUE) %>%
  do.call(rbind, .)

data$type_index = as.numeric(data$type)

# data transformations
# centralise data
data$diversity.c = data$diversity - mean(data$diversity)
data$frequency.c = data$frequency - mean(data$frequency)
data$strength.c = data$strength - mean(data$strength)

# log then centralise data
data$diversity.lc = log(data$diversity) - mean(log(data$diversity))
data$frequency.lc = log(data$frequency) - mean(log(data$frequency))
data$strength.lc = log(data$strength) - mean(log(data$strength))


# -- Visualise data -- #

p1 = ggplot(data, aes(x = strength.c, y = frequency.c, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA, lwd = 0.5)

p2 = ggplot(data, aes(x = strength.lc, y = frequency.lc, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA, lwd = 0.5)


p1 / p2

pdf('results/best-model-plot.pdf', width = 10, height = 8)
ggplot(data, aes(x = strength.c, y = diversity.c, group = type, col = type)) + 
  geom_jitter(width = 0.1, size = 3) +
  geom_smooth(method = "lm", fill = NA, size=2) + 
  theme_light() +
  scale_color_manual(labels = c(bquote(~G^0), bquote(~G^+1), bquote(~G^+2), "Niblings", "Siblings"), 
                     values=cols) + 
  ylab("Centralised Diversity score") + 
  xlab('Centralised Strength score') + 
  facet_wrap(~type) + 
  theme(legend.title = element_blank(), legend.position = c(0.93, 0.25), 
        text = element_text(size=21), strip.text.x = element_blank(), 
        axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"))
dev.off()

p4 = ggplot(data, aes(x = strength.lc, y = diversity.lc, group = type, col = type)) + 
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm", fill = NA)

p3 / p4


# -- Models -- #
# -- Diversity -- #

# Null model
diversity_null = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# By Strength
diversity_strength = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a + ba*strength.c,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# By Strength with random intercepts for type
diversity_strength_randint = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a[type_index] + ba*strength.c,
    a[type_index] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data[,c("diversity.c", "strength.c", "type_index")]
)

diversity_strength_randslopes = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a + ba[type_index]*strength.c,
    a ~ dnorm(0, 1),
    ba[type_index] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

diversity_strength_randintslopes = quap(
  alist(
    diversity.c ~ dnorm(mu, sigma),
    mu <- a[type_index] + ba[type_index]*strength.c,
    a[type_index] ~ dnorm(0, 1),
    ba[type_index] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

diversity_comparison = rethinking::compare(diversity_null, diversity_strength, diversity_strength_randint, diversity_strength_randslopes, diversity_strength_randintslopes)
plot(diversity_comparison)

write.csv(diversity_comparison, "results/global/models/diversity_comparison.csv")

top_model = precis(diversity_strength_randslopes, depth = 2)
plot(top_model)

# -- Frequency -- # 
# Null model
frequency_null = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# By Strength
frequency_strength = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a + ba*strength.c,
    a ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

# By Strength with random intercepts for type
frequency_strength_randint = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type_index] + ba*strength.c,
    a[type_index] ~ dnorm(0, 1),
    ba ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

frequency_strength_randslopes = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a + ba[type_index]*strength.c,
    a ~ dnorm(0, 1),
    ba[type_index] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

frequency_strength_randintslopes = quap(
  alist(
    frequency.lc ~ dnorm(mu, sigma),
    mu <- a[type_index] + ba[type_index]*strength.c,
    a[type_index] ~ dnorm(0, 1),
    ba[type_index] ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = data
)

frequency_comparison = compare(frequency_null, frequency_strength, frequency_strength_randint, frequency_strength_randslopes, frequency_strength_randintslopes)

write.csv(frequency_comparison, "results/global/models/frequency_comparison.csv")

# -- Models -- #

