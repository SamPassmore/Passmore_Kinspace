# /usr/bin/Rscript

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(ggplot2)
  library(ggtext)
})

dir.create(path = "results/brms/", recursive = TRUE)

iter = 10000
chains = 2

# model the effect of networks on frequency and on diversity

## Import data
files = list.files('results/global/data/', full.names = TRUE)

data = files %>% 
  lapply(., read.csv) %>%
  do.call(rbind, .)

# Raw count models
fit.0r = brm(frequency ~ Freq,
             data = data,
             save_pars = save_pars(all = TRUE), 
             iter = iter, chains = chains,
             file = "results/brms/simple_counts"
             )

fit.1r = brm(
  frequency ~ Freq + (1 | type),
  data = data,
  control = list(adapt_delta = 0.99),
  save_pars = save_pars(all = TRUE),
  iter = iter, chains = chains,
  file = "results/brms/rintercept_counts"
)

fit.2r = brm(
  frequency ~ Freq + (Freq | type),
  data = data,
  control = list(adapt_delta = 0.99),
  iter = iter, chains = chains,
  save_pars = save_pars(all = TRUE),
  file = "results/brms/rslopes_counts"
)

fit.0r = add_criterion(fit.0r, criterion = "loo", moment_match = TRUE)
fit.1r = add_criterion(fit.1r, criterion = "loo", moment_match = TRUE)
fit.2r = add_criterion(fit.2r, criterion = "loo", moment_match = TRUE, reloo = TRUE)

loo_compare(fit.0r, fit.1r, fit.2r)

coef(fit.2r)

data$type = recode(data$type,
                      g0 = "G^0",
                      g1 = "G^1",
                      g2 = "G^2",
                      niblings = "Niblings",
                      siblings = "Siblings")

data$type = factor(data$type,
                      levels = c("Niblings",
                                 "Siblings",
                                 "G^0",
                                 "G^1",
                                 "G^2"))

cols = c("#57B5ED", "#ED5C4D", "#F4E9DA", "#FBBE4B", "#ED8F57")


p1 = ggplot(data, aes(y = frequency, x = Freq)) +
  geom_point(aes(col = type, fill = type), colour = "black",
             shape = 21, size = 4, alpha = 0.7) +
  xlab("Count of Network links") +
  ylab("Count of type") +
  theme_linedraw() +
  facet_wrap(~type, nrow = 2) + 
  coord_cartesian(clip = 'off') + 
  guides(fill = "none") +
  scale_fill_manual(values = cols) + 
  theme(legend.title = element_blank(), legend.direction = "horizontal",
        strip.text = element_markdown(),
        legend.position = "bottom",
        legend.box.just = "right",
        legend.text = element_markdown())

ggsave(filename = "results/figure_6.jpeg", plot = p1, bg = 'white', height = 290/2, width = 210, units = "mm")

## Diversity models
fit.0 = brm(diversity ~ strength, data = data)
fit.1 = brm(diversity ~ strength + (1|type), data = data)
fit.2 = brm(diversity ~ strength + (strength|type), data = data)

fixef(fit.0)
coef(fit.1)
coef(fit.2)

fit.0 = brm(diversity ~ centrality, data = data)
fit.1 = brm(diversity ~ centrality + (1|type), data = data)
fit.2 = brm(diversity ~ centrality + (centrality|type), data = data)

fixef(fit.0)
coef(fit.1)
coef(fit.2)

cols = c("#3a4a7b", "#57B5ED","#ED5C4D", "#FF9438", "#FBBE4B")

hum_names <- as_labeller(
  c(g0 = "Cousins + Siblings",
    g1 = "Parents + Parent's Siblings",
    g2 = "Grandparents",
    niblings = "Niblings",
    siblings = "Siblings"))

ggplot(data, aes(y = diversity, x = strength, col = type)) + 
  geom_point() + 
  scale_color_manual(values = cols) + 
  facet_wrap(~type, nrow = 2, labeller = hum_names) + 
  ylab("Diversity") + xlab("Strength") + 
  theme(legend.position = "none")

ggplot(data, aes(y = frequency, x = Freq, col = type)) + 
  geom_point() + 
  scale_color_manual(values = cols) + 
  facet_wrap(~type, nrow = 2, labeller = hum_names, scale = "free_y") + 
  ylab("Diversity") + xlab("Strength") + 
  theme(legend.position = "none")



