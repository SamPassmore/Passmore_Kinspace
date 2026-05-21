## Make a table of language distributions
library(dplyr)

l = read.csv('https://raw.githubusercontent.com/kinbank/kinbank/master/kinbank/cldf/languages.csv')
glottolog = read.csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/csv/glottolog.csv")

languages = left_join(l, glottolog, by = c("Glottocode" = "id"))

family = languages %>% 
  group_by(Family) %>%
  summarise(n_languages = n()) %>% 
  arrange(desc(n_languages)) %>% 
  filter(Family != "") %>% 
  top_n(5)

macroarea = languages %>% 
  group_by(Macroarea) %>%
  summarise(n_languages = n()) %>% 
  arrange(desc(n_languages)) %>% 
  top_n(5)

xx = cbind(family, macroarea)
write.csv(xx, "results/languagedistribution_table.csv")
