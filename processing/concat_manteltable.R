
files = list.files('results/global/mantel/', "*.csv", full.names = TRUE)

d = purrr::map(files, read.csv) %>% 
  do.call(rbind, .) %>% 
  round(., 3) %>% 
  dplyr::select(-X)

rownames(d) = tools::file_path_sans_ext(files) %>% basename()

write.csv(d, 'results/global/mantel_table.csv')
