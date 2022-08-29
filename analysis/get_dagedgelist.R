# /usr/bin/Rscript

## get edge list for dags

library(readxl)
library(ggdag)

subsets = c("siblings", "niblings", "g0", "g1", "g2")

for(s in subsets){
  type = s
  dg = suppressMessages(read_xlsx(
    paste0('results/type-descriptions/', type, '.xlsx'), 
    sheet = "dag", col_names = FALSE))
  
  dag = tidy_dagitty(unlist(dg[2]))
  edges = data.frame(from = dag$data$name, to = dag$data$to, stringsAsFactors = FALSE)
  
  write.csv(edges, file = 
              paste0('results/global/networks/vertices/', type, '.csv'), 
            row.names = FALSE)
  
}
