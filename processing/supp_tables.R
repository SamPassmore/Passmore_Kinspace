## supplementary tables
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(assertthat)
})

kin_subsets = c("siblings", "niblings", "g0", "g1", "g2")

for(k in kin_subsets){
  measures = read.csv(
    paste0('results/global/data/', k, ".csv")
  )  
  
  typology_description = read_xlsx("data/typology.xlsx",
                                   sheet = k)
  
  supp_table = left_join(measures, 
                         typology_description, 
                         by = c("label_" = "Cluster"))
  
  supp_table = 
    supp_table[,c("label_", "Kin Description", "frequency", 
                  "Modal Count", "diversity", "centrality", "strength", "silhouette")]
  
  supp_table[supp_table$label_ == "Outlier",c("Kin Description", "Modal Count", "diversity", "centrality", "strength", "silhouette")] = 
    NA

  colnames(supp_table) = c("Label", "Description", "Count", "Modal Count", "Diversity", "Centrality", "Strength",
                           "Silhouette")
    
  
  assert_that(all(supp_table$frequency > supp_table$`Mode Count`))
  
  filenames = paste0("results/supp_tables/", k, ".csv")
  write.csv(supp_table, 
            file = filenames,
            row.names = FALSE, 
            na = "-")
}
