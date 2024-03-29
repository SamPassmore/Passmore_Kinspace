# /usr/bin/Rscript

suppressMessages(library(stringr))
suppressMessages(library(bayestraitr))
suppressMessages(library(readxl))
suppressMessages(library(ggdag))
suppressMessages(library(dagitty))

kintypes = c("siblings", "niblings", "g0", "g1", "g2")

for(kintype in kintypes){
  cat('Create', kintype, '...\n')  
  
  dg = suppressMessages(read_xlsx(
      paste0('data/type-descriptions/', kintype, '.xlsx'), 
      sheet = "dag", col_names = FALSE))
  
  edges = read_xlsx('data/edges.xlsx', sheet = kintype, na = "NA")
  edges = edges[!is.na(edges$rule),]
  
  edges = edges(dag)
  edges$from = str_extract(edges$from, "^[A-Z]{1}")
  edges$to = str_extract(edges$to, "^[A-Z]{1}")
    
  
  new_dag_str = paste0("dag{",
                     paste(
                     paste0(edges$from, "->", edges$to)
                     , collapse = ";"),
                     paste(
                       paste0(edges$to, "->", edges$from)
                       , collapse = ";"),
                     "}")
    
    new_dag = dagitty(new_dag_str)
    
    model_string = as.character(bayestraitr::bt_addmodel(new_dag))
    write.table(model_string, file = paste0("data/bayestraits/modelstrings/", kintype, ".txt"),
                quote = TRUE, col.names = FALSE, row.names = FALSE)
}

x = read.csv('data/bayestraits/siblings_an.btdata', sep = "\t", header = FALSE)
    