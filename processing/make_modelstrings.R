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
      paste0('results/type-descriptions/', kintype, '.xlsx'), 
      sheet = "dag", col_names = FALSE))
    
    dag = dagitty(unlist(dg[2]))
    edges = edges(dag)
    edges$v = str_extract(edges$v, "[0-9]")
    edges$w = str_extract(edges$w, "[0-9]")
    
    # convert to letters to match data change in get_btdata
    edges$v = letters[as.numeric(edges$v) + 1]
    edges$w = letters[as.numeric(edges$w) + 1]
    
    new_dag_str = paste0("dag{",
                     paste(
                     paste0(edges$v, edges$e, edges$w)
                     , collapse = ";"),
                     "}")
    
    new_dag = dagitty(new_dag_str)
    
    model_string = as.character(bayestraitr::bt_addmodel(new_dag))
    write.table(model_string, file = paste0("data/bayestraits/modelstrings/", kintype, ".txt"),
                quote = TRUE, col.names = FALSE, row.names = FALSE)
}
    