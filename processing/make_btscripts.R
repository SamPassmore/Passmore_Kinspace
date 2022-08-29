library(stringr)
library(bayestraitr)
library(readxl)
library(ggdag)
library(dagitty)

kintypes = c("siblings", "niblings", "g0", "g1", "g2")
language_families = c("an", "bt", "ie")

for(kintype in kintypes){
  for(lf in language_families){
    
    dg = suppressMessages(read_xlsx(
      paste0('results/type-descriptions/', kintype, '.xlsx'), 
      sheet = "dag", col_names = FALSE))
    
    dag = dagitty(unlist(dg[2]))
    edges = edges(dag)
    edges$v = str_extract(edges$v, "[0-9]")
    edges$w = str_extract(edges$w, "[0-9]")
    
    new_dag_str = paste0("dag{",
                     paste(
                     paste0(edges$v, edges$e, edges$w)
                     , collapse = ";"),
                     "}")
    
    new_dag = dagitty(new_dag_str)
    
    model_string = bayestraitr::bt_addmodel(new_dag)
    
    string = str_interp("
      #!/bin/bash
      #
      #
      #PBS -l nodes=1:ppn=1,walltime=90:00:00
      #PBS -o results/{kintype}/bayestraits/{kintype}-{lf}-output.txt
      #PBS -e results/{kintype}/bayestraits/{kintype}-{lf}-error.txt
      
      # Define working directory
      export WORK_DIR\=\$HOME\/kinspace\/code
      # Change into working directory
      cd $WORK_DIR
      
      # record some potentially useful details about the job:
      echo Running on host `hostname`
      echo Time is `date`
      echo Directory is `pwd`
      echo PBS job ID is $PBS_JOBID
      echo This jobs runs on the following machines: echo `cat $PBS_NODEFILE | uniq`
      
      module load apps/bayestraits-v3-passmore
      
      datetime=$(date +%d-%b-%Y-%H_%M)
      
      output=$(basename $data)
      
      HOME=$(pwd)
      
      # Ancestral state
      BayesTraitsV3Passmore $HOME/data/bayestraits-input/{kintype}_{lf}.bttrees $HOME/data/bayestraits-input/{kintype}_{lf}.btdata << ANSWERS
      1
      2
      NQM
      ScaleTrees
      Stones 100 1000
      RevJump exp 10
      Iterations 10000000
      Sample 500
      LogFile $HOME/results/{kintype}/bayestraits/{kintype}_{lf}_$output_$datetime
      run
      ANSWERS
    ")    
  }
}
