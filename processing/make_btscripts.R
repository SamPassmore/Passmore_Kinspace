library(stringr)
library(bayestraitr)
library(readxl)
library(ggdag)
library(dagitty)

kintypes = c("siblings", "niblings", "g0", "g1", "g2")
language_families = c("an", "bt", "ie")

for(kintype in kintypes){
  for(lf in language_families){
    
    # dg = suppressMessages(read_xlsx(
    #   paste0('results/type-descriptions/', kintype, '.xlsx'), 
    #   sheet = "dag", col_names = FALSE))
    
    edges = read_xlsx('data/edges.xlsx', sheet = kintype, na = "NA")
    edges = edges[!is.na(edges$rule),]
    
    # edges = edges(dag)
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

    
string = str_interp(
"#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=90:00:00
#PBS -o results/${kintype}/bayestraits/${kintype}-${lf}-output.txt
#PBS -e results/${kintype}/bayestraits/${kintype}-${lf}-error.txt

# Define working directory
export WORK_DIR=$HOME/kinspace/code
# Change into working directory
cd $WORK_DIR

# record some potentially useful details about the job:
# echo Running on host `hostname`
# echo Time is `date`
# echo Directory is `pwd`
# echo PBS job ID is $PBS_JOBID
# echo This jobs runs on the following machines: echo `cat $PBS_NODEFILE | uniq`

# module load apps/bayestraits-v3-passmore

datetime=$(date +%d-%b-%Y-%H_%M)

output=$(basename $data)

HOME=$(pwd)

# Ancestral state
../../BayesTraitsV4.0.0-OSX/BayesTraitsV4 ${kintype}_${lf}.bttrees ${kintype}_${lf}.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 100000
Sample 500
LogFile ../../results/${kintype}_${lf}_$output_$datetime
run
ANSWERS
")    
    
    writeLines(string, paste0("data/bayestraits/script_", lf, "_", kintype, ".sh"))
  }
}
