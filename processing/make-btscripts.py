from pandas import read_csv

kintypes = ["siblings", "niblings", "g1", "g2", "g0"]
language_families = ["an", "bt", "ie"]

for i in [1,2,3]:
  for kintype in kintypes:
    modelstring = read_csv('data/bayestraits/modelstrings/'+kintype+'.txt', header=None)
    modelstring = modelstring.iloc[0,0]
    for lf in language_families:
      string = """#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=90:00:00
#PBS -o results/bayestraits/logs/{kintype}_{lf}{i}-output.txt
#PBS -e results/bayestraits/logs/{kintype}_{lf}{i}-error.txt

# Define working directory
export WORK_DIR=$HOME/kinspace2
# Change into working directory
cd $WORK_DIR

# record some potentially useful details about the job:
echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo PBS job ID is $PBS_JOBID
echo This jobs runs on the following machines: echo `cat $PBS_NODEFILE | uniq`

module load apps/bayestraits-v3-passmore

HOME=$(pwd)

# Ancestral state
BayesTraitsV3Passmore $HOME/data/bayestraits/{kintype}_{lf}.bttrees $HOME/data/bayestraits/{kintype}_{lf}.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
LogFile $HOME/results/bayestraits/{i}/{kintype}_{lf}
run

BayesTraitsV3Passmore $HOME/data/bayestraits/{kintype}_{lf}.bttrees $HOME/data/bayestraits/{kintype}_{lf}.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
{model_string}
LogFile $HOME/results/bayestraits/{i}/{kintype}_{lf}_restricted
run
ANSWERS
      """.format(**{"kintype": kintype, "lf": lf, "model_string": modelstring, "i": i})

      with open("data/bayestraits/jobs/"+kintype+"_"+lf+str(i)+".sh", "w") as text_file:
        text_file.write(string)

