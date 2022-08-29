#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=90:00:00
#PBS -o results/bayestraits/logs/g2_ie2-output.txt
#PBS -e results/bayestraits/logs/g2_ie2-error.txt

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
BayesTraitsV3Passmore $HOME/data/bayestraits/g2_ie.bttrees $HOME/data/bayestraits/g2_ie.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
LogFile $HOME/results/bayestraits/2/g2_ie
run

BayesTraitsV3Passmore $HOME/data/bayestraits/g2_ie.bttrees $HOME/data/bayestraits/g2_ie.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
Restrict qba 0
Restrict qca 0
Restrict qda 0
Restrict qea 0
Restrict qfa 0
Restrict qcb 0
Restrict qdb 0
Restrict qeb 0
Restrict qfb 0
Restrict qdc 0
Restrict qec 0
Restrict qfc 0
Restrict qbd 0
Restrict qcd 0
Restrict qed 0
Restrict qfd 0
Restrict qae 0
Restrict qce 0
Restrict qde 0
Restrict qfe 0
Restrict qaf 0
Restrict qbf 0
LogFile $HOME/results/bayestraits/2/g2_ie_restricted
run
ANSWERS
      