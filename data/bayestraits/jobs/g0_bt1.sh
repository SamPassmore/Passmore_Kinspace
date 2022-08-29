#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=90:00:00
#PBS -o results/bayestraits/logs/g0_bt1-output.txt
#PBS -e results/bayestraits/logs/g0_bt1-error.txt

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
BayesTraitsV3Passmore $HOME/data/bayestraits/g0_bt.bttrees $HOME/data/bayestraits/g0_bt.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
LogFile $HOME/results/bayestraits/1/g0_bt
run

BayesTraitsV3Passmore $HOME/data/bayestraits/g0_bt.bttrees $HOME/data/bayestraits/g0_bt.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
Restrict qhc 0
Restrict qac 0
Restrict qbc 0
Restrict qdc 0
Restrict qfc 0
Restrict qgc 0
Restrict qce 0
Restrict qhe 0
Restrict qae 0
Restrict qbe 0
Restrict qde 0
Restrict qfe 0
Restrict qge 0
Restrict qeh 0
Restrict qah 0
Restrict qbh 0
Restrict qdh 0
Restrict qfh 0
Restrict qgh 0
Restrict qea 0
Restrict qha 0
Restrict qba 0
Restrict qda 0
Restrict qfa 0
Restrict qga 0
Restrict qeb 0
Restrict qhb 0
Restrict qab 0
Restrict qdb 0
Restrict qfb 0
Restrict qgb 0
Restrict qcd 0
Restrict qhd 0
Restrict qad 0
Restrict qbd 0
Restrict qfd 0
Restrict qgd 0
Restrict qcf 0
Restrict qhf 0
Restrict qaf 0
Restrict qbf 0
Restrict qdf 0
Restrict qgf 0
Restrict qcg 0
Restrict qeg 0
Restrict qag 0
Restrict qbg 0
Restrict qdg 0
Restrict qfg 0
LogFile $HOME/results/bayestraits/1/g0_bt_restricted
run
ANSWERS
      