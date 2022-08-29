#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=90:00:00
#PBS -o results/bayestraits/logs/g1_an1-output.txt
#PBS -e results/bayestraits/logs/g1_an1-error.txt

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
BayesTraitsV3Passmore $HOME/data/bayestraits/g1_an.bttrees $HOME/data/bayestraits/g1_an.btdata << ANSWERS
1
2
NQM
ScaleTrees
Stones 100 1000
RevJump exp 10
Iterations 10000000
Sample 500
LogFile $HOME/results/bayestraits/1/g1_an
run

BayesTraitsV3Passmore $HOME/data/bayestraits/g1_an.bttrees $HOME/data/bayestraits/g1_an.btdata << ANSWERS
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
Restrict qea 0
Restrict qfa 0
Restrict qha 0
Restrict qia 0
Restrict qja 0
Restrict qga 0
Restrict qab 0
Restrict qcb 0
Restrict qdb 0
Restrict qeb 0
Restrict qfb 0
Restrict qhb 0
Restrict qib 0
Restrict qgb 0
Restrict qac 0
Restrict qdc 0
Restrict qec 0
Restrict qfc 0
Restrict qhc 0
Restrict qic 0
Restrict qjc 0
Restrict qgc 0
Restrict qad 0
Restrict qcd 0
Restrict qed 0
Restrict qfd 0
Restrict qhd 0
Restrict qid 0
Restrict qjd 0
Restrict qgd 0
Restrict qae 0
Restrict qbe 0
Restrict qce 0
Restrict qfe 0
Restrict qhe 0
Restrict qie 0
Restrict qje 0
Restrict qge 0
Restrict qaf 0
Restrict qbf 0
Restrict qcf 0
Restrict qdf 0
Restrict qef 0
Restrict qif 0
Restrict qjf 0
Restrict qgf 0
Restrict qah 0
Restrict qbh 0
Restrict qdh 0
Restrict qeh 0
Restrict qfh 0
Restrict qih 0
Restrict qjh 0
Restrict qgh 0
Restrict qai 0
Restrict qbi 0
Restrict qdi 0
Restrict qei 0
Restrict qfi 0
Restrict qji 0
Restrict qgi 0
Restrict qbj 0
Restrict qcj 0
Restrict qdj 0
Restrict qhj 0
Restrict qgj 0
Restrict qag 0
Restrict qbg 0
Restrict qcg 0
Restrict qeg 0
Restrict qfg 0
Restrict qhg 0
Restrict qig 0
Restrict qjg 0
LogFile $HOME/results/bayestraits/1/g1_an_restricted
run
ANSWERS
      