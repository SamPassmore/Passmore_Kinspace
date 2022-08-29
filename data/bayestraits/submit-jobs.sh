#!/bin/bash
#
#
#PBS -l nodes=1:ppn=1,walltime=09:00:00
#PBS -o results/bayestraits/logs/submit-output.txt
#PBS -e results/bayestraits/logs/submit-error.txt

# record some potentially useful details about the job:
echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo PBS job ID is $PBS_JOBID
echo This jobs runs on the following machines: echo `cat $PBS_NODEFILE | uniq`

# Define working directory 
export WORK_DIR=$HOME/kinspace2/

# Change into working directory 
cd $WORK_DIR

# make output folders
mkdir -p results/bayestraits/logs/


echo "Running jobs"
for f in data/bayestraits/jobs/*.sh
do
  echo $f
  qsub $f
  sleep 20
done
