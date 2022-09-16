
# law of collaterality 

library(plyr)

# grandparents
grandparents = read.csv('data/matrix/g2_matrix.csv')

any(grandparents$mFMmFF == 1 & grandparents$mMFmFF == 1 & grandparents$mMFmFF == 0)

freq = ddply(grandparents[,-ncol(grandparents)], colnames(grandparents)[-ncol(grandparents)], nrow) %>% 
  arrange(desc(V1))
nrow(freq)
numbers::bell(4)


## g1
g1 = read.csv('data/matrix/g1_matrix.csv')

any(g1$mMeBmF == 1 & g1$mMeBmFeB == 0 & g1$mFeBmF == 0)
idx = which(g1$mMeBmF == 1 & g1$mMeBmFeB == 0 & g1$mFeBmF == 0)
g1$Glottocode[idx]

idx = which(g1$fMeBfF == 1 & g1$fMeBfFeB == 0 & g1$fFeBfF == 0)
g1$Glottocode[idx]

idx = which(g1$mMeZmM == 1 & g1$mMeZmFeZ == 0 & g1$mFeZmM == 0)
g1$Glottocode[idx]

idx = which(g1$fMeZfM == 1 & g1$fMeZfFeZ == 0 & g1$fFeZfM == 0)
g1$Glottocode[idx]


