# /usr/bin/Rscript

### This script converts the term data into the structural vectors
### For each row in a term subset (which is a language) it will compare
### every term to every other term and convert it to a vector. 
### The comparison metric will be flexible in the function. 

suppressMessages(library(stringr))
suppressMessages(library(dplyr))

# parameters
method = "binary"

source('processing/helper.R')

sexofspeaker = function(x){
  ms_idx = str_detect(colnames(x), "m")
  ws_idx = str_detect(colnames(x), "f")
  mvs = x[,ms_idx] != x[,ws_idx]
  sos_present = rowSums(mvs) > 0
  lang_sos = x$Glottocode[sos_present]
  list(lang_sos, length(lang_sos)/ nrow(x))
}

### function check
# get_dist("aaaa", "aaaa", 4) == 0
# get_dist("aaaa", "bbbb", 4) == 1
# get_dist("bbbb", "aaaa", 4) == 1
# get_dist("aaaa", "aabb", 4) == 0.5
# 
# all(get_vector(c("aaaa", "aaaa", "aabb", "bbbb")) == c(0, 0.5, 1, 0.5, 1, 0.5))

## terms
### siblings
siblings = read.csv('data/terms/sibling_terms.csv', stringsAsFactors = FALSE)
sos_sib = sexofspeaker(siblings) # 11% of languages (varikin) - 28.8% overall
sibling_matrix = apply(siblings %>% select(-Glottocode), 1, get_vector, method = method) %>% t() %>% as.data.frame()
sibling_matrix$Glottocode = siblings$Glottocode
write.csv(sibling_matrix, 'data/matrix/siblings_matrix.csv', row.names = FALSE)

n = 8 # 8 kin types in siblings

cat("Siblings: \n Sex of Speaker:", round(sos_sib[[2]], 2), 
    "\n Keep SoS:", sos_sib[[2]] > 0.1, 
    "\n # Languages: ", nrow(sibling_matrix),
    "\n Pass: ", ncol(sibling_matrix) == (((n * n) / 2) - (n / 2)) + 1, "\n")

## Cousins
#cousins = read.csv('code/data/cousin_terms.csv', stringsAsFactors = FALSE)
## There is the automatic extraction of cousin terms above, but I also manually cleaned these terms
## in the file below - with sibling terms - which I then remove here so we only analyse cousin terms
#cousins = read.csv('code/data/cousin_terms_manualclean.csv', stringsAsFactors = FALSE)
cousins = read.csv('data/terms/g0_terms.csv', stringsAsFactors = FALSE)
# cousins = cousins[,str_detect(colnames(cousins), "^(m|f)(F|M)(B|Z)(e|y)(S|D)$|Glottocode")]
sos_g0 = sexofspeaker(cousins) # 12% of languages (varikin) - 18% overall
cousin_matrix = apply(cousins %>% select(-Glottocode), 1, get_vector, method = method) %>% t() %>% as.data.frame()
cousin_matrix$Glottocode = cousins$Glottocode
write.csv(cousin_matrix, 'data/matrix/g0_matrix.csv', row.names = FALSE)

n = 40 # kin types in cousins

cat("G0: \n Sex of Speaker:", round(sos_g0[[2]], 2), 
    "\n Keep SoS:", sos_g0[[2]] > 0.1, 
    "\n # Languages: ", nrow(cousin_matrix),
    "\n Pass: ", ncol(cousin_matrix) == (((n * n) / 2) - (n / 2)) + 1, "\n")

## G+1 [parent's generation]
g1 = read.csv('data/terms/g1_terms.csv', stringsAsFactors = FALSE)
sos_g1 = sexofspeaker(g1) # 3% varikin 5% overall
# Sex of speaker distinctions only occur in 3% of the languages 
# A manual check suggests that some of these are typos, 
# and where they are not there is no distinc pattern. 
# Therefore we remove terms for women speakers to reduce the feature space
ms_idx = str_detect(colnames(g1), "m|Glottocode")
g1 = g1[,ms_idx]
g1_matrix = apply(g1 %>% select(-Glottocode), 1, get_vector, method = method) %>% t() %>% as.data.frame()
g1_matrix$Glottocode = g1$Glottocode
write.csv(g1_matrix, 'data/matrix/g1_matrix.csv', row.names = FALSE)

n = 10 # kin types in cousins (usually 20, but we are not taking sex of speaker here)

cat("G+1: \n Sex of Speaker:", round(sos_g1[[2]], 2), 
    "\n Keep SoS:", sos_g1[[2]] > 0.1, 
    "\n # Languages: ", nrow(g1_matrix),
    "\n Pass: ", ncol(g1_matrix) == (((n * n) / 2) - (n / 2)) + 1, "\n")


## G+2 [grandparent's generation]
g2 = read.csv('data/terms/g2_terms.csv', stringsAsFactors = FALSE)
sos_g2 = sexofspeaker(g2) # 1% varikin 1.5% overall
# Sex of speaker only occurs in 1% of languages here
# A manual check suggests actually only two of these are real. 
# most are typos (fixed) so we remove ws to reduce the feature space
ms_idx = str_detect(colnames(g2), "m|Glottocode")
g2 = g2[,ms_idx]
g2_matrix = apply(g2 %>% select(-Glottocode), 1, get_vector, method = method) %>% t() %>% as.data.frame()
g2_matrix$Glottocode = g2$Glottocode
write.csv(g2_matrix, 'data/matrix/g2_matrix.csv', row.names = FALSE)

n = 4 # kin types in cousins

cat("G+2: \n Sex of Speaker:", round(sos_g2[[2]], 2), 
    "\n Keep SoS:", sos_g2[[2]] > 0.1, 
    "\n # Languages: ", nrow(g2_matrix),
    "\n Pass: ", ncol(g2_matrix) == (((n * n) / 2) - (n / 2)) + 1, "\n")


## G-1 [niblings and children]
niblings = read.csv('data/terms/nibling_terms.csv', stringsAsFactors = FALSE)
sos_nib = sexofspeaker(niblings) # occurs in 13% of languages so we will keep these - 18% overall
nibling_matrix = apply(niblings %>% select(-Glottocode), 1, get_vector, method = method) %>% t() %>% as.data.frame()
nibling_matrix$Glottocode = niblings$Glottocode
write.csv(nibling_matrix, 'data/matrix/niblings_matrix.csv', row.names = FALSE)

n = 12

cat("G-1: \n Sex of Speaker:", round(sos_nib[[2]], 2), 
    "\n Keep SoS:", sos_nib[[2]] > 0.1, 
    "\n # Languages: ", nrow(nibling_matrix),
    "\n Pass: ", ncol(nibling_matrix) == (((n * n) / 2) - (n / 2)) + 1, "\n")


# ## cousins vs siblings
# matching_languages = siblings$Glottocode[siblings$Glottocode %in% cousins$Glottocode]
# 
# cousinXsiblings = matrix(NA, nrow = length(matching_languages), ncol = 28)
# for(i in 1:length(matching_languages)){
#   glottocode = matching_languages[i]
#   cs = cousins[cousins$Glottocode == glottocode,2:33] %>% as.character(as.vector(.))
#   sb = siblings[siblings$Glottocode == glottocode,2:9] %>% as.character(as.vector(.))
#   
#   mc = max(nchar(c(cs, sb)))
#   
#   cousinXsiblings[i,] = outer(sb ,cs , get_dist, mc) %>% 
#     .[lower.tri(.)] %>% 
#     c(.)
# }
# dimnames(cousinXsiblings)
# write.csv(cousinXsiblings, 'code/data/sibXcousin_matrix.csv', row.names = FALSE)