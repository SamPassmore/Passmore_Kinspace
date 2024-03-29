suppressMessages(library(bayestraitr))
suppressMessages(library(ape))
suppressMessages(library(dplyr))
suppressMessages(library(geiger))

set.seed(1990)

out = "data/bayestraits/"

## phylogeneies
n_trees = 100
indoeuropean = read.nexus('dplace-data/phylogenies/bouckaert_et_al2012/posterior.trees')[sample(1:1000, size = n_trees)]
austronesian = read.nexus('dplace-data//phylogenies/gray_et_al2009/posterior.trees')[sample(1:1000, size = n_trees)]
bantu = read.nexus('dplace-data/phylogenies/grollemund_et_al2015/posterior.trees')[sample(1:1000, size = n_trees)]

# # fix bantu error
# bantu = lapply(bantu, function(phy){ 
#   s = phy$edge[, 2] <= Ntip(phy)
#   phy$edge[s, 2] <- phy$edge[s, 2] + 1L
#   phy
# })


## standardize the branch lengths to 1 = 1000 years

# ie is 1 = 1 year. Divide by 1000 to get 1 = 1000years
indoeuropean = lapply(indoeuropean, function(t){
  t$edge.length = t$edge.length / 1000
  t
  })

## austronesian is already 1 = 1000 years

## bantu is 1 = 1 year. Divide by 1000 to get 1 = 1000 years
bantu = lapply(bantu, function(t){
  t$edge.length = t$edge.length / 1000
  t 
})

## glottolog to phylogeny link files
indoeuropean_link = read.csv('dplace-data/phylogenies/bouckaert_et_al2012/taxa.csv')
austronesian_link = read.csv('dplace-data/phylogenies/gray_et_al2009/taxa.csv')
bantu_link = read.csv('dplace-data/phylogenies/grollemund_et_al2015/taxa.csv')

# how many trees in each case
cat("Indo-European trees: ", length(indoeuropean), "\n")
cat("Austronesian trees: ", length(austronesian), "\n")
cat("Bantu trees: ", length(bantu), "\n\n")

## data & tree subsets
get_links = function(d, links){
  links = lapply(links, function(x) inner_join(d, x, "glottocode"))
  links = lapply(links, function(l) l[!duplicated(l$glottocode),])
  links = lapply(links, function(x){
    rownames(x) = x$taxon
    x
  })
  links
}

get_subsets = function(type){
  # subset and save data
  # kinterms = read.csv(
  #   paste0('results/hdbscan/', type, '.csv')
  # )
  
  kinterms = read.csv('results/kinbank_wclusters.csv')
  
  kinterms$label_ = kinterms[,type]
  kinterms = kinterms[!is.na(kinterms$label_),]
  
  # glottocode needs to be lowercase
  colnames(kinterms)[3] = "glottocode"
  
  # change outliers to missing values & ensure labels are 1 indexed (BT does not like 0 as a category)
  # kinterms$label_ = ifelse(kinterms$label_ == -1, NA, kinterms$label_ + 1)
  # # since most kin-sets have more than 10 categories, we need to use letters to work with BT
  # kinterms$alpha_label = letters[kinterms$label_]
  
  # get subsets for each languages family
  kinterm_subsets = get_links(kinterms, list(indoeuropean_link, austronesian_link, bantu_link))
  kinterm_subsets = lapply(kinterm_subsets, function(x){
    x$label_ = ifelse(x$label_ == "Outlier", NA, x$label_)
    x
  } )
  
  # print languages per subset
  cat(type, "\n")
  n_lang = lapply(kinterm_subsets, nrow)
  cat("Indo-European languages", (n_lang[[1]]), "\n")
  cat("Austronesian languages", (n_lang[[2]]), "\n")
  cat("Bantu languages", (n_lang[[3]]), "\n")
  
  
  
  # save files
  bt_write(indoeuropean, kinterm_subsets[[1]], "label_", dir = out, filename = paste0(type, "_ie"))
  bt_write(austronesian, kinterm_subsets[[2]], 'label_', dir = out, filename = paste0(type, "_an"))
  # bt_write(bantu, kinterm_subsets[[3]], 'label_', dir = out, filename = paste0(type, "_bt")) 
}

get_subsets("siblings")
get_subsets("niblings")
get_subsets("g2")
get_subsets("g1")
get_subsets("g0")
