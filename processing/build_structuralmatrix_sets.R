# /usr/bin/Rscript

### This R script will create the datasets used from kinbank
### It takes the CLDF data from the kinbank dataset 
### merges the language metadata, parameter metadata, and data for the terms
### into a single dataframe. Then converts the data into a wide matrix where
### each language is a row, and each column is a kin relationship
### Some languages have more than one kinterm per relationship, but for this 
### analysis we can only allow one term - so we select the first term in each case.

### Then we subset the columns to the appropriate set for each kinterm subset
suppressMessages(library(dplyr))
suppressMessages(library(stringr))

source("processing/fill-subordinates.R")

## functions
clean_kinterms = function(kinterm){
  kinterm = tolower(kinterm)
  ## if terms are in brackets - assume that is an alternative term and remove it
  kinterm = stringr::str_remove_all(kinterm, "\\(.*\\)")
  ## if terms are seperated by semi-colons or commas, take everything before them
  kinterm = gsub("^(.*?)(,|;).*", "\\1", kinterm)
  ## remove whitespace
  kinterm = stringi::stri_replace_all_charclass(kinterm, "\\p{WHITE_SPACE}", "")
  kinterm
}

## function test
# clean_kinterms("brother(bro)")
# clean_kinterms("brother;bro")
# clean_kinterms("bro   ther")
# clean_kinterms("(bro)brother   ;bro")
# clean_kinterms("(bro)brother   ;bro)") ## this fails

compare_set = function(x, y){
  intsct = intersect(x, y)
  value = ifelse(length(intsct) == 0, 0, 1)
  c(value)
}

# x = letters[1:3]
# y = letters[3:7]
# z = letters[20]
# 
# compare_set(x, y)
# compare_set(x, z)

compareset_wrapper = function(dx, dy){
  compare_set(dx[[1]]$Form, dy[[1]]$Form)
}

getHierarchicalRelations = function(parameters){
  hr = c("a", "a")
  for(i in 1:length(parameters)){
    v = parameters[i+1]
    new_v = stringr::str_remove(v,  "(?<=(m|f)[A-Z]{1,4})(e|y)")
    hr = rbind(hr, c(v, new_v))
  }
  # if there is no subsidary remove it
  hr = hr[hr[,1] != hr[,2],]
  
  # if the subsidary doesn't exist remove it
  hr = hr[hr[,2] %in% parameters,]
  
  hr  
}

# test 
# stringr::str_remove("mFeB", "(?<=(m|f)[A-Z]{1,4})(e|y)")
# stringr::str_remove("mFFeB", "(?<=(m|f)[A-Z]{1,4})(e|y)")

fillAllSubordinates = function(t, hr){
  for(i in 1:nrow(hr)){
    ss = t[hr[i,2] == t$Parameter_ID,]
    
    ss$Parameter_ID = hr[i,1]
  
    t = rbind(t, ss)
  }
  t
}

fillAllSubordinates_wrapper = function(t, hr){
  glottocodes = unique(t$Glottocode)
  for(gc in glottocodes){
    language_terms = t[t$Glottocode == gc,]
    language_terms = fillAllSubordinates(language_terms, hr)
  }
}


### Read in CLDF
languages = read.csv('kinbank/kinbank/cldf/languages.csv', stringsAsFactors = FALSE)
parameters = read.csv('kinbank/kinbank/cldf/parameters.csv', stringsAsFactors = FALSE)
terms = read.csv('kinbank/kinbank/cldf/forms.csv', na.strings = c("0", "", NULL, "NULL"), stringsAsFactors = FALSE)

### Join CLDF into a single DF
terms = left_join(terms, parameters, by = c("Parameter_ID" = "ID"), suffix = c(".terms", ".parameter")) %>% 
  left_join(., languages, by = c("Language_ID" = "ID"), suffix = c(".parameters", ".language"))

hr    = getHierarchicalRelations(unique(terms$Parameter_ID))
terms = fillAllSubordinates(terms, hr)

# kin_type = c("feB", "feZ", "fyB", "fyZ", "meB", "meZ", "myB", "myZ")
# kin_type = c("fD", "feBD", "feBS", "feZD", "feZS", "fS", "fyBD",
#              "fyBS", "fyZD", "fyZS", "mD", "meBD", "meBS", "meZD", "meZS", 
#              "mS", "myBD", "myBS", "myZD", "myZS")
# kin_type = c("feB", "feZ", "fFBeD", "fFBeS", "fFByD", "fFByS", "fFZeD",
#              "fFZeS", "fFZyD", "fFZyS", "fMBeD", "fMBeS", "fMByD", "fMByS",
#              "fMZeD", "fMZeS", "fMZyD", "fMZyS", "fyB", "fyZ", "meB", "meZ",
#              "mFBeD", "mFBeS", "mFByD", "mFByS", "mFZeD", "mFZeS", "mFZyD",
#              "mFZyS", "mMBeD", "mMBeS", "mMByD", "mMByS", "mMZeD", "mMZeS",
#              "mMZyD", "mMZyS", "myB", "myZ")
# kin_type = c("mF", "mFeB", "mFeZ", "mFyB", "mFyZ", "mM", "mMeB", 
#   "mMeZ", "mMyB", "mMyZ")
kin_type = c("mFF", "mFM", "mMF", "mMM")

terms = terms %>% 
  dplyr::filter(Parameter_ID %in% kin_type)

glottocodes = unique(terms$Glottocode)
structural_matrix = matrix(NA, nrow = length(glottocodes), 
                           ncol = ((length(kin_type) * length(kin_type)) - length(kin_type)) / 2)
missing_terms = c()
for(i in 1:length(glottocodes)){
  d = terms %>% 
    dplyr::filter(Glottocode == glottocodes[i] & Parameter_ID %in% kin_type)
  
  d$Form = clean_kinterms(d$Form)
  
  if(all(kin_type %in%  d$Parameter_ID)){
    d = d[match(kin_type, d$Parameter_ID),]
    d_split = split(d, f = d$Parameter_ID)
    
    structual_vector = matrix(NA, nrow = length(kin_type), ncol = length(kin_type))
    for(x in 1:length(d_split)){
      for(y in 1:length(d_split)){
        structual_vector[x,y] = compareset_wrapper(d_split[x], d_split[y])
      }
    }
    structual_vector = structual_vector %>%
      .[lower.tri(.)] %>%
      c(.)
    # test = outer(1:length(d_split), 1:length(d_split), function(x, y) compareset_wrapper(d_split[x], d_split[y])) 
    
    structural_matrix[i,] = structual_vector
    
  } else {
    missing_terms = c(missing_terms, glottocodes[i])
  }
}

structural_matrix = as.data.frame(structural_matrix)
structural_matrix$Glottocode = glottocodes
