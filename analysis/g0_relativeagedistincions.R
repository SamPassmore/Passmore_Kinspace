## here we are checking how many languages use relative age of connecting relative

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

### Read in CLDF
languages = read.csv('raw/kinbank/kinbank/cldf/languages.csv', stringsAsFactors = FALSE)
parameters = read.csv('raw/kinbank/kinbank/cldf/parameters.csv', stringsAsFactors = FALSE)
terms = read.csv('raw/kinbank/kinbank/cldf/forms.csv', na.strings = c("0", "", NULL, "NULL"), stringsAsFactors = FALSE)
### Join CLDF into a single DF
terms = left_join(terms, parameters, by = c("Parameter_ID" = "ID"), suffix = c(".terms", ".parameter")) %>% 
  left_join(., languages, by = c("Language_ID" = "ID"), suffix = c(".parameters", ".language"))

## Take the first occurance of each kinterm type
terms = terms %>% 
  group_by(Language_ID, Parameter_ID) %>% 
  slice(1) %>% 
  ungroup()

## Wide format
## convert this database to have one row per language and a column per parameter
#terms_w = tidyr::spread(terms[,c("Glottocode", "Name.parameters", "Form")], key = "Name.parameters", value = "Form", )
terms_w = tidyr::spread(terms[,c("Language_ID", "Parameter", "Form")], key = "Parameter", value = "Form")
terms_w = apply(terms_w, 2, clean_kinterms) %>% as.data.frame(.)
terms_w$Glottocode = stringr::str_extract(terms_w$Language_ID, "[a-z]{4}[0-9]{4}[a-z]?$")
#dim(terms_w)

# reduce to a single glottocode
terms_w = terms_w %>% 
  group_by(Glottocode) %>% 
  slice(1) %>% # take the first occurance of each glottocode
  ungroup()
dim(terms_w)
#colnames(terms_w)

# fill in subsidary terms
hr = getHierarchicalRelations(terms_w)
terms_w = fillAllSubordinates(terms_w, hr)

## remove languages that don't fit with our analysis plan
## IDS languages
## Sign-Languages: aust1271. remove because of complications

# IDS languages to remove (because IDS does not have satisfactory cousin term categorisation)
ids = read.csv('data/ids-subset.csv', stringsAsFactors = FALSE) %>% 
  filter(can_use == FALSE) %>% 
  pull(Language_ID) %>% 
  str_extract("[a-z]{4}[0-9]{4}")

terms_w = terms_w %>% 
  filter(!Glottocode %in% c(ids, "aust1271"))


g0_columns = str_detect(colnames(terms_w), "^(m|f)(F|M)(e|y)(B|Z)(S|D)$|^(m|f)(e|y)(B|Z)$|Glottocode")
g0 = terms_w[,g0_columns] %>% filter(complete.cases(.))

elder_idx = str_detect(colnames(g0), "^(m|f)(F|M)(e)(B|Z)(S|D)$")
younger_idx = str_detect(colnames(g0), "^(m|f)(F|M)(y)(B|Z)(S|D)$")

elder_terms = g0[,elder_idx]
younger_terms = g0[,younger_idx]

different = list()
for(i in 1:ncol(elder_terms)){
  idx = elder_terms[,i] != younger_terms[,i] 
  glottocode = g0$Glottocode[idx]
  different[[i]] = glottocode  
}

length(unique(unlist(different))) / nrow(g0)
