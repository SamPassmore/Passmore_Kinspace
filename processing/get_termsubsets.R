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

### Read in CLDF
languages = read.csv('kinbank/kinbank/cldf/languages.csv', stringsAsFactors = FALSE)
parameters = read.csv('kinbank/kinbank/cldf/parameters.csv', stringsAsFactors = FALSE)
terms = read.csv('kinbank/kinbank/cldf/forms.csv', na.strings = c("0", "", NULL, "NULL"), stringsAsFactors = FALSE)
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

### Data subsets
## Siblings
sibling_columns = str_detect(colnames(terms_w), "^(m|f)(e|y)(B|Z)$|Glottocode")
#colnames(terms_w)[sibling_columns]
siblings = terms_w[,sibling_columns] %>% filter(complete.cases(.))
#dim(siblings)
sib_test = all(colnames(siblings) %in% c("feB", "feZ", "fyB", "fyZ", "meB", "meZ", "myB", "myZ", "Glottocode"))

write.csv(siblings, 'data/terms/sibling_terms.csv', row.names = FALSE, quote = FALSE, fileEncoding = 'utf8')

## Cousins
### For cousins we deliberately exclude terms for languages where
### age of connecting relative (e.g. FeBD) because this distinction
### is rarely used, and difficult to incorporate into the analysis
### because it is not a subsidary of FBD. 
g0_columns = str_detect(colnames(terms_w), "^(m|f)(F|M)(B|Z)(e|y)(S|D)$|^(m|f)(e|y)(B|Z)$|Glottocode")
g0 = terms_w[,g0_columns] %>% filter(complete.cases(.))
write.csv(g0, 'data/terms/g0_terms.csv', row.names = FALSE, quote = FALSE, fileEncoding = 'utf8')
g0_test = ncol(g0) == 40 + 1 # 20 relatives, sex of speaker doubles the number, + 1 for language code

## G+1 [parent's generation]
g1_columns = str_detect(colnames(terms_w), "^(m|f)(F|M)(e|y)(B|Z)$|^(m|f)(F|M)$|Glottocode")
g1 = terms_w[,g1_columns] %>% filter(complete.cases(.))
write.csv(g1, 'data/terms/g1_terms.csv', row.names = FALSE, quote = FALSE, fileEncoding = 'utf8')
g1_test = ncol(g1) == 21 # 4 Parents siblings * 2 for e/y, + M & F, * 2 for Sex of speaker, + 1 for language id

## G+2 [grandparent's generation]
g2_columns = str_detect(colnames(terms_w), "^(m|f)(F|M)(F|M)$|Glottocode")
#colnames(terms_w)[g2_columns]
g2 = terms_w[,g2_columns] %>% filter(complete.cases(.))
#dim(g2)
write.csv(g2, 'data/terms/g2_terms.csv', row.names = FALSE, quote = FALSE, fileEncoding = 'utf8')
g2_test = ncol(g2) == 9

## G-1 [niblings and children]
nibling_columns = str_detect(colnames(terms_w), "^(m|f)(e|y)(B|Z)(S|D)$|^(m|f)(S|D)$|Glottocode")
niblings = terms_w[,nibling_columns] %>% filter(complete.cases(.))
write.csv(niblings, 'data/terms/nibling_terms.csv', row.names = FALSE, quote = FALSE, fileEncoding = 'utf8')
niblings_test = all(colnames(niblings) %in% c("feBS", "fyBS", "feBD", "fyBD", "feZS", "fyZS", "feZD", "fyZD", "fS", "fD", "meBS", "myBS", "meBD", "myBD", "meZS", "myZS", "meZD", "myZD", "mS", "mD", "Glottocode"))

cat('Siblings:\n # Languages:', nrow(siblings), '\n # Columns:', ncol(siblings), '\n Pass:', sib_test, "\n")
cat('G0:\n # Languages:', nrow(g0), '\n # Columns:', ncol(g0), '\n Pass:', g0_test, "\n")
cat('G1:\n # Languages:', nrow(g1), '\n # Columns:', ncol(g1), '\n Pass:', g1_test, "\n")
cat('G2:\n # Languages:', nrow(g2), '\n # Columns:', ncol(g2), '\n Pass:', g2_test, "\n")
cat('Niblings:\n # Languages:', nrow(niblings), '\n # Columns:', ncol(niblings), '\n Pass:', niblings_test, "\n")
