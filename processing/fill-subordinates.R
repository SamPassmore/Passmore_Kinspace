getHierarchicalRelations = function(terms_w){
  hr = c("a", "a")
  for(i in 1:length(colnames(terms_w)[2:580])){
    v = colnames(terms_w)[i+1]
    # Strip the relative-age marker (e = elder, y = younger) from the final
    # kin abbreviation, e.g. "mFeB" -> "mFB", to find the parent (base) term.
    new_v = stringr::str_remove(v,  "(?<=(m|f)[A-Z]{1,4})(e|y)")
    hr = rbind(hr, c(v, new_v))
  }
  # Remove rows where the term has no age marker (child == parent after stripping)
  hr = hr[hr[,1] != hr[,2],]

  # Remove rows where the parent term doesn't exist as a column in the data
  hr = hr[hr[,2] %in% colnames(terms_w),]

  hr
}

# test
# stringr::str_remove("mFeB", "(?<=(m|f)[A-Z]{1,4})(e|y)")
# stringr::str_remove("mFFeB", "(?<=(m|f)[A-Z]{1,4})(e|y)")

fillAllSubordinates = function(terms_w, hr){
  for(i in 1:nrow(hr)){
    ss = as.matrix(terms_w[,hr[i,]])
    # If the specific (age-marked) term is missing, inherit from the base term
    ss[is.na(ss[,1]),1] = as.character(ss[is.na(ss[,1]),2])
    terms_w[,hr[i,1]] = ss[,1]
  }
  terms_w
}

# test
# d = data.frame(fFBeS = c("a", NA, NA, "c"), fFBS = c("a", "b", NA, NA))
# hr = matrix(c("fFBeS", "fFBS"), ncol = 2)
# fillAllSubordinates(d, hr)
