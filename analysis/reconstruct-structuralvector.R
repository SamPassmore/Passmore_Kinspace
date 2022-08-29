x = c("a", "a", "a", "a")

m = outer(x, x, '==')


## get from V back to x without using m
v = m[lower.tri(m)]



# this might onlt work for  vectors length 4
reconstruction = function(v){  
  x = floor(1 + sqrt(1 + 8 * length(v)) / 2)
  
  m2 = matrix(NA, ncol = x, nrow = x)
  m2[lower.tri(m2)] = v
  
  # does the lower triangle match?
  #all(m2[lower.tri(m2)] == m[lower.tri(m)])
  
  # recreate vector structure
  sv = vector(length = ncol(m2), mode = "character")
  for(i in 1:ncol(m2)){
    value = letters[i]
    idx = which(as.matrix(m2[,i]) == TRUE, arr.ind = TRUE)
    sv[idx] = value 
  }
  nf = length(unique(sv[sv != ""])) # number of values filled
  sv[sv == ""] = letters[(1+nf):(length(sv[sv==""])+nf)]
  sv
}

reconstruction(v)
