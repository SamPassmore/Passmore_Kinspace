
string_df = read.csv("data/matrix/siblings_matrix.csv") #

pairs = data.frame(t(combn(string_df$Glottocode, 2, c)))
pairs$jac = NA
pairs$ham = NA

for(i in 1:nrow(pairs)){
  p = unlist(pairs[i,])
  x = string_df[string_df$Glottocode == p[1], 1:(ncol(string_df)-1)]
  y = string_df[string_df$Glottocode == p[2], 1:(ncol(string_df)-1)]
  jac = sum(x == 1 & y == 1)/sum(x == 1 | y == 1)
  ham = sum(x != y)
  pairs[i, c("jac", "ham")] = c(jac, ham)
}
beepr::beep()

plot(jitter(pairs$jac, amount = 0.01), jitter(pairs$ham))
