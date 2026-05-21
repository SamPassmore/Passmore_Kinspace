## This script uses decision trees to show the key splits in kinship terminology

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  # library(party)
  library(ggparty)
  library(partykit)
})

# Arguments 
args = commandArgs(trailingOnly = TRUE)

# args
type = args[1]
# type = "siblings"
cat("Analysing:", type, "\n")

# Get the data
c_filename = paste0("results/hdbscan/", type, ".csv")
c_filename = "results/kinbank_wclusters.csv"
f_filename = paste0("data/matrix/", type, "_matrix.csv")

clusters = read.csv(c_filename)
features = read.csv(f_filename)

data = left_join(clusters, features, "Glottocode")

# data$label_ = factor(data$label_)


# make dataset
pv_idx = stringr::str_detect(colnames(data), "^[m|f]")
predictor_variables = colnames(data)[pv_idx]

# remove missing data
fit_df = data %>% 
  dplyr::select(all_of(predictor_variables), label = all_of(type)) %>% 
  filter(label != "Outlier") %>%
  na.omit()

fit_df$label = factor(fit_df$label)

# Partition the data
ind = sample(2, nrow(fit_df), replace = T, prob = c(0.8, 0.2))

train = fit_df[ind == 1,]
test = fit_df[ind == 2,]

# Tree Classification

ff = paste0("label ~ ", paste(predictor_variables, collapse = " + "))

fit.plot = partykit::ctree(formula(ff), data = train)

fit.random = cforest(formula(ff), data = train)

train_predict = predict(fit.random, train, type = "response")

# Test the model on the training data
table(train_predict, train$label)
# % accurate
cat("The training data shows that the model is",
    round((1 - mean(train_predict != train$label)) * 100, 1),
    "% accurate")

# Test the model on the test data
test_predict = predict(fit.plot, newdata = test, type="response")
table(test_predict, test$label)
# % accurate
cat("The test data shows that the model is",
    round((1 - mean(test_predict != test$label)) * 100, 1),
    "% accurate")


## Things to keeps

## Model accuracy 
train_accuracy = round((1 - mean(train_predict != train$label)) * 100, 1)
test_accuracy = round((1 - mean(test_predict != test$label)) * 100, 1)

# Rules
rules = partykit:::.list.rules.party(fit.plot)

# rule importance
vi = varimp(fit.plot)
variable_importance = sort(vi, decreasing = TRUE)

fname = paste0("results/", type, "_output.txt")
sink(fname)
cat(type, "\n\n")

cat("The training data shows that the model is",
    round((1 - mean(train_predict != train$label)) * 100, 1),
    "% accurate\n\n")

cat("The test data shows that the model is",
    round((1 - mean(test_predict != test$label)) * 100, 1),
    "% accurate\n\n")

cat("Rules:\n")
cat(rules, sep = "\n")

cat("\nImportance:\n")
cat(knitr::kable(cbind(names(variable_importance), round(variable_importance, 2))), sep = "\n")

sink()

fig_name = paste0("results/", type, "_tree.svg")
svg(fig_name)
plot(fit.plot, type = "simple",
     main = "Siblings",
     drop_terminal = TRUE)
dev.off()

