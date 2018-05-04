# total downloads for neural network packages off CRAN
# make sure to change cdate to time of writing
library("cranlogs")
today <- as.Date("2016-10-20")

tots <- c(
  sum(cran_downloads("nnet", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("neuralnet", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("RSNNS", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("FCNN4R", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("AMORE", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("monmlp", from = "2000-01-01", to = today)$count),
  sum(cran_downloads("qrnn", from = "2000-01-01", to = today)$count)
)

tots_nnt <- 100 * sum(tots[1:3])/sum(tots)
tots_nnt
tots_nnet <- 100 * tots[1]/sum(tots)
tots_nnet

# install.packages("NeuralNetTools")
library("NeuralNetTools")

set.seed(123)
library("RSNNS")
x <- neuraldat[, c("X1", "X2", "X3")]
y <- neuraldat[, "Y1"]
mod1 <- mlp(x, y, size = 5)
library("neuralnet")
mod2 <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
library("nnet")
mod3 <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)

par(mar = c(0, 0, 0, 0))
plotnet(mod3, nid = FALSE, circle_col = "grey", bord_col = "grey")
plotnet(mod3)

modskip <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, skip = TRUE)

par(mar = c(0, 0, 0, 0))
plotnet(modskip, skip = TRUE)
plotnet(modskip)

pruneFuncParams <- list(max_pr_error_increase = 10.0, pr_accepted_error = 1.0,
  no_of_pr_retrain_cycles = 1000, min_error_to_stop = 0.01,
  init_matrix_value = 1e-6, input_pruning = TRUE, hidden_pruning = TRUE)
mod <- mlp(x, y, size = 5, pruneFunc = "OptimalBrainSurgeon",
 pruneFuncParams = pruneFuncParams)
plotnet(mod, rel_rsc = c(3, 8))
plotnet(mod, prune_col = "lightblue", rel_rsc = c(3, 8))

garson(mod1)
olden(mod1)
garson(mod2)
olden(mod2)
garson(mod3)
olden(mod3)

lekprofile(mod3)
lekprofile(mod3, group_show = TRUE)
lekprofile(mod3, group_vals = 6)
lekprofile(mod3, group_vals = 6, group_show = TRUE)

library("nnet")
library("NeuralNetTools")
library("dplyr")
library("tidyr")
library("ggplot2")
data("flights", package = "nycflights13")

# data preprocessing
# select UA carrier for month of december and relevant variables
# normalize and standardize variables
tomod <- filter(flights, month == 12 & carrier == "UA") %>% 
  select(arr_delay, dep_delay, dep_time, arr_time, air_time, distance) %>%
  mutate_each(funs(scale), -arr_delay) %>% 
  mutate_each(funs(as.numeric), -arr_delay) %>% 
  mutate(arr_delay = scales::rescale(arr_delay, to = c(0, 1))) %>% 
  data.frame

# example model
mod <- nnet(arr_delay ~ ., size = 5, linout = TRUE, data = tomod, trace = FALSE)  

# plots of the example model
plotnet(mod)
garson(mod)
olden(mod)
lekprofile(mod, group_vals = 5)
lekprofile(mod, group_vals = 5, group_show = TRUE)

# test the effects of model structure and starting conditions on variable importance

# setup initial conditions
nodes <- c(1, 5, 10)
seeds <- sample(c(1:5000), 100)
grids <- expand.grid(nodes, seeds)
names(grids) <- c("nodes", "seeds") 

# iterate through conditions
imp_vals <- vector("list", nrow(grids))
for(i in 1:nrow(grids)){
  
  cat(i, "\t")
  
  # get specific conditions
  seed_val <- grids[i, "seeds"]
  set.seed(seed_val)
  node_val <- grids[i, "nodes"]

  # create model
  mod <- nnet(arr_delay ~ ., size = node_val, linout = TRUE, data = tomod, trace = FALSE)  

  # get importance, append to output
  imp <- olden(mod, bar_plot = FALSE)
  imp_vals[[i]] <- imp
  
}

# summarize
imp_sums <- do.call("cbind", imp_vals)
nms <- row.names(imp_sums)
imp_sums <- t(imp_sums)
names(imp_sums) <- nms
row.names(imp_sums) <- 1:nrow(imp_sums)
imp_sums <- data.frame(grids, imp_sums) %>% 
  gather("variable", "importance", -nodes, -seeds) %>% 
  group_by(nodes, variable) %>% 
  do((function(x) {
       
    out <- quantile(x$importance, c(0.05, 0.5, 0.95))
    out <- data.frame(t(out))
    names(out) <- c("lo", "med", "hi")

    out
      
  })(.)) %>% 
  data.frame
# save(imp_sums, file = "imp_sums.RData", compress = "xz")

# subet by nodes so barplots can be ordered by medians
toplo1 <- filter(imp_sums, nodes == 1) %>% 
  mutate(variable = factor(variable, levels = variable[order(med)]))
toplo2 <- filter(imp_sums, nodes == 5) %>% 
  mutate(variable = factor(variable, levels = variable[order(med)]))
toplo3 <- filter(imp_sums, nodes == 10) %>% 
  mutate(variable = factor(variable, levels = variable[order(med)]))

# widths
wd <- 0.3
ylims <- with(imp_sums, c(min(lo), max(hi)))

p1 <- ggplot(toplo1, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_y_continuous("Importance", limits = ylims)

p2 <- ggplot(toplo2, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_y_continuous("Importance", limits = ylims)
  
p3 <- ggplot(toplo3, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_y_continuous("Importance", limits = ylims)

print(p1)
print(p2)
print(p3)

sessionInfo()
