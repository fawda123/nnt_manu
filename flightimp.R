library(nnet)
library(NeuralNetTools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nycflights13)

# data preprocessing
# select UA carrier for month of december and relevant variables
# normalize and standardize variables
tomod <- filter(flights, month == 12 & carrier == 'UA') %>% 
  select(arr_delay, dep_delay, dep_time, arr_time, air_time, distance) %>%
  mutate_each(funs(scale), -arr_delay) %>% 
  mutate_each(funs(as.numeric), -arr_delay) %>% 
  mutate(arr_delay = scales::rescale(arr_delay, to = c(0, 1))) %>% 
  data.frame

# setup initial conditions
nodes <- c(1, 5, 10)
seeds <- sample(c(1:5000), 100)
grids <- expand.grid(nodes, seeds)
names(grids) <- c('nodes', 'seeds') 

# iterate through conditions
imp_vals <- vector('list', nrow(grids))
for(i in 1:nrow(grids)){
  
  cat(i, '\t')
  
  # get specific conditions
  seed_val <- grids[i, 'seeds']
  set.seed(seed_val)
  node_val <- grids[i, 'nodes']

  # create model
  mod <- nnet(arr_delay ~ ., size = node_val, linout = TRUE, data = tomod, trace = F)  

  # get importance, append to output
  imp <- olden(mod, bar_plot = FALSE)
  imp_vals[[i]] <- imp
  
}

# summarize
imp_sums <- do.call('cbind', imp_vals)
nms <- row.names(imp_sums)
imp_sums <- t(imp_sums)
names(imp_sums) <- nms
row.names(imp_sums) <- 1:nrow(imp_sums)
imp_sums <- data.frame(grids, imp_sums) %>% 
  gather('variable', 'importance', -nodes, -seeds) %>% 
  group_by(nodes, variable) %>% 
  do((function(x) {
       
    out <- quantile(x$importance, c(0.05, 0.5, 0.95))
    out <- data.frame(t(out))
    names(out) <- c('lo', 'med', 'hi')

    out
      
  })(.)) %>% 
  data.frame

# subet by nodes so barplots can be ordered by medians
toplo1 <- filter(imp_sums, nodes == 1) %>% 
  mutate(variable = factor(variable, levels = levels(variable)[order(med)]))
toplo2 <- filter(imp_sums, nodes == 5) %>% 
  mutate(variable = factor(variable, levels = levels(variable)[order(med)]))
toplo3 <- filter(imp_sums, nodes == 10) %>% 
  mutate(variable = factor(variable, levels = levels(variable)[order(med)]))

# widths
wd <- 0.3
ylims <- c(-3.93, 3.49)

p1 <- ggplot(toplo1, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = 'none') +
  scale_y_continuous('Importance', limits = ylims)

p2 <- ggplot(toplo2, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = 'none') +
  scale_y_continuous('Importance', limits = ylims)
  
p3 <- ggplot(toplo3, aes(x = variable, y = med, fill = med)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = wd) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position = 'none') +
  scale_y_continuous('Importance', limits = ylims)
