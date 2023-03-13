#-----------------------------------------------------------------
# 1. Missing values imputation using KNN and Violin plots/Boxplots
#-----------------------------------------------------------------

# 1. Missing values imputation

# load libraries and create artificial dataset
library(caret)
library(missForest)
library(tidyverse)
library(RANN)
library(gridExtra)

set.seed(2023) # for reproducibility
var1 <- rpois(900,2)
var2 <- rpois(900,4)
var3 <- rpois(900,6)
category <- c(rep('cat1', 300), rep('cat2', 300), rep('cat3', 300))
subcategory <- sample(LETTERS[1:6], size = 900, replace = TRUE, 
                      prob = c(0.25, 0.2, 0.2, 0.1, 0.1, 0.05))
dataset <- data.frame(var1, var2, var3, subcategory, category)

# prodNA produce 5% missing data
dataset.mis = data.frame(prodNA(dataset[,1:3], noNA = 0.05), subcategory, category) 
write.csv(dataset.mis, "dataset.mis.csv")
dataset.mis = read.csv("dataset.mis.csv", header = TRUE)
dataset.mis <- dataset.mis[,2:6]
dataset.mis[298:301, ] # excerpt of the dataset contains NA values

#     var1 var2 var3 subcategory category
# 298    0   10    5           C     cat1
# 299   NA    1    5           E     cat1
# 300    2    3    5           A     cat1
# 301    2   NA    8           B     cat2

# knn imputation using caret and 5 'neighbours'
set.seed(2023)
dataset.mis.model = preProcess(dataset.mis %>% 
                              dplyr::select(names(dataset.mis)),
                              "knnImpute", k = 5, knnSummary = mean)
dataset.mis.model
dataset.mis.pred = predict(dataset.mis.model, dataset.mis) # variables are normalized
dataset.mis.pred[298:301, ]

#            var1       var2       var3 subcategory category
# 298 -1.37892580  3.0235674 -0.4137490           C     cat1
# 299  0.70767513 -1.4093204 -0.4137490           E     cat1
# 300  0.01214149 -0.4242342 -0.4137490           A     cat1
# 301  0.01214149 -0.1287083  0.8754844           B     cat2


# values in original scale
complete.dataset <- data.frame(col = names(dataset.mis[,1:3]), 
                               mean = dataset.mis.model$mean, 
                               sd = dataset.mis.model$std)
for(i in complete.dataset$col){
  dataset.mis.pred[i] <- dataset.mis.pred[i]*dataset.mis.model$std[i] + dataset.mis.model$mean[i] 
}

# now the dataset is complete
complete.data..dataset <- dataset.mis.pred
complete.data.dataset[298:301, ]

#     var1 var2 var3 subcategory category
# 298    0 10.0    5           C     cat1
# 299    3  1.0    5           E     cat1
# 300    2  3.0    5           A     cat1
# 301    2  3.6    8           B     cat2


# 2. Visualization using Violin plots

dataset <- complete.data.dataset

# compute the mean of the variable 'var1' for each 'subcategory' group
dataset2 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var1 = mean(var1))

# compute the mean of the variable 'var2' for each 'subcategory' group
dataset3 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var2 = mean(var2))

# compute the mean of the variable 'var3' for each 'subcategory' group
dataset4 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var3 = mean(var3))

#-------------------------------------------------------------------------------
# minimal code violin plot
ggplot(dataset, aes(x = subcategory, y = var1)) +
  geom_violin()

# multiple violin plots
ggplot(dataset2, aes(x = subcategory, y = var1)) +
  geom_violin(aes(fill=Mean_var1)) +
  geom_point(aes(x = subcategory, y = var1), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var1)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset2$Mean_var1)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Boxplot for var1 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var1'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))
#-------------------------------------------------------------------------------

# Violin plot with facet_wrap
p1 <- ggplot(dataset2, aes(x = subcategory, y = var1)) +
  geom_violin(aes(fill=Mean_var1)) +
  geom_point(aes(x = subcategory, y = var1), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var1)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset2$Mean_var1)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Violin plot for var1 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var1'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

# Violin plot with facet_wrap
p2 <- ggplot(dataset3, aes(x = subcategory, y = var2)) +
  geom_violin(aes(fill= Mean_var2)) +
  geom_point(aes(x = subcategory, y = var2), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var2)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset3$Mean_var2)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Violin plot for var2 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var2'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

# Violin plot with facet_wrap
p3 <- ggplot(dataset4, aes(x = subcategory, y = var3)) +
  geom_violin(aes(fill= Mean_var3)) +
  geom_point(aes(x = subcategory, y = var3), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var3)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset4$Mean_var3)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Violin plot for var3 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var3'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

final.plot <- grid.arrange(p1, p2, p3)


# 3. Visualization using Boxplots

dataset <- complete.data.dataset

# compute the mean of the variable 'var1' for each 'subcategory' group
dataset2 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var1 = mean(var1))

# compute the mean of the variable 'var2' for each 'subcategory' group
dataset3 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var2 = mean(var2))

# compute the mean of the variable 'var3' for each 'subcategory' group
dataset4 <- dataset %>% 
  group_by(subcategory) %>%
  mutate(Mean_var3 = mean(var3))

#-------------------------------------------------------------------------------
# minimal code violin plot
ggplot(dataset, aes(x = subcategory, y = var1)) +
  geom_boxplot()

# multiple violin plots
ggplot(dataset, aes(x = subcategory, y = var1)) +
  geom_boxplot() +
  geom_point(aes(x = subcategory, y = var1)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Boxplot for var1 x subcategory, for each category group') +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))
#-------------------------------------------------------------------------------

# Violin plot with facet_wrap
p2 <- ggplot(dataset3, aes(x = subcategory, y = var2)) +
  geom_boxplot(aes(fill= Mean_var2)) +
  geom_point(aes(x = subcategory, y = var2), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var2)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset3$Mean_var2)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Boxplot for var2 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var2'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

# Violin plot with facet_wrap
p3 <- ggplot(dataset4, aes(x = subcategory, y = var3)) +
  geom_boxplot(aes(fill= Mean_var3)) +
  geom_point(aes(x = subcategory, y = var3), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(var3)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(dataset4$Mean_var3)) +
  facet_wrap(~category, scales="free") +
  labs(title = 'Boxplot for var3 x subcategory, for each category group',
       subtitle = "Color gradient indicate the mean of the variable 'var3'",
       caption = "Artificial dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

final.plot <- grid.arrange(p1, p2, p3)

#-------
# end --
#-------
