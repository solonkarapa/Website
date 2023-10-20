
library(ggplot2)
library(dplyr)
# paper
# https://www.nature.com/articles/s42256-021-00353-8#Sec2
ML_data <- read.delim("~/IDrive-Sync/ML_data.txt", comment.char="#")

library(tidyr)
data_long <- gather(ML_data, model, measurement, Adjutorium:PREDICT, factor_key=TRUE)
data_long



library(stringr)
one_step <- str_split_fixed(data_long$measurement, ' \\(', 2)

two_step <- str_split_fixed(one_step[,2], '\xd0', 2)

three_step <- str_split_fixed(two_step[,2], '\\)', 2)

three_step


means <- one_step[, 1]
low_ci <- two_step[, 1]
upper_ci <- three_step[,1]


ML_data_2 <- cbind(data_long[1:4], means, low_ci, upper_ci)

str(ML_data_2)

ggplot(ML_data_2) + 
    geom_point(aes(x = factor(model), y = as.numeric(means), col = factor(time)), size = 1.8) + 
    geom_errorbar(aes(x = factor(model), ymin = as.numeric(low_ci), ymax = as.numeric(upper_ci), col = factor(time)), 
                  width = .01, size = 1.8) +
    facet_grid( metric~mortality) +
    geom_hline(data = ML_data_2 %>% filter(model == "Adjutorium"), 
               aes(yintercept = as.numeric(means)), linetype = "dashed") +
    theme_bw() +
    labs(x = "", y = "Mean 95% CI", col = "Time horizon")

ML_data_2 %>% filter(metric == "AUC-ROC" & time == 10)
