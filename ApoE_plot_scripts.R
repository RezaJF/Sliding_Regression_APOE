library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(broom)
library(ggpubr)
library("gridExtra")

df <- read.csv('~/Documents/Centnarian_analysis/Imputation/Imputed_data_rolling_window_frq/split_30_15_unRelated/APOE_plot_df.txt', sep='\t')


ApoE2 <-  ggplot(data = df, aes(x = ApoE2, y = Mean.Age)) + 
  geom_point(color='darkblue', shape=8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size= 0.5)+
  theme_classic()+ xlab("APOE2 allelic frequency") + ylab("mean age")

ApoE3 <-  ggplot(data = df, aes(x = ApoE3, y = Mean.Age)) + 
  geom_point(color='darkblue', shape=8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size= 0.5)+
  theme_classic()+ xlab("APOE3 allelic frequency") + ylab("mean age")

ApoE4 <-  ggplot(data = df, aes(x = ApoE4, y = Mean.Age)) + 
  geom_point(color='darkblue', shape=8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size= 0.5)+
  theme_classic()+ xlab("APOE4 allelic frequency") + ylab("mean age")

grid.arrange (ApoE2, ApoE3, ApoE4, ncol= 3, nrow=1)

##Scatter-plot with regression line and p-value
# (P-values shown in the plots are related to the Pearson's correlation coefficients and not the regression)

ggscatter(df, x = "ApoE2", y = "Mean.Age", add = "reg.line") +
  stat_cor(label.y = 0.6) +
  stat_regline_equation(label.y = 0.143)

ggscatter(df, x = "ApoE3", y = "Mean.Age", add = "reg.line") +
  stat_cor(label.y = 0.6) +
  stat_regline_equation(label.y = 0.56)

ggscatter(df, x = "ApoE4", y = "Mean.Age", add = "reg.line") +
  stat_cor(label.y = 0.2) +
  stat_regline_equation(label.y = 0.19)
