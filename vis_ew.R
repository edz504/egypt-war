library(ggplot2)
load("trial_results.RData")

pm.df <- data.frame(df)

ggplot(pm.df, aes(x=turn, y=a.more)) + geom_line()