library(ggplot2)
library(dplyr)
load("trial_results.RData")

pm.df <- data.frame(mat)
colnames(pm.df) <- c("a_reaction",
    "result",
    "numTurns",
    "mean_plus_minus",
    "median_plus_minus")

# time vs. a reaction
pm.df$f.result <- factor(pm.df$result)
ggplot(pm.df, 
    aes(x=a_reaction, y=numTurns)) + geom_point(aes(colour=f.result)) + 
    labs(title="Time vs. Player A reaction 0 to 1 (by 0.01), 100 trials each")
ggsave(file="time_vs_reaction.png", width=10, height=6)

grouped.pm.df <- pm.df %>% 
    group_by(a_reaction) %>%
        summarise_each(funs(mean))
ggplot(grouped.pm.df, 
    aes(x=a_reaction, y=numTurns)) + geom_point(aes(colour=result)) + 
    labs(title="Time vs. Player A reaction 0 to 1 (by 0.01), average 100 trials")
ggsave(file="time_vs_reaction_grouped.png", width=10, height=6)

# probabiltity of a winning vs. a reaction
probs.df <- pm.df %>% 
    group_by(a_reaction, result) %>%
        summarise(count=n())
probs.graph <- data.frame(matrix(nrow=100, ncol=2))
colnames(probs.graph) <- c("a_reaction", "emp_prob_a_wins")
i <- 1
for (val in unique(pm.df$a_reaction)) {
    probs.graph[i, 1] <- val 
    probs.graph[i, 2] <- length(which(pm.df$result == 1 &
        pm.df$a_reaction == val)) / 100
    i <- i + 1
}

ggplot(probs.graph, aes(x=a_reaction, y=emp_prob_a_wins)) + 
    geom_bar(stat="identity")
ggsave(file="emp_prob_vs_reaection.png", width=10, height=6)


# game median vs mean, coloured by result
ggplot(pm.df, aes(x=mean_plus_minus, y=median_plus_minus)) + 
    geom_point(aes(colour=f.result))
ggsave(file="m_vs_mean_cresult.png", width=10, height=6)

ggplot(grouped.pm.df,
    aes(x=mean_plus_minus, y=median_plus_minus)) + 
    geom_point(aes(colour=result))
ggsave(file="m_vs_mean_cresult_grouped.png", width=10, height=6)