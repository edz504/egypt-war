source("ew.R")

N <- 100
gran <- 0.01

mat <- matrix(nrow=(length(seq(0, .99, by=gran)) * length(1:N)),
    ncol = 5)

i <- 1
for (a.react in seq(0, .99, by=gran)) {
    for (t in 1:N) {
        pm <- egypt.war.game(a.react)

        # a's % reaction win
        mat[i, 1] <- a.react 

        # result of game
        mat[i, 2] <- as.numeric((pm[length(pm)] > 0))  

        # length of game
        mat[i, 3] <- length(pm)

        # mean +/- of game
        mat[i, 4] <- mean(pm)

        # median +/- of game
        mat[i, 5] <- median(pm)  

        i <- i + 1
    }

    print(a.react)
}

save(mat, file="trial_results.RData")