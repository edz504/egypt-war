# set reaction times
a.react <- 0.5


# initialize card deck
deck <- rep(seq(2, 14), 4)

# shuffle 
set.seed(1234)
deck <- deck[sample(length(deck), length(deck))]

# deal the cards
a <- deck[1:26]
b <- deck[27:52]

# play the game
t <- 0
a.turn <- TRUE
while (TRUE) {
    if ((length(a) == 52) | length(b) == 52) {
        break
    }

    # accumulate pot
    pot <- c()

    non.face.run <- 0 # how many non-face cards have been played
    # (if face-chain is on)
    latest.face <- 0 # last placed face card
    while (TRUE) {
        if (a.turn) {
            this.card <- a[1]
            a <- a[-1]
        } else {
            this.card <- b[1]
            b <- b[-1]
        }

        pot <- c(this.card, pot)

        # check if double (only if pot is 2 or larger)
        if ((length(pot) >= 2) & (pot[1] == pot[2])) {
            # simulate slap
            if (runif(1) > a.react) {
                # b wins 
                b <- c(b, pot)
            } else {
                # a wins
                a <- c(a, pot)
            } 
            latest.face <- 0
            non.face.run <- 0
            break
        }

        # check sandwich
        if ((length(pot) >= 3) & (pot[1] == pot[3])) {
            # simulate slap
            if (runif(1) > a.react) {
                # b wins 
                b <- c(b, pot)
            } else {
                # a wins
                a <- c(a, pot)
            } 
            latest.face <- 0
            non.face.run <- 0
            break
        }

        # deal with face-card chain
        if (latest.face != 0) {
            # if you don't play a face card
            if (this.card < 11) {
                non.face.run <- non.face.run + 1
                # if it's the end of the chain
                if (non.face.run == (latest.face - 10)) {
                    # give the pot to the other person
                    if (a.turn) {
                        b <- c(b, pot)
                        latest.face <- 0
                        non.face.run <- 0
                        break
                    } else {
                        a <- c(a, pot)
                        latest.face <- 0
                        non.face.run <- 0 
                        break
                    }
                } else { # if it's not, we've added to the run so
                # just move to the next card without switching turns
                    next
                }
            } else { # if you play a face card
                latest.face <- this.card 
                non.face.run <- 0
                a.turn <- !a.turn
                next
            }
        }

        # new face-card starting situation
        if (this.card > 10) {
            latest.face <- this.card 
        }

        # if not face or slap, continue
        a.turn <- !a.turn
    }



    t <- t + 1
}