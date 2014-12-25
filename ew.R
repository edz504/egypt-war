egypt.war.game <- function(a.react) {
    # initialize card deck
    deck <- rep(seq(2, 14), 4)

    # shuffle 
    deck <- deck[sample(length(deck), length(deck))]

    # deal the cards
    a <- deck[1:26]
    b <- deck[27:52]

    # track plus.minus
    a.more.b <- c()

    # play the game
    a.turn <- TRUE
    while (TRUE) {

        a.more.b <- c(a.more.b, length(a) - length(b))

        # check if game over
        if ((length(a) == 0) | length(b) == 0) {
            break
        }

        # accumulate pot
        pot <- c()
        non.face.run <- 0 # how many non-face cards have been played
        # (if face-chain is on)
        latest.face <- 0 # last placed face card
        while (TRUE) {

            if (a.turn) {
                if (length(a) > 0) {
                    this.card <- a[1]
                    a <- a[-1]
                } else {
                    b <- c(b, pot)
                    break
                }
            } else {
                if (length(b) > 0) {
                    this.card <- b[1]
                    b <- b[-1]
                } else {
                    a <- c(a, pot)
                    break
                }
            }

            pot <- c(this.card, pot)

            # check if double (only if pot is 2 or larger)
            if ((length(pot) >= 2) & (pot[1] == pot[2])) {
                # simulate slap
                if (runif(1) > a.react) {
                    # b wins 
                    b <- c(b, pot)
                    a.turn <- FALSE # upon collecting, it's winner's turn
                } else {
                    # a wins
                    a <- c(a, pot)
                    a.turn <- TRUE
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
                    a.turn <- FALSE
                } else {
                    # a wins
                    a <- c(a, pot)
                    a.turn <- TRUE
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
                            b.turn <- TRUE
                            break
                        } else {
                            a <- c(a, pot)
                            latest.face <- 0
                            non.face.run <- 0
                            a.turn <- TRUE
                            break
                        }
                    } else { # if it's not, we've added to the run so
                    # just move to the next card without switching turns
                        next
                    }
                } else {
                    # if you play a face card, restart the chain onto
                    # other person
                    latest.face <- this.card 
                    non.face.run <- 0
                    a.turn <- !a.turn
                    next
                }
            }

            # new face-card chain starting situation
            if (this.card > 10) {
                latest.face <- this.card 
            }

            # if not face, slap, continue
            a.turn <- !a.turn
        }
    }

    return(a.more.b)
}

