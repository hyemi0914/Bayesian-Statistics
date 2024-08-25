
# highest posterior density interval: HPD

# install.packages("HDInterval")
library("HDInterval")

# for a vector:
tst <- rgamma(1e5, 2.5, 2)
hdi(tst)
hdi(tst, credMass=0.8)

# For comparison, the symmetrical 80% CrI:
quantile(tst, c(0.1,0.9))

# for a density:
dens <- density(tst)
hdi(dens, credMass=0.8)

# Now a data frame:
tst <- data.frame(mu = rnorm(1e4, 4, 1), sigma = rlnorm(1e4))
hdi(tst, 0.8)
apply(tst, 2, quantile, c(0.1,0.9))
tst$txt <- LETTERS[1:25]
hdi(tst, 0.8)

tst2 <- c(rnorm(1e5), rnorm(5e4, 7))
hist(tst2, freq=FALSE)
(hdiMC <- hdi(tst2))
segments(hdiMC[1], 0, hdiMC[2], 0, lwd=3, col='red')

# This is a valid 95% CrI, but not a Highest Density Interval
dens2 <- density(tst2)
lines(dens2, lwd=2, col='blue')
(hdiD1 <- hdi(dens2)) # default allowSplit = FALSE; note the warning
(ht <- attr(hdiD1, "height"))
segments(hdiD1[1], ht, hdiD1[2], ht, lty=3, col='blue')
(hdiD2 <- hdi(dens2, allowSplit=TRUE))
segments(hdiD2[, 1], ht, hdiD2[, 2], ht, lwd=3, col='blue')
# This is the correct 95% HDI.
