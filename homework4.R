
# homework 2
set.seed(2)
rep = 500; n = 16; alpha = 2; beta = 4; k = 10

u <- matrix(nrow = rep)
u_tmp <- matrix(nrow = k + 1)
v_tmp <- matrix(nrow = k + 1)

betabinom_dist <- function(rep, n, alpha, beta) {
	pb <- rbeta(rep, alpha, beta)
	rbinom(rep, n, prob = pb)
}

for(i in 1:rep) {
	v_tmp[1] <- rbeta(1, 1, 1)
	u_tmp[1] <- rbinom(1, n, v_tmp[1])
	for(j in 2:(k + 1)) {
		v_tmp[j] <- rbeta(1, u_tmp[j - 1] + alpha, n - u_tmp[j - 1] + beta)
		u_tmp[j] <- rbinom(1, n, v_tmp[j])
	}
	u[i] <- u_tmp[k + 1]
}

Direct_ <- betabinom_dist(500, 16, 2, 4)
Gibbs <- table(factor(u, levels = c(0: 16)))
Direct <- table(factor(Direct_, levels = c(0: 16)))
data <- rbind(Gibbs, Direct)

barplot(data, beside = TRUE, col = c("black", "white"), ylim = c(0, 80), legend = rownames(data))
title(main = "Comparison of Two Histograms")
