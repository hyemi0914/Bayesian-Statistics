
# homework 1
set.seed(1)

inside = 0
plot(NULL, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))

pi_monte <- function(rep){
	x <- runif(rep, min = 0, max = 1)
	y <- runif(rep, min = 0, max = 1)

	for(i in 1:length(x)){
		if(x[i]^2 + y[i]^2 <= 1){
			inside = inside + 1
			points(x[i], y[i], col = "red")
		} else{
			points(x[i], y[i], col = "blue")
		}
	}
	ppi = format(4*inside/rep, digits = 5)
	print(ppi)
	
}

result <- pi_monte(18000)

title_text <- paste("n =", 18000, ", π ≈", format(result, digits = 5))
title(main = title_text)
