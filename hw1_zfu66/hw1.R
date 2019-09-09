########################################
####
#### CSE6242: Data & Visual Analytics / Spring 2018
####
####
#### Name: Zheng Fu / GT account name: zfu66
####
########################################


########################################
####
#### Problem 2: Log Gamma (Loop)
####
########################################

# Building log_gamma_loop function

log_gamma_loop <- function (n) {
	
	result <- 0
	i <- 1

	while (i < n){
		result <- result + log(i)
		i <- i + 1
	}

	return(result)
}

print(log_gamma_loop(5))

########################################
####
#### Problem 3: Log Gamma (Recursive)
####
########################################

# Building log_gamma_recursive function

log_gamma_recursive <- function (n) {

	result <- 0

	if ( n == 1) {
		result <- 0
	}
	else{
		result <- log(n-1) + log_gamma_recursive(n-1)
	}

	return(result)
	
}

print(log_gamma_recursive(5))

########################################
####
#### Problem 4: Sum of Log Gamma 
####
########################################

# Building sum_log_gamma_loop function

sum_log_gamma_loop <- function (n) {

	result <- 0
	i <- 1

	while (i <= n) {
		result <- result + log_gamma_loop(i)
		i <- i + 1
	}

	return(result)
}

print(sum_log_gamma_loop(5))

# Building sum_log_gamma_recursive function

sum_log_gamma_recursive <- function (n) {

	result <- 0
	i <- 1

	while (i <= n) {
		result <- result + log_gamma_recursive(i)
		i <- i + 1
	}

	return(result)
}

print(sum_log_gamma_recursive(5))

########################################
####
#### Problem 5: Compare Results to Buil-In R Function
####
########################################

# Building sum_lgamma function

sum_lgamma <- function (n) {

	result <- 0
	i <- 1

	while (i <= n) {
		result <- result + lgamma(i)
		i <- i + 1
	}

	return(result)

}

print(sum_lgamma(5))

# setup options(expressions=500000) for deep recursive loop

options(expressions=500000)

# Initializing lists for storing running time

sum_log_gamma_loop_time <- c()
sum_log_gamma_recursive_time <- c()
sum_lgamma_time <- c()

# Setting a list of n values

n_values <- seq(0,3000,100)
n_values[1] <- 1
smoothLoop <- 20 # For each n, running functions 20 times and taking the average of user time.

# Calculating sum_lgamma running time

for (n in n_values){

	sumTime <- 0

	for (t in seq(1,smoothLoop)) {

		runningTime <- system.time(sum_lgamma(n))
		userTime <- as.numeric(runningTime[1]) * 1000
		sumTime <- sumTime + userTime
	}

	averageTime <- sumTime / smoothLoop
	sum_lgamma_time <- c(sum_lgamma_time, averageTime)

}

print(sum_lgamma_time)

# Calculating sum_log_gamma_loop_time running time

for (n in n_values){

	sumTime <- 0

	for (t in seq(1,smoothLoop)) {

		runningTime <- system.time(sum_log_gamma_loop(n))
		userTime <- as.numeric(runningTime[1]) * 1000
		sumTime <- sumTime + userTime
	}

	averageTime <- sumTime / smoothLoop
	sum_log_gamma_loop_time <- c(sum_log_gamma_loop_time, averageTime)

}

print(sum_log_gamma_loop_time)

# Calculating sum_log_gamma_recursive_time running time

for (n in n_values){

	sumTime <- 0

	for (t in seq(1,smoothLoop)) {

		runningTime <- system.time(sum_log_gamma_recursive(n))
		userTime <- as.numeric(runningTime[1]) * 1000
		#print(c("Usertime"))
		#print(userTime)
		sumTime <- sumTime + userTime
	}

	averageTime <- sumTime / smoothLoop
	sum_log_gamma_recursive_time <- c(sum_log_gamma_recursive_time, averageTime)

}

print(sum_log_gamma_recursive_time)


# Output a plot

png("hw1.png", width = 1000, height = 1000)

plot(n_values, sum_log_gamma_recursive_time, cex=1.5, type="o", col="blue", ylim=c(0,15000), ann=FALSE)
axis(2, cex=1.5, at=seq(0,15000,1000))
lines(n_values, sum_log_gamma_loop_time, cex=1.5, type="o", pch=22, lty=2, col="red")
lines(n_values, sum_lgamma_time, type="o", cex=1.5, pch=22, lty=2, col="black")


# Create a title with a black, bold/italic font
title(main="Compare Results to Buil-In R Function", cex=5, col.main="black", font.main=4)

# Label the x and y axes 
title(xlab="Values of n", cex=5, col.lab="black")
title(ylab="Average execution time (milliseconds)", cex=5, col.lab="black")

# Add legend
legend(1, 14000, c("sum_log_gamma_recursive(n)","sum_log_gamma_loop(n)", "sum_lgamma_loop(n)"), cex=1.5, col=c("blue","red","black"), pch=21:22, lty=1:2)

dev.off()









