# Function that returns Root Mean Squared Error
rmse <- function(error)
{
    sqrt(mean(error^2))
}

# Function for calulating mean and sd by groups
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm=na.rm), mean = mean (xx[[col]], na.rm=na.rm), sd = sd (xx[[col]], na.rm=na.rm))
  }, measurevar )
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t‐statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N‐1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

# Model evaluation
model_evaluation <- function(data, modelName) {
	
	# Figure title
	title <- paste("Learning curve on model ", modelName)

	## 80% of the data as training set
	split_size <- floor(0.8 * nrow(data))

	## set the seed to make partition reproductible
	set.seed(2018)
	train_ind <- sample(seq_len(nrow(data)), size = split_size)
	train <- data[train_ind, ]
	test <- data[-train_ind, ]

	sampleSize_train_rmse <- c()
	sampleSize_test_rmse <- c()
	trained_models <- list()
	model.index <- 1

	for (i in seq(0.1,1,0.1)){

  		for (j in seq(1,10,1)){

  			#sample data
  			sampleSize_train <- train[sample(nrow(train),nrow(train)*i),]
  			train.results <- lm(Gross ~ ., data = sampleSize_train)
  			sampleSize_train_rmse <- c(sampleSize_train_rmse, rmse(train.results$residuals))
  			prediction <- predict(train.results, newdata = test)
  			sampleSize_test_rmse <- c(sampleSize_test_rmse, rmse(test$Gross - prediction))
  			trained_models[[model.index]] <- train.results
  		}
  	}

  	train_all <- data.frame(rmse=sampleSize_train_rmse, type=rep('train', 100), fraction=rep(seq(0.1,1,0.1), each = 10))
  	test_all  <- data.frame(rmse=sampleSize_test_rmse,  type=rep('test', 100),  fraction=rep(seq(0.1,1,0.1), each = 10))
  	model_all <- rbind(train_all, test_all)
  	model_summary <- summarySE(model_all, measurevar="rmse", groupvars=c("type","fraction"))

  	p01 <- ggplot(model_summary, aes(x=fraction, y=rmse, colour=type)) + geom_errorbar(aes(ymin=rmse-se, ymax=rmse+se), width=.1) + geom_line() + geom_point()

  	p01 <- p01 + geom_point(size=1.5) + scale_x_continuous(name = "Sample Size Fraction", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        scale_y_continuous(name = "Mean RMSE", breaks = seq(0.5, 1, 0.05), limits=c(0.5, 1)) +
        ggtitle(title) + theme_bw()

    list(figure=p01, model_summary=model_summary, trained_models=trained_models)

}