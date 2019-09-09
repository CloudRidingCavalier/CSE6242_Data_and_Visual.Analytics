#######################
##
## CSE6242: Data & Visual Analytics / Spring 2018
## Name: Zheng Fu / GT account name: zfu66
##
#######################                                                

#######################
##
## Q1
##
#######################

library(ggplot2)
data(midwest)

midwest$state <- as.factor(midwest$state)

fill <- "#4271AE"
line <- "#1F3552"

p1_1 <- ggplot(midwest, aes(x = state, y = percprof)) + 
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "State") + 
        scale_y_continuous(name = "Percentage of people \nhaving professional education(%)",
                           breaks = seq(0, 25, 5),
                           limits=c(0, 25)) +
        ggtitle("Figure 1.1: Boxplot of percentage of people \nhaving professional education by state") +
        theme_bw()

p1_1

# Calculating overall percprof

midwest$total_prof<- as.integer(with(midwest, percprof * poptotal / 100))

df1 <- aggregate(midwest$total_prof, by=list(Category=midwest$state), FUN=sum)

#df2 <- tapply(midwest$total_prof, midwest$state, FUN=sum)

df2 <- aggregate(midwest$poptotal, by=list(Category=midwest$state), FUN=sum)

colnames(df1) <- c("state", "sumTotalProf")
colnames(df2) <- c("state", "sumTotalPop")
df3 <- merge(df1, df2, by="state")
df3$overall_percprof <- with(df3, sumTotalProf * 100/sumTotalPop)

p1_2 <- ggplot(df3, aes(y = overall_percprof, x = state)) + 
		geom_point(size=5) + 
		scale_x_discrete(name = "State") + 
        scale_y_continuous(name = "Overall percentage of people \nhaving professional education(%)",
                           breaks = seq(4, 8, 1),
                           limits=c(4, 8)) +
        ggtitle("Figure 1.2: Scatter plot of overall percentage of people \nhaving professional education by state") +
        theme_bw()
p1_2

#######################
##
## Q2
##
#######################

# perchsd vs. percollege

p <- ggplot(data = midwest, aes(x = perchsd, y = percollege)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Percentage of people with a high school diploma in each county(%)") + 
        		scale_y_continuous(name = "Percentage of college educated population \nin each county(%)") +
        		ggtitle("Figure 2.1: Relation between percentage of college educated population in each county and\npercentage of people with a high school diploma in each county") + 
				geom_point() + theme_bw()
p

df <- as.data.frame(cbind(midwest$percollege, midwest$perchsd))
colnames(df) <- c("y", "x")

m <- lm(y ~ x, df);
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
lm_text <- as.character(as.expression(eq));  

p2_1 = p + geom_text(aes(x=55, y=40), label = lm_text, parse = TRUE)
p2_1

cor(midwest$perchsd, midwest$percollege)

# state vs. perchsd

midwest$state <- as.factor(midwest$state)

fill <- "#4271AE"
line <- "#1F3552"

p2_2 <- ggplot(midwest, aes(x = state, y = perchsd)) + 
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "State") + 
        scale_y_continuous(name = "Percentage of people with \na high school diploma in each county(%)") +
        ggtitle("Figure 2.2: Boxplot of percentage of people with \na high school diploma in each county by state") +
        theme_bw()

p2_2

# state vs. percollege

midwest$state <- as.factor(midwest$state)

fill <- "#4271AE"
line <- "#1F3552"

p2_3 <- ggplot(midwest, aes(x = state, y = percollege)) + 
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "State") + 
        scale_y_continuous(name = "Percentage of college educated population \nin each county(%)") +
        ggtitle("Figure 2.3: Boxplot of percentage of college educated population \nin each county by state") +
        theme_bw()

p2_3

#######################
##
## Q3
##
#######################

# Boxplot

midwest$state <- as.factor(midwest$state)

line <- "#1F3552"

p <- ggplot(midwest, aes(x = state, y = percollege, fill = state)) + 
        geom_boxplot(colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "State") + 
        scale_y_continuous(name = "Percentage of college educated population \nin each county(%)") +
        ggtitle("Figure 3.1: Boxplot of percentage of college educated population \nin each county by state") +
        stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..), color=state),
               position=position_nudge(x=0.33), size=3.5)
        theme_bw()

p3_1 <- p + annotate("text", x = 1, y = c(42.8, 20.4, 16.9, 14.2, 11.9), label=c("Maximum", "Q3", "Median", "Q1", "Minimum"))

p3_1

# Histogram

p3_2 <- ggplot(midwest, aes(x = percollege)) + 
               geom_histogram(aes(y=..density..), colour="black", fill="white") +
               scale_x_continuous(name = "Percentage of college educated population \nin each county(%)") + 
        	   scale_y_continuous(name = "Density") +
        	   ggtitle("Figure 3.2: Histogram of percentage of college educated population \nin each county by state") +
               geom_density(alpha=.2, fill="#FF6666") 
p3_2

# Q-Q plot

y     <- quantile(midwest$percollege, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75) )         
slope <- diff(y) / diff(x)             
int   <- y[1] - slope * x[1]           

p3_3 <- ggplot(midwest, aes(sample=percollege)) + 
        stat_qq(distribution=qnorm) + 
        geom_abline(intercept=int, slope=slope) + 
        scale_y_continuous(name = "Percentage of college educated population \nin each county(%)") +
        ggtitle("Figure 3.3: QQ plot of percentage of college educated population \nin each county by state") +
        theme_bw()

p3_3

#######################
##
## Q4
##
#######################

runif_df1 <- as.data.frame(cbind(runif(10), runif(10)))
colnames(runif_df1) <- c("x", "y")

runif_df2 <- as.data.frame(cbind(runif(1000), runif(1000)))
colnames(runif_df2) <- c("x", "y")

p4_1 <- ggplot(runif_df1, aes(y = y, x = x)) + 
		geom_point(size=1.5) + 
		scale_x_continuous(name = "Set1", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        scale_y_continuous(name = "Set2", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        ggtitle("Figure 4.1: Scatter plot of two sets of 10 random uniformly-distributed values") +
        theme_bw()
p4_1

p4_2 <- ggplot(runif_df2, aes(y = y, x = x)) + 
		geom_point(size=1.5) + 
		scale_x_continuous(name = "Set1", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        scale_y_continuous(name = "Set2", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        ggtitle("Figure 4.2: Scatter plot of two sets of 1000 random uniformly-distributed values") +
        theme_bw()
p4_2

# File size 

filesize <- c()

for (fileFormat in c("ps", "pdf", "jpeg", "png")){

	for (N in seq(50,1000,50)){

		runif_df <- as.data.frame(cbind(runif(N), runif(N)))
        colnames(runif_df) <- c("x", "y")

		p <- ggplot(runif_df, aes(y = y, x = x)) + 
		geom_point(size=1.5) + 
		scale_x_continuous(name = "Set1", breaks = seq(0, 1, 0.1), limits=c(0, 1)) +
        scale_y_continuous(name = "Set2", breaks = seq(0, 1, 0.1), limits=c(0, 1)) + theme_bw()

		ggsave("temp_figure", plot = p, device = fileFormat, scale = 1, width = 8, height = 8, units = "in", dpi = 300)

		filesize <- c(filesize, as.integer(file.size("temp_figure")))
	}

}

file_format <- c(rep("ps", 20), rep("pdf", 20), rep("jpeg", 20), rep("png", 20))

results <- as.data.frame(cbind(seq(50,1000,50), filesize, file_format))
colnames(results) <- c("N", "FileSize", "FileFormat")
results$FileFormat <- as.factor(results$FileFormat)
results$FileSize <- as.numeric(levels(results$FileSize))[results$FileSize]
results$N <- as.numeric(levels(results$N))[results$N]

p4_3 <- ggplot(data = results, aes(x = N, y = FileSize, color = FileFormat), size = 2) +
	    geom_line() +
	    geom_point(size=3) + 
	    scale_x_continuous(name = "N", breaks = seq(50, 1000, 50), limits=c(50, 1000)) +
	    scale_y_continuous(name = "File Size(bytes)") +
	    ggtitle("Figure 4.3: Relationship between file size and N for different file formats") + 
	    theme_bw()
p4_3

#######################
##
## Q5
##
#######################

library(ggplot2)
data(diamonds)

diamonds$color <- as.factor(diamonds$color)

# Barchart

p5_1 <- ggplot(diamonds, aes(color)) + 
			   geom_bar(colour="black", fill="white") + 
               xlab("Diamond Color") + 
        	   ylab("Count") +
        	   ggtitle("Figure 5.1: Number of diamonds in each color") + 
               theme_bw()
p5_1

# Histogram

p5_2 <- ggplot(diamonds, aes(x = carat)) + 
               geom_histogram(aes(y=..density..), colour="black", fill="white") +
               scale_x_continuous(name = "Carat") + 
        	   scale_y_continuous(name = "Density") +
        	   ggtitle("Figure 5.2: Histogram of diamond carat") +
               geom_density(alpha=.2, fill="#FF6666") 
p5_2

p5_3 <- ggplot(diamonds, aes(x = price)) + 
               geom_histogram(aes(y=..density..), colour="black", fill="white") +
               scale_x_continuous(name = "Price") + 
        	   scale_y_continuous(name = "Density") +
        	   ggtitle("Figure 5.3: Histogram of diamond price") +
               geom_density(alpha=.2, fill="#FF6666") 
p5_3

# Color Vs. Carat

line <- "#1F3552"

p5_4 <- ggplot(diamonds, aes(x = color, y = carat, fill = color)) + 
        geom_boxplot(colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "Color") + 
        scale_y_continuous(name = "Carat") +
        ggtitle("Figure 5.4: Boxplot of carat by color") +
        theme_bw()

p5_4

# Color Vs. Price

line <- "#1F3552"

p5_5 <- ggplot(diamonds, aes(x = color, y = price, fill = color)) + 
        geom_boxplot(colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "Color") + 
        scale_y_continuous(name = "Price") +
        ggtitle("Figure 5.5: Boxplot of price by color") +
        theme_bw()

p5_5

# Carat Vs. Price

p <- ggplot(data = diamonds, aes(x = carat, y = price)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Carat") + 
        		scale_y_continuous(name = "Price") +
        		ggtitle("Figure 5.6: Relation between diamonds carat and price") + 
				geom_point() + theme_bw()
p

df <- as.data.frame(cbind(diamonds$price, diamonds$carat))
colnames(df) <- c("y", "x")

m <- lm(y ~ x, df);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))
p5_6 <- p + annotate("text", x = 2, y = 30000, label = lm_text)
p5_6

cor(diamonds$price, diamonds$carat)






