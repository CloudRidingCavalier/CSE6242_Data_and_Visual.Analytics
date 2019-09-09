---
output:
  pdf_document: default
  html_document: default
---
# Zheng Fu / zfu66@gatech.edu

---
title: 'Project 1: Explore and Prepare Data'
subtitle: |-
  CSE6242 - Data and Visual Analytics - Fall 2017
  Due: Sunday, October 15, 2017 at 11:59 PM UTC-12:00 on T-Square
output:
  pdf_document: default
  html_notebook: default
  html_document:
    df_print: paged
---

_Note: This project involves getting data ready for analysis and doing some preliminary investigations. Project 2 will involve modeling and predictions on the same dataset, and will be released at a later date. Both projects will have equal weightage towards your grade. You may reuse some of the preprocessing/analysis steps from Project 1 in Project 2._

# Data

In this project, you will explore a dataset that contains information about movies, including ratings, budget, gross revenue and other attributes. It was prepared by Dr. Guy Lebanon, and here is his description of the dataset:

> The file [`movies_merged`](https://s3.amazonaws.com/content.udacity-data.com/courses/gt-cs6242/project/movies_merged) contains a dataframe with the same name that has 40K rows and 39 columns. Each row represents a movie title and each column represents a descriptor such as `Title`, `Actors`, and `Budget`. I collected the data by querying IMDb’s API (see [www.omdbapi.com](http://www.omdbapi.com/)) and joining it with a separate dataset of movie budgets and gross earnings (unknown to you). The join key was the movie title. This data is available for personal use, but IMDb’s terms of service do not allow it to be used for commercial purposes or for creating a competing repository.

# Objective

Your goal is to investigate the relationship between the movie descriptors and the box office success of movies, as represented by the variable `Gross`. This task is extremely important as it can help a studio decide which titles to fund for production, how much to bid on produced movies, when to release a title, how much to invest in marketing and PR, etc. This information is most useful before a title is released, but it is still very valuable after the movie is already released to the public (for example it can affect additional marketing spend or how much a studio should negotiate with on-demand streaming companies for “second window” streaming rights).

# Instructions

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. Open this file in RStudio to get started.

When you execute code within the notebook, the results appear beneath the code. Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
x = 1:10
print(x^2)
```

Plots appear inline too:
```{r}
plot(x, x^2, 'o')
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*. Enter some R code and run it.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file).

Please complete all the tasks below by implementing code chunks that have a `TODO` comment in them, running all code chunks so that output and plots are displayed, and typing in answers to each question (**Q:** ...) next to/below the corresponding answer prompt (**A:**). Feel free to add code chunks/show additional output to support any of the answers.

When you are done, you will need to submit the final R markdown file (as **pr1.Rmd**) with all code chunks implemented and executed, and all text responses written in. You also need to submit a PDF export of the markdown file (as **pr1.pdf**), which should show your code, output, plots and written responses--this will be your project report. Compress these two files into a single .zip archive and upload it on T-Square.

# Setup

## Load data

Make sure you've downloaded the [`movies_merged`](https://s3.amazonaws.com/content.udacity-data.com/courses/gt-cs624l2/project/movies_merged) file and it is in the current working directory. Now load it into memory:

```{r}
load('movies_merged')
cat("Dataset has", dim(movies_merged)[1], "rows and",
    dim(movies_merged)[2], "columns", end="\n", file="")
```

This creates an object of the same name (`movies_merged`). For convenience, you can copy it to `df` and start using it:

```{r}
df = movies_merged
cat("Column names:", end="\n", file="")
colnames(df)
```

## Load R packages

Load any R packages that you will need to use. You can come back to this chunk, edit it and re-run to load any additional packages later.

```{r}
library(ggplot2)
library(GGally)
```

If you are loading any non-standard packages (ones that have not been discussed in class or explicitly allowed for this project), please mention them below. Include any special instructions if they cannot be installed using the regular `install.packages('<pkg name>')` command.

**Non-standard packages used**: None

# Tasks

Each task below is worth **10** points, and is meant to be performed sequentially, i.e. do step 2 after you have processed the data as described in step 1. Total points: **100**

Complete each task by implementing code chunks as described by `TODO` comments, and by responding to questions ("**Q**:") with written answers ("**A**:"). If you are unable to find a meaningful or strong relationship in any of the cases when requested, explain why not by referring to appropriate plots/statistics.

It is okay to handle missing values below by omission, but please omit as little as possible. It is worthwhile to invest in reusable and clear code as you may need to use it or modify it in project 2.

## 1. Remove non-movie rows

The variable `Type` captures whether the row is a movie, a TV series, or a game. Remove all rows from `df` that do not correspond to movies.

```{r}
# TODO: Remove all rows from df that do not correspond to movies
df_original <- df
df2 <- df[df$Type == "movie",]
df <- df2
nrow(df)
```

**Q**: How many rows are left after removal? _Enter your response below._

**A**: 40,000 rows are left after removing non-movie objects.

## 2. Process `Runtime` column

The variable `Runtime` represents the length of the title as a string. Write R code to convert it to a numeric value (in minutes) and replace `df$Runtime` with the new numeric column.

```{r}
# TODO: Replace df$Runtime with a numeric column containing the runtime in minutes
a <- df$Runtime
a <- gsub(" min", "", a)
a <- gsub("N/A", "0", a)
a <- gsub("1 h", "60", a)
a <- gsub("2 h", "120", a)
a <- gsub("3 h", "180", a)
a <- gsub("4 h", "240", a)
b <- sapply(strsplit(a," "),
  function(x) {
    x <- as.numeric(x)
    x[1]+x[2]
    }
)
b <- as.character(b)
a[!is.na(b)] <- b[!is.na(b)]
df$Runtime <- as.numeric(a)
```

Now investigate the distribution of `Runtime` values and how it changes over years (variable `Year`, which you can bucket into decades) and in relation to the budget (variable `Budget`). Include any plots that illustrate.

```{r}
# TODO: Investigate the distribution of Runtime values and how it varies by Year and Budget
NO.0.runtime <- df[df$Runtime > 0,]
p2_1 <- ggplot(NO.0.runtime, aes(x = Runtime)) + 
               geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=10) +
               scale_x_continuous(name = "Movie Runtime (mins)") + 
        	     scale_y_continuous(name = "Density") +
        	     ggtitle("Figure 2.1: Histogram of Movie Runtime") +
               geom_density(alpha=.2, fill="#FF6666") 
p2_1
```

```{r}
movieYear <- NO.0.runtime[!is.na(NO.0.runtime$Year),]
movieYear$Year <- as.factor(movieYear$Year)
aa <- as.data.frame(aggregate(movieYear$Runtime, by=list(movieYear$Year), FUN=mean))
colnames(aa) <- c("Year", "Average_Runtime")
aa$Year <- as.numeric(levels(aa$Year)[as.integer(aa$Year)])

p2_2 <- ggplot(data = aa, aes(x = Year, y = Average_Runtime)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Year", breaks = seq(1880, 2020, 20), limits=c(1880, 2020)) + 
        		scale_y_continuous(name = "Average Runtime (mins)", breaks = seq(0, 120, 10), limits=c(0, 120)) +
        		ggtitle("Figure 2.2: Relation between movie releasing year and average runtime") + 
				geom_point() + theme_bw()

m <- lm(Year ~ Average_Runtime, aa);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p2_2 <- p2_2 + annotate("text", x = 1900, y = 100, label = lm_text)
p2_2



```

```{r}
movieBudget <- NO.0.runtime[!is.na(NO.0.runtime$Budget),]
movieBudget$Budget <- log(movieBudget$Budget, 10)

p2_3 <- ggplot(data = movieBudget, aes(x = Budget, y = Runtime)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "log10(Budget)", breaks = seq(2, 10, 1), limits=c(2, 10)) + 
        		scale_y_continuous(name = "Runtime (mins)", breaks = seq(0, 400, 100), limits=c(0, 400)) +
        		ggtitle("Figure 2.3: Relation between movie budget and runtime") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieBudget$Runtime, movieBudget$Budget))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p2_3 <- p2_3 + annotate("text", x = 3, y = 300, label = lm_text)
p2_3

```


_Feel free to insert additional code chunks as necessary._

**Q**: Comment on the distribution as well as relationships. Are there any patterns or trends that you can observe?

**A**: 
1. The distribution of movie runtime has a double-peaks shape with a long right tail. One peak occurs in runtime = 10 ~ 20 mins and the other peak occurs in runtime = 100 ~ 120 mins.

2. The average runtime of movies increased dramatically from year 1900 and 1920. And there is a positive correlation between movie releasing year and average run time with R^2 = 0.781.

3. No obvious relationship between movie budget and runtime was observed (R^2 = 0.101). High budgets does not necessarily produce longer runtime movies.

## 3. Encode `Genre` column

The column `Genre` represents a list of genres associated with the movie in a string format. Write code to parse each text string into a binary vector with 1s representing the presence of a genre and 0s the absence, and add it to the dataframe as additional columns. Then remove the original `Genre` column.

For example, if there are a total of 3 genres: Drama, Comedy, and Action, a movie that is both Action and Comedy should be represented by a binary vector <0, 1, 1>. Note that you need to first compile a dictionary of all possible genres and then figure out which movie has which genres (you can use the R `tm` package to create the dictionary).

```{r}
# TODO: Replace Genre with a collection of binary columns
t1  <- strsplit(as.character(df$Genre), ",\\s?") # split the strings
lvl <- unique(unlist(t1))                         # get unique elements
t2  <- lapply(t1, factor, levels = lvl)           # convert to factor
results <- as.data.frame(t(sapply(t2, table)))
df <- cbind(df, results)
df$Genre <- NULL
```

Plot the relative proportions of movies having the top 10 most common genres.

```{r}
# TODO: Select movies from top 10 most common genres and plot their relative proportions
df$`N/A`<-NULL
#colnames(df)
df_genres <- df[,39:66]
df_genres_sort <- df_genres[, order(-colSums(df_genres))]
#colSums(df_genres)[order(-colSums(df_genres))]
df_genres_top10 <- df_genres_sort[,1:10]
df_genres_top10_movies <- df_genres_top10[rowSums(df_genres_top10) > 0, ]
rp <- colSums(df_genres_top10_movies) * 100 / nrow(df_genres_top10_movies)
rp_df <- as.data.frame(rp)
rp_df$genres <- rownames(rp_df)

p3_1 <- ggplot(rp_df, aes(y = rp, x = genres)) + 
		geom_point(size=5) + 
		scale_x_discrete(name = "Genres") + 
        scale_y_continuous(name = "Relative proportions of movies(%)",
                           breaks = seq(0, 50, 5),
                           limits=c(0, 50)) +
        ggtitle("Figure 3.1: Scatter plot of relative proportions of movies having \nthe top 10 most common genres") +
        theme_bw()
p3_1
```

Examine how the distribution of `Runtime` changes across genres for the top 10 most common genres.

```{r}
colnames(df_genres_top10)
#df["Drama"] > 0
```

```{r}
# Multiplot function from R cookbook

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

```{r}
# TODO: Plot Runtime distribution for top 10 most common genres

plots <- list()  # new empty list
for (i in 1:10) {
    df_distribution <- df[df[colnames(df_genres_top10)[i]] > 0, ]
    df_distribution <- df_distribution[df_distribution$Runtime > 0,]
    p1 <- ggplot(df_distribution, aes(x = Runtime)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=10) + scale_x_continuous(name = "Movie Runtime (mins)") + scale_y_continuous(name = "Density") + geom_density(alpha=.2, fill="#FF6666") + ggtitle(colnames(df_genres_top10)[i])
    plots[[i]] <- p1  # add each plot into plot list
}
multiplot(plotlist = plots, cols = 3)

```

**Q**: Describe the interesting relationship(s) you observe. Are there any expected or unexpected trends that are evident?

**A**: 

1. The distribution of runtime of Drama, Action, Adventure, Crime, Thriller and Romance have the similar single-peak bell shape with mean of runtime around 100 mins.

2. The distribution of runtime of Short and Animation have the similar single-peak shape with long right tails (skewed right), and the peak occurs around 10mins, that is what I expect.

3. The distribution of runtime of Comedy and Documentary have the similar double-peak shapes with long right tails (skewed right). Both of them have one peak occurs around 10 mins and the other one occurs around 100 mins.

## 4. Eliminate mismatched rows

The dataframe was put together by merging two different sources of data and it is possible that the merging process was inaccurate in some cases (the merge was done based on movie title, but there are cases of different movies with the same title). There are 3 columns that contain date information: `Year` (numeric year), `Date` (numeric year), and `Released` (string representation of the release date).

Find and remove all rows where you suspect a merge error occurred based on a mismatch between these variables. To make sure subsequent analysis and modeling work well, avoid removing more than 10% of the rows that have a `Gross` value present.

_Note: Do not remove the rows with `Gross == NA` at this point, just use this a guideline._

```{r}
# TODO: Remove rows with Year/Date/Released mismatch
print("NA in df$Year:")
sum(is.na(df$Year))
print("NA in df$Released:")
sum(is.na(df$Released))
print("NA in df$Date:")
sum(is.na(df$Date))
print("no NA in df$Gross:")
sum(!is.na(df$Gross))

df_noNA_release <- df[!is.na(df$Released),]
df_NA_release <- df[is.na(df$Released),]
year <- as.numeric(df_noNA_release$Year)
released.date <- as.character(df_noNA_release$Released)

released.year <- sapply(strsplit(released.date,"-"),
  function(x) {
    as.numeric(x[1])
    }
)

boolean_list <- year == released.year
df_noNA_release <- df_noNA_release[boolean_list,]
print("Number of rows of removing mismath releasing years and year:")
nrow(df_noNA_release)
print("no NA in df_noNA_release$Gross:")
sum(!is.na(df_noNA_release$Gross))



```
```{r}
df2<- df[df$Year == df$Date, ]
nrow(df2)
print("no NA in df2$Gross:")
sum(!is.na(df2$Gross))
```

**Q**: What is your precise removal logic, and how many rows remain in the resulting dataset?

**A**: I fisrt checked the number of NA values in "Year" and "Released" columns in df dataframe. There are 4949 rows have NA values in Released columns and I removed them from the dataframe. I extracted released year information from "Released" column and removed those mismatched "Year" column. Finally I have a data frame that has 29324 rows with 3743 rows have "Gross" value presented.

I then compared "Date" column and "Year" in df dataframe and removed rows with mismatched values in these two columns. Consequently I have a data frame that has 39293 rows with 3851 rows have "Gross" value presented.

In the original 40,000 rows df data frame, both "Gross" and "Date" have same 35442 "NA" value indicating they might be from the same source of dataset. And for subsequent analysis and modeling work, I want to keep as much rows having "Gross" value as possible. Thus I finally chose the data frame has 39293 rows with 3851 rows have "Gross" value presented.

## 5. Explore `Gross` revenue

For the commercial success of a movie, production houses want to maximize Gross revenue. Investigate if Gross revenue is related to Budget, Runtime or Genre in any way.

_Note: To get a meaningful relationship, you may have to partition the movies into subsets such as short vs. long duration, or by genre, etc._

```{r}
# TODO: Investigate if Gross Revenue is related to Budget, Runtime or Genre
df <- df2
noNA.gross <- df[!is.na(df$Gross), ]

movieBudget <- noNA.gross[!is.na(noNA.gross$Budget),]
movieBudget$Budget <- log(movieBudget$Budget, 10)
movieBudget$Gross <- movieBudget$Gross / 1000000

p5_1 <- ggplot(data = movieBudget, aes(x = Budget, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "log10(Budget)", breaks = seq(2, 10, 1), limits=c(2, 10)) + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 5.1: Relation between movie budget and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieBudget$Gross, movieBudget$Budget))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p5_1 <- p5_1 + annotate("text", x = 3, y = 2500, label = lm_text)
p5_1
```

```{r}
noNA.gross <- df[!is.na(df$Gross), ]

movieBudget <- noNA.gross[!is.na(noNA.gross$Budget),]
movieBudget$Budget <- log(movieBudget$Budget, 10)
movieBudget$Gross <- movieBudget$Gross / 1000000
movieBudget <- movieBudget[movieBudget$Budget >= 6, ]

p5_1.1 <- ggplot(data = movieBudget, aes(x = Budget, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "log10(Budget)") + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 5.1.1: Relation between movie budget (>1M) and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieBudget$Gross, movieBudget$Budget))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p5_1.1 <- p5_1.1 + annotate("text", x = 7, y = 2500, label = lm_text)
p5_1.1
```
```{r}
# Runtime

movieRuntime <- noNA.gross[!is.na(noNA.gross$Runtime),]
movieRuntime$Gross <- movieRuntime$Gross / 1000000

p5_2 <- ggplot(data = movieRuntime, aes(x = Runtime, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Runtime", breaks = seq(0, 400, 50), limits=c(0, 400)) + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 5.2: Relation between movie runtime and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieRuntime$Gross, movieRuntime$Runtime))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p5_2 <- p5_2 + annotate("text", x = 100, y = 2500, label = lm_text)
p5_2

```
```{r}
# Top 10 Genres

plots <- list()  # new empty list
for (i in 1:10) {
    df_distribution <- noNA.gross[noNA.gross[colnames(df_genres_top10)[i]] > 0, ]
    df_distribution$Gross <- df_distribution$Gross / 1000000
    p1 <- ggplot(df_distribution, aes(x = Gross)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=200) + scale_x_continuous(name = "Gross Revenue (millions)") + scale_y_continuous(name = "Density") + geom_density(alpha=.2, fill="#FF6666") + ggtitle(colnames(df_genres_top10)[i])
    plots[[i]] <- p1  # add each plot into plot list
}
multiplot(plotlist = plots, cols = 3)



```

**Q**: Did you find any observable relationships or combinations of Budget/Runtime/Genre that result in high Gross revenue? If you divided the movies into different subsets, you may get different answers for them - point out interesting ones.

**A**: 

1. As far as the "Budget" variable was concerned, if the budgets are less than 1M dollors, the gross revenue are similar and around 1M dollors. If the budgets are greater than 1M dollors, there are moderate positive correlations between log(budgets) and gross revenues, which indicates a potential exponential relationship between budgets and gross revenues.

2. From Figure 5.2 I could ont observe clear relationship between movie runtimes and gross revenues.

3. I choose the top 10 genres of movies and made the histogram of gross revenues in Figure 5.3:
1) Action movies and Adventure movies are easier to make more gross revenues than other genres.
2) Documentary movies has least posibilty to result in high gross revenue, most of their gross revenues are less than 1M dollors.
3) There are obvious peaks in the distribution of gross revenues of Short movies, and some short movies even have gross revenues less than 0.

```{r}
# TODO: Investigate if Gross Revenue is related to Release Month
df_noNA_release <- df[!is.na(df$Released),]
released.date <- as.character(df_noNA_release$Released)

released.month <- sapply(strsplit(released.date,"-"),
  function(x) {
    as.numeric(x[2])
    }
)
df_noNA_release$Month <- as.factor(released.month)

df_noNA_release$Gross <- df_noNA_release$Gross / 1000000

fill <- "#4271AE"
line <- "#1F3552"

p5_4 <- ggplot(df_noNA_release, aes(x = Month, y = Gross)) + 
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "Month") + 
        scale_y_continuous(name = "Gross Revenue (millions)") +
        ggtitle("Figure 5.4: Boxplot of gross revenue by released month") +
        theme_bw()

p5_4

```

## 6. Process `Awards` column

The variable `Awards` describes nominations and awards in text format. Convert it to 2 numeric columns, the first capturing the number of wins, and the second capturing nominations. Replace the `Awards` column with these new columns, and then study the relationship of `Gross` revenue with respect to them.

_Note: The format of the `Awards` column is not standard; you may have to use regular expressions to find the relevant values. Try your best to process them, and you may leave the ones that don't have enough information as NAs or set them to 0s._

```{r}
# TODO: Convert Awards to 2 numeric columns: wins and nominations

wins <- c()
nominations <- c()

counter1 <- 0
counter2 <- 0
counter3 <- 0

for (i in seq(1,length(df$Awards),1)) {
    
    a <- 0
    b <- 0
    
    if (is.na(df$Awards[i])){
      wins <- c(wins, a)
      nominations <- c(nominations, b)
      next
    }

    if(grepl("win", df$Awards[i]) & grepl("nomination", df$Awards[i])){
      
      a <- sapply(strsplit(df$Awards[i]," "),function(x){as.numeric(x[1])})
      b <- sapply(strsplit(df$Awards[i]," "),function(x){as.numeric(x[4])})
      counter1 <- counter1 + 1
      
    }
    else if(grepl("win", df$Awards[i])){
      a <- sapply(strsplit(df$Awards[i]," "),function(x){as.numeric(x[1])})
      b <- 0
      counter2 <- counter2 + 1
    }
    else if(grepl("nomination", df$Awards[i])){
       a <- 0
       b <- sapply(strsplit(df$Awards[i]," "),function(x){as.numeric(x[1])})
       counter3 <- counter3 + 1
    }
    else {
      a <- 0
      b <- 0
    }
    
    wins <- c(wins, a)
    nominations <- c(nominations, b)
}


```
```{r}
df$Wins <- wins
df$Nominations <- nominations
length(wins)
length(nominations)
nrow(df)
counter1
counter2
counter3
counter2 + counter3

```

**Q**: How did you construct your conversion mechanism? How many rows had valid/non-zero wins or nominations?

**A**: 
1. Check each element in award vector.
2. If it is NA, assigning 0 to both win and nomination
3. If the string contains both "win" and "nomination", extracting two values
4. If the string contains "win" or "nomination", only extrating one value and assigning the other one = 0.
5. For the rest cases, assigning 0 to both win and nomination
6. 2322 rows had valid/non-zero wins and nominations.
7. 924 rows had valid/non-zero wins or nominations.

```{r}
# TODO: Plot Gross revenue against wins and nominations
movieWins <- df[!is.na(df$Wins),]
movieWins$Gross <- movieWins$Gross / 1000000

p6_1 <- ggplot(data = movieWins, aes(x = Wins, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Award Wins") + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 6.1: Relation between movie award wins and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieWins$Gross, movieWins$Wins))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p6_1 <- p6_1 + annotate("text", x = 10, y = 2000, label = lm_text)
p6_1
```
```{r}
movieNominations <- df[!is.na(df$Nominations),]
movieNominations$Gross <- movieNominations$Gross / 1000000

p6_2 <- ggplot(data = movieNominations, aes(x = Nominations, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Award Nominations") + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 6.2: Relation between movie award nominations and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieNominations$Gross, movieNominations$Nominations))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p6_2 <- p6_2 + annotate("text", x = 10, y = 2000, label = lm_text)
p6_2
```


**Q**: How does the gross revenue vary by number of awards won and nominations received?

**A**: From the above figurs I could not observe a clear relationship between number of awards won / nominations received and gross revenue vary.

## 7. Movie ratings from IMDb and Rotten Tomatoes

There are several variables that describe ratings, including IMDb ratings (`imdbRating` represents average user ratings and `imdbVotes` represents the number of user ratings), and multiple Rotten Tomatoes ratings (represented by several variables pre-fixed by `tomato`). Read up on such ratings on the web (for example [rottentomatoes.com/about](https://www.rottentomatoes.com/about) and [ www.imdb.com/help/show_leaf?votestopfaq](http:// www.imdb.com/help/show_leaf?votestopfaq)).

Investigate the pairwise relationships between these different descriptors using graphs.

```{r}
# TODO: Illustrate how ratings from IMDb and Rotten Tomatoes are related
movieIMDB <- df[!is.na(df$imdbRating),]

p7_1 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = tomatoRating)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Tomato Rating") +
        		ggtitle("Figure 7.1: Relation between IMDB and Tomato rating") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$tomatoRating))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p7_1 <- p7_1 + annotate("text", x = 2.5, y = 7.5, label = lm_text)
p7_1

```
```{r}
movieIMDB <- df[!is.na(df$imdbRating),]
movieIMDB$tomatoFreshRate <- movieIMDB$tomatoFresh / movieIMDB$tomatoReviews

p7_2 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = tomatoFreshRate)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Tomato Fresh Rate") +
        		ggtitle("Figure 7.2: Relation between IMDB and Tomato fresh rate") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$tomatoFreshRate))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p7_2 <- p7_2 + annotate("text", x = 2.5, y = 1, label = lm_text)
p7_2
```
```{r}
movieIMDB <- df[!is.na(df$imdbRating),]
movieIMDB$tomatoRottenRate <- movieIMDB$tomatoRotten / movieIMDB$tomatoReviews

p7_3 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = tomatoRottenRate)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Tomato Rotten Rate") +
        		ggtitle("Figure 7.3: Relation between IMDB and Tomato rotten rate") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$tomatoRottenRate))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p7_3 <- p7_3 + annotate("text", x = 2.5, y = 2, label = lm_text)
p7_3
```
```{r}
movieIMDB <- df[!is.na(df$imdbRating),]

p7_4 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = tomatoUserMeter)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Tomato User Meter") +
        		ggtitle("Figure 7.4: Relation between IMDB and Tomato user meter") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$tomatoUserMeter))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p7_4 <- p7_4 + annotate("text", x = 2.5, y = 100, label = lm_text)
p7_4
```

```{r}
movieIMDB <- df[!is.na(df$imdbRating),]

p7_5 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = tomatoUserRating)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Tomato User Rating") +
        		ggtitle("Figure 7.5: Relation between IMDB and Tomato user rating") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$tomatoUserRating))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p7_5 <- p7_5 + annotate("text", x = 2.5, y = 5, label = lm_text)
p7_5
```


**Q**: Comment on the similarities and differences between the user ratings of IMDb and the critics ratings of Rotten Tomatoes.

**A**: According to my observations, IMDb rating has strong postive correlation with Tomato Rating, Fresh Rate (Number of Fresh / Total Reviews), User Meter and User Rating, respectively, accordint to the R^2 values reported on the above figures. Also the IMDb rating has a negative linear relationship with Tomato Rotten Rate (Number of Rotten / Total Reviews).

## 8. Ratings and awards

These ratings typically reflect the general appeal of the movie to the public or gather opinions from a larger body of critics. Whereas awards are given by professional societies that may evaluate a movie on specific attributes, such as artistic performance, screenplay, sound design, etc.

Study the relationship between ratings and awards using graphs (awards here refers to wins and/or nominations). 

```{r}
# TODO: Show how ratings and awards are related

movieIMDB <- df[!is.na(df$imdbRating),]

p8_1 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = Wins)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Award Won") +
        		ggtitle("Figure 8.1: Relation between IMDB and award won") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$Wins))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p8_1 <- p8_1 + annotate("text", x = 2.5, y = 30, label = lm_text)
p8_1
```
```{r}
movieIMDB <- df[!is.na(df$imdbRating),]

p8_2 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = Nominations)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Award Nominations") +
        		ggtitle("Figure 8.2: Relation between IMDB and award nominations") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$Nominations))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p8_2 <- p8_2 + annotate("text", x = 2.5, y = 75, label = lm_text)
p8_2
```
```{r}
movieTOMATO <- df[!is.na(df$tomatoRating),]

p8_3 <- ggplot(data = movieTOMATO, aes(x = tomatoRating, y = Wins)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Tomato Rating") + 
        		scale_y_continuous(name = "Award Won") +
        		ggtitle("Figure 8.3: Relation between Tomato rating and award won") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieTOMATO$tomatoRating, movieTOMATO$Wins))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p8_3 <- p8_3 + annotate("text", x = 2.5, y = 30, label = lm_text)
p8_3
```

```{r}
movieTOMATO <- df[!is.na(df$tomatoRating),]

p8_4 <- ggplot(data = movieTOMATO, aes(x = tomatoRating, y = Nominations)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "TOMATO Rating") + 
        		scale_y_continuous(name = "Award Nominations") +
        		ggtitle("Figure 8.4: Relation between Tomato rating and award nominations") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieTOMATO$tomatoRating, movieTOMATO$Nominations))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p8_4 <- p8_4 + annotate("text", x = 2.5, y = 30, label = lm_text)
p8_4
```

**Q**: How good are these ratings in terms of predicting the success of a movie in winning awards or nominations? Is there a high correlation between two variables?

**A**: From question 7 the conclusion is IMDB rating and Tomoto rating results are highly correlated. Thus here I only chose imdbRating and tomatoRating as the rating variables, and comparing them with award won and award nominations. Figure 8.1 ~ 8.4 show that the performances of these ratings were not good in 
in terms of predicting the success of a movie in winning awards or nominations. And there is no high correlation between ratings and award won and/or award nomination times.

## 9. Expected insights

Come up with two new insights (backed up by data and graphs) that is expected. Here “new” means insights that are not an immediate consequence of one of the above tasks. You may use any of the columns already explored above or a different one in the dataset, such as `Title`, `Actors`, etc.

```{r}
# TODO: Find and illustrate two expected insights
movie <- df[!is.na(df$Domestic_Gross),]
movie$Gross <- movie$Gross / 1000000
movie$Domestic_Gross <- movie$Domestic_Gross / 1000000

p9_1 <- ggplot(data = movie, aes(x = Domestic_Gross, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "Domestic Gross Revenue (millions)") + 
        		scale_y_continuous(name = "Gross Revenue (millions)") +
        		ggtitle("Figure 9.1: Relation between IMDB domestic gross and total gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movie$Gross, movie$Domestic_Gross))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p9_1 <- p9_1 + annotate("text", x = 100, y = 2000, label = lm_text)
p9_1
```

**Q**: Expected insight #1.

**A**: I expect there is a high correlation between domestic gross revenue and total gross revenue, and Figure 9.1 shows these two variables do have strong positive linear relationship. It indicates the domestic customers contribute a lot to the gross revenue of a movie.

```{r}
movie <- df[!is.na(df$Rated),]
movie <- movie[movie$Rated != "N/A", ]
movie <- movie[movie$Rated != "NOT RATED", ]
movie <- movie[movie$Rated != "UNRATED", ]
movie$Gross <- movie$Gross / 1000000

fill <- "#4271AE"
line <- "#1F3552"

p9_2 <- ggplot(movie, aes(x = Rated, y = Gross)) + 
        geom_boxplot(fill = fill, colour = line, alpha = 0.7,
                     outlier.colour = "#1F3552", outlier.shape = 20) +
        scale_x_discrete(name = "Rated") + 
        scale_y_continuous(name = "Gross Revenue (millions)") +
        ggtitle("Figure 9.2: Boxplot of gross revenue by rated") +
        theme_bw()

p9_2
```

**Q**: Expected insight #2.

**A**: Figure 9.2 shows the boxplot of gross revenue by movie rate.
1) All movie rates have similar mean of gross revenue values, but the gross revenue values of 
PG13 are more variable than others.
2) PG13 has the maximum value of gross revenue.


## 10. Unexpected insight

Come up with one new insight (backed up by data and graphs) that is unexpected at first glance and do your best to motivate it. Same instructions apply as the previous task.

```{r}
# TODO: Find and illustrate one unexpected insight
movieIMDB <- df[!is.na(df$imdbRating),]
movieIMDB$Gross <- movieIMDB$Gross / 1000000

p10_1 <- ggplot(data = movieIMDB, aes(x = imdbRating, y = Gross)) +
				geom_smooth(method = "lm", se=FALSE, color="#4271AE", formula = y ~ x) +
				scale_x_continuous(name = "IMDB Rating") + 
        		scale_y_continuous(name = "Gross Revenue (millions") +
        		ggtitle("Figure 10.1: Relation between IMDB rating and gross revenue") + 
				geom_point() + theme_bw()

df1 <- as.data.frame(cbind(movieIMDB$imdbRating, movieIMDB$Gross))
colnames(df1) <- c("y", "x")

m <- lm(y ~ x, df1);
r2 = format(summary(m)$r.squared, digits = 3)
lm_text <- as.character(paste("R^2 = ", r2))

p10_1 <- p10_1 + annotate("text", x = 2.5, y = 2000, label = lm_text)
p10_1
```

**Q**: Unexpected insight.

**A**: Figure 10.1 shows the relationship bewteen IMDB rating and gross revenue. My expectation is movie has higher IMDB Rating means the audiances have higher satisfactions, and consequently it will cause higher gross revenue. Yet from Figure 10.1 I could not see clear correlation between IMDB user ratings and movie gross revenue.

