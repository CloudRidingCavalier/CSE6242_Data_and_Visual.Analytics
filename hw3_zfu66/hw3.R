#######################
##
## CSE6242: Data & Visual Analytics / Spring 2018
## Name: Zheng Fu / GT account name: zfu66@gatech.edu
## Homework 3
##
#######################   

# Read training dataset

train <- read.csv("./mnist/mnist_train.csv", header=FALSE)

# partitioning training dataset

train_data_0_1 <- cbind(train[, train[785,] == 0], train[, train[785,] == 1])
train_data_3_5 <- cbind(train[, train[785,] == 3], train[, train[785,] == 5])

train_labels_0_1 <- train_data_0_1[785, ]
train_labels_3_5 <- train_data_3_5[785, ]

train_data_0_1 <- train_data_0_1[-785, ]
train_data_3_5 <- train_data_3_5[-785, ]

# Read testing dataset

test <- read.csv("./mnist/mnist_test.csv", header=FALSE)

# partitioning testing dataset

test_data_0_1 <- cbind(test[, test[785,] == 0], test[, test[785,] == 1])
test_data_3_5 <- cbind(test[, test[785,] == 3], test[, test[785,] == 5])

test_labels_0_1 <- test_data_0_1[785, ]
test_labels_3_5 <- test_data_3_5[785, ]

test_data_0_1 <- test_data_0_1[-785, ]
test_data_3_5 <- test_data_3_5[-785, ]

# define rotate function

rotate <- function(x) t(apply(x, 1, rev))

# plot image with label = 0 in train_data_0_1

figure0.index = 5000
figure0 = matrix(train_data_0_1[, figure0.index], nrow=28, byrow=TRUE)
image(rotate(figure0), col=gray(0:255/255), main = paste("Figure true label is:", train_labels_0_1[figure0.index]))

# plot image with label = 1 in train_data_0_1

figure1.index = 9000
figure1 = matrix(train_data_0_1[, figure1.index], nrow=28, byrow=TRUE)
image(rotate(figure1), col=gray(0:255/255), main = paste("Figure true label is:", train_labels_0_1[figure1.index]))

# plot image with label = 3 in train_data_3_5

figure2.index = 5000
figure2 = matrix(train_data_3_5[, figure2.index], nrow=28, byrow=TRUE)
image(rotate(figure2), col=gray(0:255/255), main = paste("Figure true label is:", train_labels_3_5[figure2.index]))


# plot image with label = 5 in train_data_3_5

figure3.index = 9000
figure3 = matrix(train_data_3_5[, figure3.index], nrow=28, byrow=TRUE)
image(rotate(figure3), col=gray(0:255/255), main = paste("Figure true label is:", train_labels_3_5[figure3.index]))




