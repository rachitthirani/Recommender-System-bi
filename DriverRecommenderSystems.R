
# Session --> Set Working Dir -> To Source File Location
source("LoadLibraries.R")

#---------------------------------------------------------
# STEP 1: Load the data and examine its summary statistics
#---------------------------------------------------------
data.pkg <- data(package = "recommenderlab")
data.pkg
str(data.pkg)
data.pkg$results[, "Item"]

data(MovieLense)
data <- MovieLense
print(data)
rm(MovieLense)

#---------------------------------------------
# STEP 2: Examine basic info about the data structure
#   and statistics of the data
#---------------------------------------------
# Matrix is stored as Sparse Matrix (many zero's), realRatingMatrix
# object.size() should show ~10X less space than dense matrix

class(data)
object.size(data)
object.size(as(data, "matrix"))
object.size(as(data, "matrix")) / object.size(data)

# If you have a regular matrix, user.item.matrix,
# here is how to coerse to a realRatingMatrix:
# rating.matrix <- as (user.item.matrix, "realRatingMatrix")

# Examine basic statistics
# Note that only statistics for a slice of data
# is being displayed for ease of viewing
dim(data)
slotNames(data)
dimnames(data[1:5, 1:3]) # the names of the movies as columns
rowCounts(data)
length(rowCounts(data))
colCounts(data) # number of non-missing col values
length(colCounts(data))
rowMeans(data[1:5, ])
colMeans(data[, 1:3])

# access the matrix of rating values
# note that the class is dgCMatrix
# that inherits from Matrix 
ratings.matrix <- data@data
class(ratings.matrix)

# unique values of ratings
vec.ratings <- as.vector(ratings.matrix)
unique(vec.ratings)

# histogram of ratings or display counts
hist(getRatings(data))
table.ratings <- table(vec.ratings)
table.ratings

# note that rating of zero means missing value
# remove missing values; convert them into categories
# and view the chart using methods from ggplot2 package
# Remomber that ggplot adds layers of information
# to the image display using the '+' sign
vec.ratings <- vec.ratings[vec.ratings != 0]
vec.ratings <- factor(vec.ratings)
qplot(vec.ratings) + ggtitle("Rating Distribution")

# Filter and sort Items by the number of ratings
ratings.per.item <- colCounts(data)
table.ratings <- data.frame(item = names(ratings.per.item),
                            ratings = ratings.per.item)
sort.table.ratings <- table.ratings [order (table.ratings$ratings,
                                            decreasing = TRUE), ]

# Visualize the top five most rated Items
ggplot(sort.table.ratings[1:5, ],
       aes(x = item, y = ratings)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Number of Ratings of the Top Items")

# Examine the average ratings
avg.ratings <- colMeans(data)
qplot(avg.ratings) + stat_bin(binwidth = 0.1) +
  ggtitle("Average Rating Distribution")

# Examine the average ratings
# Note the extreme ratings: 1 or 5
avg.ratings <- colMeans(data)
qplot(avg.ratings) + stat_bin(binwidth = 0.1) +
  ggtitle("Average Rating Distribution")

# Remove Items that have too few ratings
# Note how the highest values shifted from 3 to 4.
rating.threshold <- 100
avg.ratings.filtered <- avg.ratings[ratings.per.item > rating.threshold]
qplot(avg.ratings.filtered) + stat_bin(binwidth = 0.1) +
  ggtitle("Average Filtered Rating Distribution")

# Examine top 1% percent of Users and Items
min.items <- quantile(rowCounts(data), 0.99)
min.users <- quantile(colCounts(data), 0.99)
print(min.items)
print(min.users)

#--------------------------------------------
# STEP 3: Examine associations between Users
#   and Items, Users and Users, Items and Items
#-------------------------------------------
# View a user-item ratings
head (as(data, "data.frame"))
as(data[1:5, 1:3], "data.frame")

# visualize 20 user and their 15 item ratings
image (data[1:20, 1:15], 
       main = "Selected Raw User-Item Ratings")

# all User-Item associations
image (data, main = "All User-Item Raw Ratings" )

# top percentile of users and items
image(data[rowCounts(data) > min.items, 
           colCounts(data) > min.users], 
      main = "Heatmap of top percentile users and items")

# filter out users with less than 50 ratings
# and items with less than 100 user ratings
filtered.ratings <- data[rowCounts(data) > 50, 
                         colCounts(data) > 100]
dim(filtered.ratings)
dim(data)
  
# possibly remove the bias from the filtered ratings: 
# remove the users that mostly rank
# items as 1's or 5's using normalize() method;
# also, center the ratings so that the mean of the normalized
# ratings becomes zero

normalized.ratings <- normalize(filtered.ratings)
dim(normalized.ratings)
sum (rowMeans(normalized.ratings) > 0.00001)

# heatmap of normalized ratings for 
# the top 2% users and items
min.items <- quantile(rowCounts(normalized.ratings), 0.98)
min.users <- quantile(colCounts(normalized.ratings), 0.98)
min.items
min.users
image(normalized.ratings[rowCounts(normalized.ratings) > min.items,
                         colCounts(normalized.ratings) > min.users], 
      main = "Heatmap of Normalized Ratings")

#--------------------------------------------
# compute similarity between first 10 Users
# note that all reviews across all items are used
# return object is of class dist;
# that's why similarity to itself is of distance 0 and
# diagonal elements are zeros; the smaller the values
# the smaller the dissimilarity (distance) 
# note how similarity and dist are related:
# s=1/(1+d) or s=1-d depending on the measure

user.dist.matrix <- similarity(data[1:10, ],
                              method = "cosine",
                              which = "users")
options(digits=2)
as.matrix(user.dist.matrix)
class(user.dist.matrix)

# red is more similar; distance closer to zero
# Who user-1 is the most similar to?

heatmap(as.matrix(user.dist.matrix), 
      main  = "User Pairwise Similarity")


# Because obj is of class "dist"
# we can cluster the users based on this distance 
# here is hierarchical clustering method
# help(hclust)
hc <- hclust(user.dist.matrix)
plot(hc, hang = -1, main = "Clustering of Users")

# likewise, compute similarity between first 10 Items
item.dist.matrix <- similarity(data[, 1:10],
                               method = "cosine",
                               which = "items")
options(digits=2)
as.matrix(item.dist.matrix)
class(item.dist.matrix)
heatmap(as.matrix(item.dist.matrix), 
        main  = "Item Pairwise Similarity")
hc <- hclust(item.dist.matrix)
plot(hc, hang = -1, main = "Clustering of Items")

#-------------------------------------
# STEP 4: Buld a recommendation model
#-------------------------------------
# Examine what recommendation methods are
# available for realRatingMatrix
# IBCF: Item-Based Collaborative Filtering
# UBCF: User-Based Collaborative Filtering
# POPULAR: Item popularity based recommendation
# SVD: SVD approximation of User-Item matrix
# PCA: PCA approximation of covariance matrix
rec.models <- recommenderRegistry$get_entries(
               dataType = "realRatingMatrix")
names(rec.models)

# Examine parameters for a particular model
# Note on the parameters:
#  k: k-top most similar items to keep for each Item
# method: Similarity function, such as cosine
rec.models$IBCF_realRatingMatrix$parameters

# Build IBCF: Item-Based Collaborative Filtering Model
# IBCF performs the following:
# 1. For each pair of Items, measures how similar
#    they are in terms of having similar ratings
#    by similar Users
# 2. For each Item, identify top-k most similar ones
# 3. For each User, identify items most similar to
#    his/her prior selections/purchases/views

# Create 80% training and 20% test data;
# see below how to do k-fold cross-validation
which.train <- sample (x = c(TRUE, FALSE),
                      size = nrow(filtered.ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
head(which.train)
train.data <- filtered.ratings[which.train, ]
test.data <- filtered.ratings[!which.train, ]

# Build Recommender Model from train.data
ibcf.model <- Recommender(data = train.data,
                     method = "IBCF",
                     parameter = list(k = 30))
ibcf.model
class(ibcf.model)
model.details <- getModel(ibcf.model)
model.details$description
model.details$k

# Item-Item similarity matrix; 
# only top-k neighbors are kept
dim(model.details$sim) 
head(model.details$sim)
image(model.details$sim[1:20, 1:20],
      main = "Heatmap of Item's Top-k (k=30) Neighbors")
row.sums <- rowSums(model.details$sim > 0)
table(row.sums)

# Note how there are few Items similar to  many others
col.sums <- colSums(model.details$sim > 0)
table(col.sums)
qplot(col.sums) + stat_bin(binwidth = 1) +
  ggtitle("Distribution of the Column Count")

# Which Items have the most neighbors
which.max <- order(col.sums, decreasing = TRUE) [1:10]
data.frame(rownames(model.details$sim) [which.max], 
           col.sums[which.max])

# Test the model on the test.data
# IBCF model Prediction process:
#   1. Extract Items rated by this target User
#   2. For each rated Item, extract its similar Items
#      from the Item similarity matrix (model.details$sim)
#   3. Rank each extracted Item:
#        - Item.weight = the User's rating of this Item
#        - Item.sim = the similarities between the Item
#                     and its top-k neighbor Items
#        - Item.weighted.sim = Item.weight * Item.sim
#   4. Sum weighted similarities across all extracted Items
#        - Sum = Sum (Item.weighted.sim) over extracted Items
#   5. Report top-R recommendations

# top recommendations
top.recommended <- 6
predictions <- predict(object = ibcf.model,
                       newdata = test.data,
                       n = top.recommended)

class(predictions)
slotNames(predictions)

# recommendations for the first user in test.data
usr.recommendations <- predictions@items[[1]]
usr.recommendations
data.frame(usr.recommendations,
           predictions@itemLabels[usr.recommendations])

# recommendations for all the users
usr.recom.matrix <- sapply(predictions@items,
                           function(usr){
                             colnames(filtered.ratings) [usr]
                           })
dim(usr.recom.matrix)
dim(test.data)
# recommendations for the first 2 users
usr.recom.matrix[, 1:2]

# Identify the most recommended Items;
# most items are recommended only a few times
# and a few items are recommended many times
number.of.recommendations <- factor(table(usr.recom.matrix))
qplot(number.of.recommendations) +
  ggtitle("Distribution of the Recommended Items by IBCF")

# Which recommendations are the most popular
sorted.recommendations <- sort(number.of.recommendations,
                               decreasing = TRUE)
number.top.recommendations <- head(sorted.recommendations, n=5)
data.frame(names(number.top.recommendations), 
           number.top.recommendations)

#--------------------
# Build UBCF: User-Based Collaborative Filtering Model
# UBCF performs the following:
# 1. Given a new user, identify his/her top-k most similar Users
#    based on cosine or correlation metric; or
#    make sure that similarity is above a certain threshold
# 2. Recommend the Items that are top-rated
#    by similar users:
#    - Computed weighted average using User-User similarity
#      from the above step as weights
# 3. Recommend top-rated items

# Examine UBCF model parameters
rec.models$UBCF_realRatingMatrix$parameters
ubcf.model <- Recommender(data = train.data,
                          method = "UBCF")
ubcf.model
model.details <- getModel(ubcf.model)
names(model.details)
model.details$description
model.details$nn
model.details$data

# Test UBCF model on test.data
top.recommended <- 6
predictions <- predict(object = ubcf.model,
                       newdata = test.data,
                       n = top.recommended)

usr.recom.matrix <- sapply(predictions@items,
                           function(usr){
                             colnames(filtered.ratings) [usr]
                           })
dim(usr.recom.matrix)
dim(test.data)
usr.recom.matrix[, 1:2]

# Identify the most recommended Items;
# most items are recommended only a few times
# and a few items are recommended many times
number.of.recommendations <- factor(table(usr.recom.matrix))
qplot(number.of.recommendations) +
  ggtitle("Distribution of the Recommended Items by UBCF")

# Which recommendations are the most popular
sorted.recommendations <- sort(number.of.recommendations,
                               decreasing = TRUE)
number.top.recommendations <- head(sorted.recommendations, n=4)
data.frame(names(number.top.recommendations), 
           number.top.recommendations)

#--------------
# k-fold cross-validation using for()-loop
# technically, for-loop could be converted to
# apply()
k <- 5
which.set <- sample(x = 1:k, 
                    size = nrow(filtered.ratings),
                    replace = TRUE)
for (i.model in 1:k) {
  which.train <- which.set == i.model
  train.data <- filtered.ratings[which.train, ]
  test.data <- filtered.ratings[!which.train, ]
  # Build Recommender Model on train.data
  
  # Test the model on test.data
}

#-------------------------------------
# STEP 5: Evaluate a recommendation model
#   General idea: 
#    - For each user in the test data,
#      define how many items to use to
#      generate recommendations (items.known). The remaining
#      items will be used to assess model
#      performance (accuracy, ROC, RMSE, etc)
#    - Define the threshold for user ratings (rating.threshold);
#      only items above the specified threshold
#      will be recommended
#   -  How many times to repeat the evaluation (n.evals)
#   -  How many items to recommend (items.to.recommend)
#     
#   Methods for creating Training and Testing Data 
#   A. Using k-fold cross-validation
#-------------------------------------

items.known <- 15
items.to.recommend <- 10
rating.threshold <- 3.0
n.evals <- 1
k.fold <- 4

model.to.evaluate <- "IBCF"
model.parameters <- NULL

#-------------------------------------
# A. Using k-fold cross-validation
#-------------------------------------

evaluation.sets <- evaluationScheme(data = filtered.ratings,
        method = "cross-validation",
        k = k.fold,
        given = items.known,
        goodRating = rating.threshold,
        k = n.evals)

evaluation.sets

eval.recommender <- Recommender (getData(evaluation.sets, "train"),
                                 method = model.to.evaluate,
                                 parameter = model.parameters)
eval.predictions <- predict (object = eval.recommender,
              newdata = getData(evaluation.sets, "known"),
              n = items.to.recommend,
              type = "ratings")

class(eval.predictions)

# how many items do we recommend to each user
qplot(rowCounts(eval.predictions)) + 
        geom_histogram(binwidth = 10) +
        ggtitle("Distribution of Items Recommended per User")

# compute performance metrics: per user, per fold, across folds
# RMSE: Root Mean Square Error -- the standard deviation of
#       the difference between the real and predicted ratings
# MSE: Mean Square Error: RMSE^2
# MAS: Mean Absolute Error: the mean of the absolute difference
#       between the real and predicted ratings

eval.accuracies <- calcPredictionAccuracy(
  x = eval.predictions, 
  data = getData(evaluation.sets, "unknown"),
  byUser = TRUE)

head(eval.accuracies)

# histogram of RMSE errors by Users
qplot(eval.accuracies[, "RMSE"]) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of RMSE by User")

# compute accuracies across all the users
eval.accuracies <- calcPredictionAccuracy(
  x = eval.predictions, 
  data = getData(evaluation.sets, "unknown"),
  byUser = FALSE)

eval.accuracies

# compute accuracies in terms of other metrics:
#  TP, FP, TN, FN, precision, recall, TPR, FPR
#  If the rating is above the threshold
#   (rating.threshold <- 3.0), then TRUE, else FALSE)
#  n: the number of items recommend to the user

eval.metrics <- evaluate(
  x = evaluation.sets,
  method = model.to.evaluate,
  n = seq(10, 100, 10))

class(eval.metrics)

# average of eval.metrics
avg (eval.metrics)

getConfusionMatrix(eval.metrics)[[1]]

plot(eval.metrics, annotate = TRUE, main = "ROC Curve")
plot (eval.metrics, "prec/rec", 
      annotate = TRUE, main = "Precision vs. Recall" )

# Compare multiple models
models.to.compare <- list (
  IBCF_cos = list(name = "IBCF", 
                  param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", 
                  param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", 
                  param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", 
                  param = list(method = "pearson")),
  random = list(name = "RANDOM", 
                  param = NULL)
  )

n.recommendations <- c(1, 5, seq(10, 100, 10))

result.list <- evaluate(
  x = evaluation.sets,
  method = models.to.compare,
  n = n.recommendations)

class(result.list)

class(result.list[[1]])

sapply(result.list, class) == "evaluationResults"

avg.matrices <- lapply(result.list, avg)

head(avg.matrices$IBCF_cos[, 5:8])

plot(result.list, annotate = 1,
     legend = "topleft")
     title("ROC Curves")

plot(result.list, "prec/rec", annotate = 1,
     legend = "topright")
     title("Precision vs. Recall")

# Optimizing the model parameters

k.choices <- c(5, 10, 20, 30, 40)
models.to.optimize <- lapply(
  k.choices, function(k){
    list(name = "IBCF",
         param = list(method = "cosine", k=k))
  })

names(models.to.optimize) <- paste0("IBCF_k_", k.choices)

# 
result.list <- evaluate(
  x = evaluation.sets,
  method = models.to.optimize,
  n = n.recommendations)


plot(result.list, annotate = 1,
     legend = "bottomright")
     title("ROC Curves for Diff Model Params")

plot(result.list, "prec/rec", annotate = 1,
     legend = "bottomright")
     title("Precision vs. Recall for Diff Model Params")


#---------------------------------
# B. Spliting data into training and testinf

train.percentage <- 0.8

evaluation.sets <- evaluationScheme(
  data = filtered.ratings,
  method = "split",
  given = items.known,
  goodRating = rating.threshold,
  k = n.evals)

evaluation.sets

getData(evaluation.sets, "train")

# confirm it is 80% training data
nrow (getData(evaluation.sets, "train")) / nrow(filtered.ratings)

# test data: 20%
getData(evaluation.sets, "known") 
nrow (getData(evaluation.sets, "known")) / nrow(filtered.ratings)
getData(evaluation.sets, "unknown")

# in the "known" test data, row counts=items.known
unique(rowCounts(getData(evaluation.sets, "known")))

# in the test data: the "unknown" ratings is
# the number of remaining ratings after "known"
# ratings have been used for training
unique(rowCounts(getData(evaluation.sets, "unknown")))
qplot(rowCounts(getData(evaluation.sets, "unknown"))) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of unknown items for each test used")

#-------------------------------------
# STEP 6: Check if binarizing the data 
#         produces better model
#-------------------------------------
# Convert the rating matrix into 0-1 matrix:
# value = 1 -- Item has been accesed by the user
#   and the user rated it
# value = 0 -- otherwise

binary.ratings <- binarize (filtered.ratings,
                            minRating = 3)
min.bin.items <- quantile(rowCounts(filtered.ratings), 0.95)
min.bin.users <- quantile(colCounts(filtered.ratings), 0.95)

image(binary.ratings[rowCounts(filtered.ratings) > min.bin.items,
                     colCounts(filtered.ratings) > min.bin.users],
      main = "Heatmap of binary ratings for top 5% users and items")

# how many Items are associated with each user
# based on distribution plot
# on average, each user is associated with 100 Items;
# a few are associated with more than 200 Items
binary.ratings <- binarize (filtered.ratings,
                            minRating = 1)
qplot(rowSums(binary.ratings)) + stat_bin(binwidth = 10) +
  geom_vline(xintercept = mean(rowSums(binary.ratings)),
             col = "red", linetype = "dashed") +
  ggtitle("Distribution of Items by Users: Binary Data")

# Build IBCF or UBCF Model for Binary Data
# NOTE: It is better to use "Jaccard" method
# to measure pairwise similarity; that's the only difference
# Because the actual ratings are not taken into account
# the results will be less accurate

which.train <- sample (x = c(TRUE, FALSE),
                       size = nrow(filtered.ratings),
                       replace = TRUE,
                       prob = c(0.8, 0.2))
train.data <- filtered.ratings[which.train, ]
test.data <- filtered.ratings[!which.train, ]

ibcf.binary.model <- Recommender(data = train.data,
                      method = "IBCF",
                      parameter = list(method = "Jaccard"))

#ubcf.binary.model <- Recommender(data = train.data,
#                                 method = "UBCF",
#                                 parameter = list(method = "Jaccard"))

model.details <- getModel(ibcf.binary.model)

# test the model
top.recommended <- 6
model <- ibcf.binary.model
# model <- ubcf.binary.model
predictions <- predict(object = model,
                       newdata = test.data,
                       n = top.recommended)

usr.recom.matrix <- sapply(predictions@items,
                           function(usr){
                             colnames(filtered.ratings) [usr]
                           })
dim(usr.recom.matrix)
dim(test.data)
usr.recom.matrix[, 1:2]
