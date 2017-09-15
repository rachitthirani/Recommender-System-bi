
if (!"recommenderlab" %in% rownames(installed.packages())) {
  install.packages("recommenderlab")
}

if (!"ggplot2" %in% rownames(installed.packages())) {
  install.packages("ggplot")
}

library(recommenderlab)
library(ggplot2)
