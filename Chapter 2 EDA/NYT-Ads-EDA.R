install.packages("doBy")
install.packages("ggplot2")

library("doBy")
library("ggplot2")


# Read data in, make sure numeric labels are treated as labels
# and not as continuous variables
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
data1$Gender = factor(data1$Gender)
data1$Signed_In = factor(data1$Signed_In)

age_breaks <- c(-Inf, 0, 17, 24, 34, 44, 54, 64, Inf)
age_group <- c("0", "1-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
data1$Age_split <- cut(data1$Age, age_breaks, age_group)

# Initially appears many more female users than male
# Many users have age reported as 0
# ~70% of users are logged in
summary(data1)

# Grouping results by age groups to check age=0 category
# shows all age=0 users are listed as female, and not signed in.
# Assume the age/gender are default values when not signed in.
# Any age or gender comparisons will need to be done on subset
# of users who are signed in.  
summaryBy(Gender+Signed_In+Clicks+Impressions~Age_split, data=data1)

# Average user will see 5 ads
# The youngest and oldest users tend have the highest total
# number of clicks, similar to anonymous users (not logged in)
aggregations <- function(x){c(length(x), min(x), mean(x), max(x), sum(x))}
function_names = c("length", "min", "mean", "max", "sum")
summaryBy(Impressions~Age_split, data=data1, FUN=aggregations, fun.name=function_names)
summaryBy(Clicks~Age_split, data=data1, FUN=aggregations, fun.name=function_names)

logged_in = subset(data1, data1$Signed_In==1)

# see roughly normal distribution of ages
# could re-bin 65+ category to more similar size as other bins
# Generally see slightly more male users than female, except in
# 65+ category.  Due to higher longevity in women? Less interest from older men?
ggplot(logged_in, aes(x=Age_split, fill=Gender))+ geom_histogram(binwidth=1)





