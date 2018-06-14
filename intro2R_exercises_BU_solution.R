# Make a directory on your computer (BONUS: do this at the command line)
# Set your working directory to that directory
setwd("~/Documents/daviesLab/codingHour/") # <------ this should be the path to YOUR directory

# PART 1: Analyzing Donut Consumption ----

# Create a vector of 20 random numbers ranging from 1 to 10. Call it `donuts_week1`.
set.seed(1) # <---- I'm using "set.seed" to make sure the numbers I randomly generate are the same when you do it
donuts_week1 <- sample(1:10, 20, replace=T)

# Create a vector of 20 random numbers ranging from 5 to 15. Call it `donuts_week2`.
set.seed(1) # <----- you have to "set the seed" every time you call a random operation
donuts_week2 <- sample(5:15, 20, replace = T)

# Summarize the `donuts_week1` and `donuts_week2` objects using the `summary` function
summary(donuts_week1)
summary(donuts_week2)


# What is the data type of the `donuts_week2` and `donuts_week1` objects?

# There are lots of ways to do it...
typeof(donuts_week1)
typeof(donuts_week2)

class(donuts_week1)
class(donuts_week2)

str(donuts_week2)
str(donuts_week1)

# Make a histogram of `donuts_week1`. Title the histogram "Donut Consumption at Peer Coding Hour". Call the x-axis "Number of Donuts Eaten by Peer Coders".
hist(donuts_week1, main = "Donut Consumption at Peer Coding Hour", xlab = "Number of Donuts Eaten by Peer Coders")

# Plot a histogram of `donuts_week2` on top of a histogram of `donuts_week1`.
# HINT: you may want to play around with colors here. You'll definitely need to adjust the axes to see all of the data.
# BONUS: add a legend to the figure to discrimate between the histograms
hist(donuts_week1, main = "Donut Consumption at Peer Coding Hour", xlab = "Number of Donuts Eaten by Peer Coders", xlim = c(0,18), ylim = c(0,8), col=rgb(0,1,0,0.5))
hist(donuts_week2, col=rgb(1,0,0,0.25), add = T)
legend(x = 10, y = 9, c("Week 2", "Week 3"), col=c("green", "pink"), lwd = 5, bty = "n")

# Use a t-test to determine if there is a significant difference in the number of donuts consumed on week 1 vs. week 2
t.test(donuts_week1, donuts_week2, paired = F)

# PART 2: Relationship between Donut and Coffee Consumption ----
# Create a vector of 20 random numbers ranging from 1 to 10. Call it `coffee_week1`.
set.seed(1)
coffee_week1 <- sample(1:10, 20, replace=T)

# Examine this data in some way. Coders choice!
hist(coffee_week1)
summary(coffee_week1)
str(coffee_week1)
table(coffee_week1)
max(coffee_week1)
min(coffee_week1)

# Create a data frame called `coffee_donut_data` that has one column for `donuts_week1` and one column for `coffee_week1`
coffee_donut_data <- data.frame(donuts_week1, coffee_week1)

# Enter a line of code that tells you what type of object `coffee_donut_data` is and what type of data it contains (integers? characters?)
str(coffee_donut_data)

# Plot donuts consumed on the x-axis and coffee consumed on the y-axis. Give the plot a name. Label the x- and y-axes.
plot(coffee_week1~donuts_week1,data=coffee_donut_data, main = "Coffee~Donut Consumption Relationship", ylab = "Coffee Consumed (cups)", xlab = "Donuts Consumed in Week 1")

# Use a linear model to test the relationship between coffee and donut consumption. Save the results to a variable called `lm_coffee_donuts`.
lm_coffee_donuts <- lm(coffee_week1~donuts_week1,data=coffee_donut_data)

# Summarize `lm_coffee_donuts`
summary(lm_coffee_donuts)

# Add a regression line to the plot you made earlier using this linear model.
plot(coffee_week1~donuts_week1,data=coffee_donut_data, main = "Coffee~Donut Consumption Relationship", ylab = "Coffee Consumed (cups)", xlab = "Donuts Consumed in Week 1")
abline(lm_coffee_donuts, col = "red")
