# hw1.R

# q2.2:

blood_types <- c("A", "AB", "B", "O")
frequencies <- c(104, 15, 46, 166)

barplot(
  frequencies,
  names.arg = blood_types,
  xlab = "Blood Type",
  ylab = "Number of Patients",
  col = "skyblue",
  main = "Blood Type Distribution Among 331 COVID-19 Patients"
)

# q2.3:

total <- sum(frequencies)
relative_freq <- round(frequencies / total, 3)

blood_table <- data.frame(
  Blood_Type = blood_types,
  Frequency = frequencies,
  Relative_Frequency = relative_freq
)

print(blood_table)

# q2.4:

most_common_type <- blood_table$Blood_Type[which.max(blood_table$Frequency)]
print(most_common_type)

# q3.1:

systolic_bp <- c(121, 113, 142, 137, 126, 124, 102, 112, 111, 113, 141)
median(systolic_bp)
mean(systolic_bp)
quantile(systolic_bp, 0.25, type = 6)
quantile(systolic_bp, 0.1, type = 6)

# q3.2:

# histogram:

hist(systolic_bp,
     breaks = 5,
     main = "Distribution of Systolic Blood Pressure",
     xlab = "Systolic Blood Pressure",
     col = "lightblue")

# boxplot:

boxplot(systolic_bp,
        main = "Boxplot of Systolic Blood Pressure",
        ylab = "Systolic Blood Pressure",
        col = "lightblue")

# q4.1:

MyQuant <- function(x, p) {
  sorted_x <- sort(x)
  n <- length(x)
  h <- p * (n+1)
  k <- floor(h)
  r <- h - floor(h)
  return(sorted_x[k] + r * (sorted_x[k+1] - sorted_x[k]))
}

# example:

set.seed(999)
p <- 0.5
num_vec <- rnorm(100)
MyQuant(num_vec, p)
median(num_vec)

# q4.2:

MyWeightedMean <- function(x,w) {
  
  if(sum(w < 0) > 0 || sum(w) != 1) {
    stop("Error")
  }
  return(sum(x * w))
}

 # example:

x <- c(1,2,3)
w <- c(1/3,1/3,1/3)
MyWeightedMean(x,w)

# q4.3:

MyTrimMean <- function(x,a) {
  n <- length(x)
  x_sorted <- sort(x)
  k <- floor(n * a)
  x_trimmed <- x_sorted[(k+1):(n-k)]
  return(sum(x_trimmed)/(n-2*k))
}

# example:

y <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
MyTrimMean(y, 0.1)
mean(y, trim = 0.1)




