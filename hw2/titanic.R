# q3:

df_titanic <- read.csv("hw2/titanic.csv")
head(df_titanic)

# q3.a:
logic_vec <- !colnames(df_titanic) %in% c("Cabin", "Ticket")

df_titanic <- df_titanic[,logic_vec]
head(df_titanic)

# q3.b:
completed_rows <- complete.cases(df_titanic)

df_titanic <- df_titanic[completed_rows, ]
head(df_titanic)

# q3.c:
fare_summary <- aggregate(Fare ~ Pclass, data = df_titanic, FUN = summary)
print(fare_summary)

boxplot(Fare ~ Pclass, data = df_titanic,
        main = "Fare by Passenger Class",
        xlab = "Pclass", ylab = "Fare",
        col = c("lightblue", "lightgreen", "salmon"),
        ylim = c(0,600))

df_titanic$logFare <- log(df_titanic$Fare + 1)


boxplot(logFare ~ Pclass, data = df_titanic,
        main = "Fare by Passenger Class",
        xlab = "Pclass", ylab = "Fare (log)",
        col = c("lightblue", "lightgreen", "salmon"),
        ylim = c(0,10)
        )

variance_by_class <- aggregate(Fare ~ Pclass, data = df_titanic, FUN = var)
print(variance_by_class)

# q3.d:

table_survival <- table(df_titanic$Survived, df_titanic$Pclass)
rownames(table_survival) <- c("Died", "Survived")
colnames(table_survival) <- c("Class 1", "Class 2", "Class 3")
mosaicplot(table_survival,
           color = TRUE,
           main = "Survival by Passenger Class"
           )

# q3.d.1:

colSums(table_survival)
rowSums(table_survival)

# q3.d.2:

row_total <- c(424, 290) # Died, Survived
col_total <- c(186, 173, 355) # Class 1,2,3
total <- sum(row_total)

expected <- matrix(0, nrow = 2, ncol = 3)

for (i in 1:2) {
  for (j in 1:3) {
    expected[i,j] <- (row_total[i] * col_total[j]) / total
  }
}

rownames(expected) <- c("Died", "Survived")
colnames(expected) <- c("Class 1", "Class 2", "Class 3")

print(expected)

# q3.d.3:
x <- 0
for (i in 1:2) {
  for (j in 1:3) {
    x <- x + ((table_survival[i,j] - expected[i,j]) ^ 2) / expected[i,j]
  }
}

print(x)

# q.e:

survival_ratio <- table_survival[2, ] / (table_survival[1, ] + table_survival[2, ])
print(survival_ratio)

# q.f:

boxplot(Age ~ Survived,
        names = c("Died", "Survived"),
        data = df_titanic,
        ylab = "Age",
        xlab = "Survival Status",
        col = c("salmon", "lightgreen"),
        main = "Age Distribution by Survival"
        )

boxplot(Age ~ Survived + Pclass, 
        data = df_titanic,
        names = c("Died-1st", "Survived-1st", 
                  "Died-2nd", "Survived-2nd", 
                  "Died-3rd", "Survived-3rd"),
        col = c("salmon", "lightgreen"),
        xlab = "Survival Status by Class",
        ylab = "Age",
        main = "Age Distribution by Survival and Class"
        )

