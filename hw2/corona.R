# q1.1:

corona_df <- read.csv("hw2/data_corona_world.csv")
head(corona_df)

# q1.2:

filterd_df <- subset(corona_df, Country %in% c("Italy", "China"))
head(filterd_df)

# q1.3:

str(filterd_df)

# q1.4:

china_data <- subset(filterd_df, Country == 'China',
                     select = c(Confirmed.cases, Confirmed.fatalities))
china_data <- colSums(china_data)
china_survivors <- china_data["Confirmed.cases"] - china_data["Confirmed.fatalities"]

italy_data <- subset(filterd_df, Country == 'Italy',
                     select = c(Confirmed.cases, Confirmed.fatalities))
italy_data <- colSums(italy_data)
italy_survivors <- italy_data["Confirmed.cases"] - italy_data["Confirmed.fatalities"]

contingency_table <- rbind(
  c(china_survivors, china_data["Confirmed.fatalities"]),
  c(italy_survivors, italy_data["Confirmed.fatalities"])
)

rownames(contingency_table) <- c("China","Italy")
colnames(contingency_table) <- c("Survivor","Death")

print(contingency_table)

mosaicplot(contingency_table, color = TRUE,
           main = "Contingency Table: China vs. Italy")


# q1.5:

china_dp <- (contingency_table["China", "Death"] / sum(contingency_table["China", ])) * 100
print(china_dp)

italy_dp <- (contingency_table["Italy", "Death"] / sum(contingency_table["Italy", ])) * 100
print(italy_dp)

# q1.6:

death_rate_df <- data.frame(
  Country = c("China", "Italy"),
  DeathRate = c(china_dp, italy_dp)
)

library(lattice)
barchart(DeathRate ~ Country, 
         data = death_rate_df, 
         main = "Death Rate by Country", 
         xlab = "Country", 
         ylab = "Death Rate (%)",
         ylim = c(0,5)
         )

# barchart by Country & Age:

df_age_china <- subset(filterd_df, Country == 'China',
                 select = c(Age.group, Confirmed.cases, Confirmed.fatalities))

df_age_china$DeathRatio <- (df_age_china$Confirmed.fatalities / (df_age_china$Confirmed.cases + df_age_china$Confirmed.fatalities)) * 100
df_age_china$Country <- "China"


df_age_italy <- subset(filterd_df, Country == 'Italy',
                       select = c(Age.group, Confirmed.cases, Confirmed.fatalities))

df_age_italy$DeathRatio <- (df_age_italy$Confirmed.fatalities / (df_age_italy$Confirmed.cases + df_age_italy$Confirmed.fatalities)) * 100
df_age_italy$Country <- "Italy"


death_rate_age <- rbind(df_age_china, df_age_italy)
death_rate_age
death_rate_age$Age.group <- factor(
  death_rate_age$Age.group,
  levels = c("0-9", " 10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  ordered = TRUE
)


barchart(DeathRatio ~ Age.group,
         group = Country,
         data = death_rate_age,
         auto.key = list(space = "right"),
         main = "Death Rate by Country & Age",
         xlab = "Age Group",
         ylab = "Death Rate (%)"
         )