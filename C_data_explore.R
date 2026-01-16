#using trainall data clean from s data explore script


all_data_shuffled 


#variables one at a time
hist(all_data_shuffled$Pclass)
boxplot(all_data_shuffled$Pclass)

hist(all_data_shuffled$Sex)
#cant plot categorical like hist
barplot(table(all_data_shuffled$Sex),
        xlab = "Sex",
        ylab = "Count",
        main = "Distribution of Sex")

hist(all_data_shuffled$Fare)
#note fare is very right skewd = big values impact model significantly
#categorical if very few numerical values, use to decide if include
#log fare
all_data_shuffled$log_Fare <- log1p(all_data_shuffled$Fare)
hist(
  all_data_shuffled$log_Fare,
  main = "Histogram of log(Fare)",
  xlab = "log(Fare)",
  col = "lightblue",
  breaks = 30
)



mean(all_data_shuffled$Survived)
#prop of survived

hist(all_data_shuffled$Age)

#assume categorical of SibSP, ParCh (0 1 2 3)
#cabin not enough, and embarked intuitively should not affect (actually it might)


#correlation matrix (numeric only)
num_df <- all_data_shuffled [sapply(all_data_shuffled 
, is.numeric)]
cor_matrix<-cor(num_df, use = "complete.obs")
library(dplyr)
library(tidyr)

cor_table <- cor_matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "Correlation"
  ) %>%
  filter(
    abs(Correlation) >= 0.1,
    Var1 != Var2
  ) %>%
  arrange(desc(abs(Correlation)))

cor_table


pairs(num_df) #visualises matrix



############look at response vs each variable
response <- "Survived"

num_vars <- names(all_data_shuffled)[
  sapply(all_data_shuffled, is.numeric) &
    names(all_data_shuffled) != response
]

cat_vars <- names(all_data_shuffled)[
  sapply(all_data_shuffled, is.factor) |
    sapply(all_data_shuffled, is.character)
]


#survived vs numerical
for (v in num_vars) {
  boxplot(all_data_shuffled[[v]] ~ all_data_shuffled[[response]],
          xlab = response, ylab = v,
          main = paste(v, "by", response))
}

#survived vs categorical
library(ggplot2)
library(ggplot2)
help here (i think didnt find the correct categorical)
????
  

