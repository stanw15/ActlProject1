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
#note fare is very right skewd
#categorical if very few numerical values, use to decide if include

mean(all_data_shuffled$Survived)
#prop of survived

hist(all_data_shuffled$Age)

#assume categorical of SibSP, ParCh (0 1 2 3)
#cabin not enough, and embarked intuitively should not affect


#correlation matrix (numeric only)
num_df <- all_data_shuffled [sapply(all_data_shuffled 
, is.numeric)]
cor(num_df, use = "complete.obs")

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

