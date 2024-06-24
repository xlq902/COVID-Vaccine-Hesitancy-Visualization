# import libraries for data visualization
library("tidyverse")
library("ggthemes")

state_name <- "Arizona"

# set working directory & read csv for reduced data
setwd("C:/Users/tetrg/R/COVID vaccine hesitancy data")
full_data <- read.csv("Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates_20240624.csv")
data <- full_data[,2:6]
ctdata <- subset(data, data[2] == str_to_upper(state_name))
write.csv(ctdata, "C:/Users/tetrg/R/COVID vaccine hesitancy data/reduced.csv")
d <- read.csv("reduced.csv")

# rearrange dataframe to expand
# new col to add
Estimated.strongly.hesitant <- c()
for(i in 1:nrow(d)){
    hesD <- as.numeric(d[i, 4] - d[i, 6])
    Estimated.strongly.hesitant <- c(Estimated.strongly.hesitant, hesD)
}
d[,6] <- NULL
d <- cbind(d, Estimated.strongly.hesitant) # replaces old strongly hesitant column with new column

# removing columns
d[,5] <- NULL # removes unsure column
d[,3] <- NULL # removes states column
d[,1] <- NULL # removes X column

# modifies titles for legend entries
colnames(d)[2] <- "Hesitant"
colnames(d)[3] <- "Strongly hesitant"

# pivots dataframe longer for colors
dl <- d %>%
  pivot_longer(
    cols = -County.Name,
    names_to = "Hesitancy",
    values_to = "Values"
  )

# display data based on county
ggplot(dl, aes(y = str_remove_all(County.Name, paste("County,", state_name)), x = Values * 100, fill = Hesitancy)) +
    geom_col() +
    labs(
        fill = "Hesitancy",
        title = paste("Vaccine Hesitancy in ", state_name, ", by County", sep = ""),
        subtitle = "From the CDC dataset \"Vaccine Hesitancy for COVID-19: County and local estimates\"",
        y = "County name",
        x = "% hesitant" # % because the formula as.numeric(Estimated.strongly.hesitant) * 100 first converts strongly hesitant to numeric, then multiplies by 100%
    ) +
    scale_color_colorblind() +
    theme_fivethirtyeight()
