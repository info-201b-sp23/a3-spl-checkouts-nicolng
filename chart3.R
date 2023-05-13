library(rmarkdown)
library(dplyr)
library(tidyverse)
library(scales)
five_check <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)


# count the amount of fiction and literature checked out
fic_vs_lit <- five_check %>% 
  group_by(CheckoutYear) %>%
  summarise(Fiction = sum(Checkouts[str_detect(tolower(Subjects), "fiction")]),
            Literature = sum(Checkouts[str_detect(tolower(Subjects), "literature")]))


# plot of fiction vs literature over time
ggplot(data = fic_vs_lit) + 
  geom_line(mapping = aes(x = CheckoutYear, y = Fiction, col = "Fiction")) +
  geom_line(mapping = aes(x = CheckoutYear, y = Literature, col = "Literature")) +
  labs(title = "Amount of Fiction versus Literature Checked Out Over Time",
        x = "Checkout Year",
        y = "Amount Checked Out",
        color = "Type of Genre") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) + 
  scale_y_continuous(labels = label_number_si())
