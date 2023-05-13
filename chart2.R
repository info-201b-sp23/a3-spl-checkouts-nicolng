library(rmarkdown)
library(dplyr)
library(tidyverse)
library(scales)
five_check <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# plot of proportion of digital and physical items overtime
ggplot(data = five_check) + 
  geom_bar(mapping = aes(x = CheckoutYear, fill = UsageClass), position = "fill")+
  labs(title = "Proportion of Digital and Phyiscal Items Overtime",
      x = "Checkout Year",
      y = "Proportion",
      fill = "Usage Class")
