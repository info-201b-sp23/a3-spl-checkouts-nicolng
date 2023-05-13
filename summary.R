library(dplyr)
library(tidyverse)
library(scales)
five_check <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# find the top 5 material types
five_check %>% 
  group_by(MaterialType) %>% 
  summarise(total = length(MaterialType)) %>%
  top_n(5)

# count the top 5 material types (qualitative to quantitative)
top_five_by_years <- five_check %>% 
  group_by(CheckoutYear) %>% 
  summarise(Audiobooks = sum(Checkouts[MaterialType == "AUDIOBOOK"]),
           Books = sum(Checkouts[MaterialType == "BOOK"]),
           Ebooks = sum(Checkouts[MaterialType == "EBOOK"]),
           SoundDis = sum(Checkouts[MaterialType == "SOUNDDISC"]), 
           VideoDis = sum(Checkouts[MaterialType == "VIDEODISC"]))

# plot checkout year vs material types
ggplot(data = top_five_by_years) + 
  geom_line(mapping = aes(x = CheckoutYear, y = Audiobooks, col = "Audiobooks")) +
  geom_line(mapping = aes(x = CheckoutYear, y = Books, col = "Books")) +
  geom_line(mapping = aes(x = CheckoutYear, y = Ebooks, col = "Ebooks")) +
  geom_line(mapping = aes(x = CheckoutYear, y = SoundDis, col = "Sound Disks")) +
  geom_line(mapping = aes(x = CheckoutYear, y = VideoDis, col = "Video Disks")) +
  labs(title = "Top 5 Types of Materials Checked Out By Year", 
      x = "Checkout Year", 
      y = "Total Material Type Checked Out",
      color = "Types of Materials") + 
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) + 
  scale_y_continuous(labels = label_number_si())
  


# plot of proportion of digital and physical items overtime
ggplot(data = five_check) + 
  geom_bar(mapping = aes(x = CheckoutYear, fill = UsageClass), position = "fill")+
  labs(title = "Proportion of Digital and Phyiscal Items Overtime",
      x = "Checkout Year",
      y = "Proportion",
      fill = "Usage Class")


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
