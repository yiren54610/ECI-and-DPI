# in one chart plot 
# install a package
install.packages("tidyverse")

# import 
require("readr")
require("ggplot2")

# set working directory
setwd("~/Documents/r-studio/in_one_chart")

ECI = read.csv('DSPI(U.S. Bureau of Economic Analysi) - DSPI.csv')

remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
require(ggplot2)

# ECI plot
ggplot(ECI) +
  aes(x = observation_date, y = ECI, color = ifelse(ECI > 0, "green", "red")) +
  geom_point() +
  theme(axis.text.x=element_blank(),
        legend.position = "none") +
  labs(title = 'Economic Confidence Index (trend)', 
       subtitle = "ECI change from Trump's first administration to Biden's administration")

ggplot(ECI) +
  aes(x=DSPI, y=ECI) +
  geom_point()

# convert char to date
ECI$date = as.Date(ECI$observation_date)
  
ggplot(ECI) +
  aes(x=date,y=ECI,group=1)+
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

# combined chart
ggplot(ECI) +
  aes(x=date,y=DSPI,group=1)+
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

# DSPI plot
# 1. Reshape the data to long format
library(dplyr)
df <- ECI %>%
  select(date, DSPI,ECI)

# wide to long
install.packages("tidyr")
require('tidyr')
df_long <- df %>%
  pivot_longer(cols = c(DSPI, ECI), 
               names_to = "Category", 
               values_to = "Amount")

# Create the combined chart
ggplot(df_long, aes(x = date, y = Amount, fill = Category)) +
  geom_bar(stat = "identity",alpha = 0.5) +  
  labs(title = "Post-COVID Reality: Income Rises But Pessimism Lingers",
       subtitle = "Comparing Real Disposable Personal Income (DSPI) to the Economic Confidence Index During Trump's Final Administration and Biden's Administration",
       x = "Observation Date",
       y = "Amount") +
  facet_wrap(~Category,ncol=1,scales='free_y') +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

#adMINISTRATION
presidents <- data.frame(
  xmin = as.Date(c("2017-01-20", "2021-01-20")),
  xmax = as.Date(c("2021-01-19", "2024-12-31")),
  ymin = -Inf,
  ymax = Inf,
  president = factor(c("Trump", "Biden")) # This will be used for the legend
)

# another type
chart <- ggplot(df_long,aes(x = date, y = Amount)) +
  geom_rect(data = presidents, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = president), 
            alpha = 0.2, inherit.aes = FALSE) +
  geom_bar(data = subset(df_long, Category != "ECI"), 
           stat = "identity", 
           alpha = 0.5,
           show.legend = FALSE) +  
  geom_line(data = subset(df_long, Category == "ECI"), 
            aes(color = Category), 
            size = 1,
            show.legend = FALSE) +
  geom_hline(data = data.frame(yintercept = 0, Category = "ECI"),
             linetype = "dotted",
             yintercept = 0, 
             color = "red", 
             linewidth = .8,
             alpha = ifelse(Category == "DSPI", 0, 0.6)) +
  labs(title = "Post-COVID Reality: Income Rises But Pessimism Lingers",
       subtitle = "Comparing Real Disposable Personal Income (DSPI) to the Economic 
       Confidence Index During Trump's Last Administration and Biden's Administration",
       fill = "Presidency",
       x = NULL,
       y = NULL) +
  facet_wrap(~Category, ncol = 1, scales = 'free_y') +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c("Trump" = "lightcoral", "Biden" = "lightblue")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  annotate("text", x = 2020-03-01:2020-04-01, y = 25, 
           label = "First lockdown on March 16th in US",
           parse=TRUE)

chart

ggsave("In_One_Chart.png")


