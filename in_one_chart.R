# in one chart plot 
# install a package
install.packages("tidyverse")
require("tidyverse")

# import 
require("readr")
require("ggplot2")

# set working directory
setwd("~/Documents/r-studio/in_one_chart")

ECI = read.csv('DSPI(U.S. Bureau of Economic Analysi) - DSPI.csv')

# explore: ECI dot plot
ggplot(ECI) +
  aes(x = observation_date, y = ECI, color = ifelse(ECI > 0, "green", "red")) +
  geom_point() +
  theme(axis.text.x=element_blank(),
        legend.position = "none") +
  labs(title = 'Economic Confidence Index (trend)', 
       subtitle = "ECI change from Trump's first administration to Biden's administration")

# explore: ECI vs DSPI
ggplot(ECI) +
  aes(x=DSPI, y=ECI) +
  geom_point()

# convert char to date
ECI$date = as.Date(ECI$observation_date)

# line chart ECI  
ggplot(ECI) +
  aes(x=date,y=ECI,group=1)+
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

# DCPI Line chart
ggplot(ECI) +
  aes(x=date,y=DSPI,group=1)+
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")


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


#ADMINISTRATION
presidents <- data.frame(
  xmin = as.Date(c("2017-01-20", "2021-01-20")),
  xmax = as.Date(c("2021-01-19", "2024-12-31")),
  ymin = -Inf,
  ymax = Inf,
  president = factor(c("Trump", "Biden")) # This will be used for the legend
)

# change order of category
df_long$Category <- factor(df_long$Category, 
                           levels = c("ECI","DSPI"), 
                           labels = c("People's confidence towards the economy tanked after covid.", 
                                      "but real incomes were rising the whole time."))

lockdown <- as.Date('2020-03-16')

interpolated_y <- approx(df_long$date, df_long$Amount, xout = lockdown, rule = 2)$y

# Combined Graph
chart2 <-ggplot(df_long,aes(x = date, y = Amount)) +
  geom_rect(data = presidents, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                fill = president), 
            alpha = 0.2, 
            inherit.aes = FALSE) +
  # ECI first
  geom_line(data = subset(df_long, Category == "ECI"), 
            aes(color = ifelse(Amount > 0, "grey", "red")), 
            linewidth = 1,
            show.legend = FALSE,
            na.rm = TRUE) +
  geom_point(data = subset(df_long, Category == "People's confidence towards the economy tanked after covid."),
             aes(x = lockdown, y = 24), color = "red", size = 3)+
  # DSPI second
  geom_line(data = subset(df_long, Category != "ECI"), 
            aes(color = Category), 
            linewidth = 1,
           show.legend = FALSE,
           na.rm = TRUE) +  
  geom_hline(data = subset(df_long, Category == "People's confidence towards the economy tanked after covid."), 
             aes(yintercept = 0), 
             alpha = .6,
             linetype = "dotted",
             color = "red", 
             linewidth = .8,
             show.legend = FALSE) +
  labs(title = "Post-COVID Reality: Income Rises But Pessimism Lingers",
       subtitle = "Comparing Real Disposable Personal Income (DSPI) to the Economic 
       Confidence Index During Trump's Last Administration and Biden's Administration",
       fill = "Presidency",
       x = NULL,
       y = NULL) +
  facet_wrap(~Category, ncol = 1, 
             scales = 'free_y',
             as.table = TRUE) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c("Trump" = "lightcoral", "Biden" = "lightblue")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  geom_text(data = subset(df_long, Category == "People's confidence towards the economy tanked after covid."), 
            aes(x = as.Date("2020-03-16"), y = 25), 
           label = "First lockdown in US on March 16, 2020", 
           color = "azure4", size = 2, hjust = 0, vjust = -1)
chart2

ggsave("In_One_Chart2.png")


