library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd(
  "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/GitHub/PrjCEC/res/res_mean_child/"
)
tab_a_24vsday_data <- read.csv("tab_a_24vsday_data.csv")

#test graph
tab_a_24vsday_data %>%
  filter(utci_thres >= 26) %>%
  ggplot() +
  geom_line(aes(x = cdf_percpoint_chg_time_0G_24, y = cdf_percent_chg_time_0G_24),
            col = "red") +
  geom_line(aes(x = cdf_percpoint_chg_time_1G_6t22, y = cdf_percent_chg_time_1G_6t22))


#reshape data into long format
reshape <- cbind(
  tab_a_24vsday_data %>%
    select(
      utci_thres,
      cdf_percpoint_chg_time_0G_24,
      cdf_percpoint_chg_time_1G_6t22
    ) %>%
    mutate(`All hours` = cdf_percpoint_chg_time_0G_24,
           `Day hours` = cdf_percpoint_chg_time_1G_6t22) %>%
    select(
      -cdf_percpoint_chg_time_0G_24,
      -cdf_percpoint_chg_time_1G_6t22
    ) %>%
    pivot_longer(2:3, names_to = "hour_select", values_to = "cdf_percpoint_chg_time"),
  
  tab_a_24vsday_data %>%
    select(
      utci_thres,
      cdf_percent_chg_time_0G_24,
      cdf_percent_chg_time_1G_6t22
    ) %>%
    mutate(`All hours` = cdf_percent_chg_time_0G_24,
           `Day hours` = cdf_percent_chg_time_1G_6t22) %>%
    select(-cdf_percent_chg_time_0G_24, -cdf_percent_chg_time_1G_6t22) %>%
    pivot_longer(2:3, names_to = "hour_select", values_to = "cdf_percent_chg_time") %>%
    select(-hour_select, -utci_thres)
)


# scatter plot show change in level and rate by UTCI threshold
reshape %>%
  filter(utci_thres >= 26) %>%
  ggplot() +
  geom_point(
    aes(
      x = utci_thres,
      y = cdf_percent_chg_time,
      size = cdf_percpoint_chg_time,
      color = hour_select
    ),
    shape = 21,
    alpha = 10 / 10,
    stroke = 1
  ) +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20)
  ) +
  theme(legend.position = "right") +
  labs(x = "\nUTCI(C°)",
       y = "Percentage increase\n",
       col = "Hour selected",
       size = "Percentage point increase") +
  scale_x_continuous(breaks = seq(26, 40, 2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("red", "blue")) +
  scale_size(labels = scales::label_percent(scale = 100, suffix = "pp")) +
  ggtitle("")
