library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggpubr)

setwd(
  "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/GitHub/PrjCEC/res/res_mean_child/"
)
tab_a_24vsday_data <- read.csv("tab_a_24vsday_data.csv")
tab_b_season_data <- read.csv("tab_b_season_data.csv")




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
    mutate(`All annual hours` = cdf_percpoint_chg_time_0G_24,
           `Daytime (6am-10pm) hours` = cdf_percpoint_chg_time_1G_6t22) %>%
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
    mutate(`All annual hours` = cdf_percent_chg_time_0G_24,
           `Daytime (6am-10pm) hours` = cdf_percent_chg_time_1G_6t22) %>%
    select(-cdf_percent_chg_time_0G_24, -cdf_percent_chg_time_1G_6t22) %>%
    pivot_longer(2:3, names_to = "hour_select", values_to = "cdf_percent_chg_time") %>%
    select(-hour_select, -utci_thres)
)


tab_b_season_data$hour_select <-"April-Septermber hours"
tab_b_season_data_small <- tab_b_season_data[,c(1,10,4,5)]
colnames(tab_b_season_data_small)<-colnames(reshape)
reshape <- rbind(reshape,tab_b_season_data_small)

# scatter plot show change in level and rate by UTCI threshold

fig1_a<-
  reshape %>%
  filter(utci_thres >= 26) %>%
  ggplot() +
  geom_point(
    aes(
      x = utci_thres,
      y = cdf_percent_chg_time,
      color = hour_select,
      shape = hour_select),
    size=3) +
  geom_line(
    aes(
      x = utci_thres,
      y = cdf_percent_chg_time,
      #size = cdf_percpoint_chg_time,
      color = hour_select,
      linetype = hour_select),
    size=2)+
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16)
  ) +
  theme(legend.position = "bottom") +
  labs(x = "\nUTCI",
       y = "Percentage change\n",
       col = "",
       linetype = "",
       shape ="") +
  scale_x_continuous(breaks = seq(26, 40, 2)) +
  scale_x_continuous(
    breaks = seq(26, 40, 2),
    labels = function(x) paste("≥", x, "C°")
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("")+
  annotate("rect", xmin = 26, xmax = 32, ymin = 0, ymax = 1.1*max(reshape$cdf_percent_chg_time), fill = "#FFEBEB", alpha = 0.15) +
  annotate("text", x = 29, y = 1.05 * max(reshape$cdf_percent_chg_time), label = "Moderate Heat Stress", color = "black", size = 4, alpha = 0.7) +
  annotate("rect", xmin = 32, xmax = 38, ymin = 0, ymax = 1.1*max(reshape$cdf_percent_chg_time), fill = "#FFB6C1", alpha = 0.15) +
  annotate("text", x = 35, y = 1.05 * max(reshape$cdf_percent_chg_time), label = "Strong Heat Stress", color = "black", size = 4, alpha = 0.7) +
  annotate("rect", xmin = 38, xmax = 40, ymin = 0, ymax = 1.1*max(reshape$cdf_percent_chg_time), fill = "#FF69B4", alpha = 0.15) +
  annotate("text", x = 39, y = 1.05 * max(reshape$cdf_percent_chg_time), label = "Very Strong\nHeat Stress", color = "black", size = 4, alpha = 0.7)




fig1_a


fig1_b <-
  reshape %>%
  filter(utci_thres >= 26) %>%
  ggplot() +
  geom_point(
    aes(
      x = utci_thres,
      y = cdf_percpoint_chg_time,
      color = hour_select,
      shape = hour_select
    ),
    size = 3
  ) +
  geom_line(
    aes(
      x = utci_thres,
      y = cdf_percpoint_chg_time,
      color = hour_select,
      linetype = hour_select
    ),size = 2
  ) +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16)
  ) +
  theme(legend.position = "bottom") +
  labs(x = "\nUTCI",
       y = "Percentage point(pp) change\n",
       col = "",
       linetype = "",
       shape = "") +
  scale_x_continuous(breaks = seq(26, 40, 2)) +
  scale_x_continuous(
    breaks = seq(26, 40, 2),
    labels = function(x) paste("≥", x, "C°")
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 100, suffix = "pp"),
                     breaks = seq(0, 0.06, 0.01)) +
  scale_color_viridis(discrete = TRUE) +
  scale_size(labels = scales::percent) +
  ggtitle("") +
  annotate("rect", xmin = 26, xmax = 32, ymin = 0, ymax = 1.1*max(reshape$cdf_percpoint_chg_time), fill = "#FFEBEB", alpha = 0.15) +
  annotate("text", x = 29, y = 1.05 * max(reshape$cdf_percpoint_chg_time), label = "Moderate Heat Stress", color = "black", size = 4, alpha = 0.7) +
  annotate("rect", xmin = 32, xmax = 38, ymin = 0, ymax = 1.1*max(reshape$cdf_percpoint_chg_time), fill = "#FFB6C1", alpha = 0.15) +
  annotate("text", x = 35, y = 1.05 * max(reshape$cdf_percpoint_chg_time), label = "Strong Heat Stress", color = "black", size = 4, alpha = 0.7) +
  annotate("rect", xmin = 38, xmax = 40, ymin = 0, ymax = 1.1*max(reshape$cdf_percpoint_chg_time), fill = "#FF69B4", alpha = 0.15) +
  annotate("text", x = 39, y = 1.05 * max(reshape$cdf_percpoint_chg_time), label = "Very Strong\nHeat Stress", color = "black", size = 4, alpha = 0.7)

fig1_b

combined_plot <- ggarrange(
  fig1_a + theme(legend.position = "none"),  # Remove legend from the first plot
  fig1_b + theme(legend.position = "none"),  # Remove legend from the second plot
  ncol = 1, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)
combined_plot



