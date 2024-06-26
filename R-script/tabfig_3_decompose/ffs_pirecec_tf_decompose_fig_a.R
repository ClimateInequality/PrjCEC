library(readxl)
library(scales)

# setwd local
setwd("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/GitHub/PrjCEC/")

# File names
bl_main_save <- TRUE
verbose <- TRUE
spt_path_res <- file.path("res", "res_decompose", fsep = .Platform$file.sep)
# Input data file, generated by `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_csv.R`
spn_path <- file.path(spt_path_res, "fig_a_data.csv", fsep = .Platform$file.sep)
counter_decom <- read_csv(spn_path)

#counter_decom <-read_excel("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/cec_results/res_c_popvsclimate/hour_cdf_heat_0908.xlsx", sheet = "long_format")

counter_decom_long <- counter_decom %>%
  mutate(region_name = case_when(
    is.na(region_name) ~ "national",
    TRUE ~ region_name
  )) %>%
  filter(region_name=="national") %>%
  select("region_name","utci_thres","cdf_percpoint_chg_20_v90","cdf_percpoint_chg_20utci_v90","cdf_percpoint_chg_20pop_v90") %>%
  pivot_longer(3:5,names_to = "label",values_to = "value") %>%
  mutate(type = case_when(label == "cdf_percpoint_chg_20_v90" ~ "2020 − 1990",
                          label == "cdf_percpoint_chg_20utci_v90" ~ "Climate Effect",
                          label == "cdf_percpoint_chg_20pop_v90" ~ "Population Effect"))

fig3 <- counter_decom_long %>%
  filter(utci_thres >=26) %>%
  ggplot() +
  #geom_vline(xintercept = 26, color = 'black', size = 1.2,linetype=3)+
  geom_line(aes(
    x = utci_thres,
    y = value,
    col = type,
    linetype = type,
    group = type
  ), size = 2) +
  geom_point(aes(
    x = utci_thres,
    y = value,
    col = type,
    group = type,
    shape=type
  ), size = 3) +
  #facet_grid(~region_name) +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 18),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16)
  ) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(26, 40, 2)) +
  scale_x_continuous(
    breaks = seq(26, 40, 2),
    labels = function(x) paste("≥", x, "°C")
  ) +
  scale_color_viridis(discrete = TRUE)+
  labs(x = "\nUTCI", y = "Percentage point (pp) change\n", col = "",linetype ="", shape="") +
  scale_y_continuous(labels = percent_format(scale = 100),
                     breaks= seq(0,0.035,0.01)) +
  annotate("rect", xmin = 26, xmax = 32, ymin = 0, ymax = 1.1*max(counter_decom_long$value), fill = "#FFEBEB", alpha = 0.15) +
  annotate("text", x = 29, y = 1.05 * max(counter_decom_long$value), label = "Moderate Heat Stress", color = "black", size = 4.5, alpha = 0.7) +
  annotate("rect", xmin = 32, xmax = 38, ymin = 0, ymax = 1.1*max(counter_decom_long$value), fill = "#FFB6C1", alpha = 0.15) +
  annotate("text", x = 35, y = 1.05 * max(counter_decom_long$value), label = "Strong Heat Stress", color = "black", size = 4.5, alpha = 0.7) +
  annotate("rect", xmin = 38, xmax = 40, ymin = 0, ymax = 1.1*max(counter_decom_long$value), fill = "#FF69B4", alpha = 0.15) +
  annotate("text", x = 39, y = 1.05 * max(counter_decom_long$value), label = "Very Strong\nHeat Stress", color = "black", size = 4.5, alpha = 0.7)+
  theme(legend.position = c(0.8, 0.5))+
  theme(legend.background = element_blank(),
        #legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank()
  )+
  geom_rect(aes(xmin = 35.2, xmax = 40, ymin = 0.42 * max(counter_decom_long$value), ymax = 0.65 * max(counter_decom_long$value)),
            fill = "white", color = "black", size = 0.5)

  

fig3
ggsave("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/GitHub/PrjCEC/res/res_decompose/fig_3_decompose.pdf", plot = fig3, width = 11.69, height = 6.27, device = cairo_pdf)


