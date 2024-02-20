library(readxl)
library(scales)

counter_decom <-
  read_excel("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/cec_results/res_c_popvsclimate/hour_cdf_heat_0908.xlsx", sheet = "long_format")

counter_decom %>%
  ggplot() +
  geom_vline(xintercept = 26, color = 'black', size = 1.2,linetype=3)+
  geom_line(aes(
    x = UTCI,
    y = pp_dev,
    col = label,
    linetype = label,
    group = label
  ), size = 2) +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    legend.text = element_text(size = 16)
  ) +
  theme(legend.position = "bottom") +
  labs(x = "UTCI(C??)", y = "Percentage Point Deviation\n", col = "",linetype ="") +
  scale_y_continuous(labels = percent_format(scale = 100))



