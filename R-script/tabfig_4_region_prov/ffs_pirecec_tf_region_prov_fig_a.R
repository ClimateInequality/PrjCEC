library(tidyverse)
library(readxl)
library(scales)
library(ggpubr)


# setwd local
#setwd("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/GitHub/PrjCEC/")

# File names
bl_main_save <- TRUE
verbose <- TRUE
spt_path_res <- file.path("res", "res_region_prov", fsep = .Platform$file.sep)

spn_path <- file.path(spt_path_res, "fig_a_data.csv", fsep = .Platform$file.sep)
region <- read_csv(spn_path)

# region <-
#   read_excel(
#     "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/cec_results/res_d_regional/region_raw_results_1990_2020.xlsx",
#     sheet = "plot"
#   )
# 
# 
# diagonal  <- data.frame(x = c(-40, 40), y = c(1, 0))
# 
# fig1<-region %>%
#   mutate(Year=as.factor(Year)) %>%
#   ggplot() +
#   geom_line(aes(
#     x = utci,
#     y = Value,
#     col = Year,
#     group = Year
#   ), size = 1) +
#   facet_wrap(~Region,nrow=2) +
#   geom_line(data = diagonal, aes(x, y), color = "red", linetype = "dashed") +
#   geom_vline(xintercept = 26, color = 'black', size = 0.7,linetype=3)+
#   theme_bw() +
#   theme(
#     #panel.border = element_blank(),
#     #panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     text = element_text(size = 16),
#     axis.text.y = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size=12)
#   ) +
#   theme(legend.position = "top") +
#   labs(x = "", y = "Proportion of Hours\n", col = "Year")+
#   scale_x_continuous(breaks=seq(-40,40,10))+
#   scale_color_manual(values=c("#005BBB", "#FF8C00","red"))
# 
# 
# 
# 
# fig2<-region %>%
#   mutate(Year=as.factor(Year)) %>%
#   select(Year, utci,Value,Region) %>%
#   pivot_wider(names_from = Year,values_from = Value) %>%
#   mutate(gap = `2020`-`1990`) %>%
#   filter(utci==26|utci==32) %>%
#   mutate(UTCI=as.character(utci))%>%
#   #arrange(rev(Year)) %>%
#   ggplot(aes(x=Region,y=gap,fill=UTCI))+
#   geom_bar(position = "dodge", stat="identity") +
#   #facet_wrap(~Region)+
#   theme_bw() +
#   theme(
#     #panel.border = element_blank(),
#     #panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     text = element_text(size = 22),
#     axis.text.y = element_text(size = 22),
#     axis.text.x = element_text(size = 22),
#     legend.text = element_text(size = 22),
#     legend.title = element_text(size=20)
#   ) +
#   #theme(legend.position = "bottom") +
#   labs(x = "\nRegion", y = "Percentage Point Deviation\n", col = "Year")+
#   # scale_color_manual(values=c("#005BBB", "#FF8C00","red"))+
#   # scale_x_continuous(breaks = seq(26,40,2))+
#   scale_y_continuous(labels = percent_format(accuracy = 1L))+
#   scale_fill_discrete(name = "UTCI(\u00B0C)")
# 
# 
# figure <- ggarrange(fig1,fig2,
#                     labels = c("A","B"),
#                     ncol = 1)
# figure



#ggsave("path", plot = name, width = 6.5, height = 6.5, dpi = 500,bg="white")



fig_pp <- region %>%
  filter(utci_thres >= 26, loc_level == "region") %>%
  filter(utci_thres %in% c(26, 28, 30, 32, 34, 38)) %>%
  filter(utci_thres %in% c(26, 32, 38)) %>%
  mutate(UTCI = as.character(utci_thres)) %>%
  ggplot(aes(x = region_prov_name, y = cdf_percpoint_chg, fill = UTCI)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  labs(
    x = "\nEconomic regions",
    y = "Percentage point (pp) change\n",
    title = "(c) Changes comparing 1989–91 and 2019–21 in percentage points"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 8)
    )
  )+  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),limits=c(0,0.07)) +
  scale_fill_discrete(
    name = "UTCI",
    labels = c("≥ 26 °C", "≥ 32 °C", "≥ 38 °C")
  ) +
  theme(legend.position = "top") +
  theme(
    legend.position = c(0.8, 0.7),  # Adjust these values to position the legend
    legend.justification = c(0.5, 0),
    legend.box.just = "left",
    legend.background = element_rect(fill = "white",color = "black")
  )
#geom_rect(aes(xmin = 3.39, xmax = 3.95, ymin = 0.031, ymax = 0.0395),fill = "white", color = "black", size = 0.5)
fig_pp
ggsave("/Users/mlaghi/Documents/GitHub/PrjCEC/res/res_region_prov/SINGLE_fig_4_c_triple_region_pp.pdf", plot = fig_pp, width = 11.69, height = 5, device = cairo_pdf)

# 
# geom_rect(aes(xmin = 3.48, xmax = 3.9, 
#               ymin = 0.031, ymax = 0.039),

region_plot <- region %>%
  filter(utci_thres >= 26, loc_level == "region") %>%
  filter(utci_thres %in% c(26, 28, 30, 32, 34, 38)) %>%
  filter(utci_thres %in% c(26, 32, 38)) %>%
  mutate(UTCI = as.character(utci_thres))

fig_1990 <- region_plot %>%
  ggplot(aes(x = region_prov_name, y = year_1990, fill = UTCI)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  labs(
    x = "\nEconomic regions",
    y = "Share of time during 1989–91\n",
    title = "(a) 1989–91"
  )+  scale_y_continuous(labels = label_percent(scale = 100),limits=c(0,0.3)) +
  scale_fill_discrete(
    name = "UTCI",
    labels = c("≥ 26 °C", "≥ 32 °C", "≥ 38 °C")
  ) +
  theme(legend.position = "top") +
  theme(
    legend.position = c(0.8, 0.7),  # Adjust these values to position the legend
    legend.justification = c(0.5, 0),
    legend.box.just = "left",
    legend.background = element_rect(fill = "white",color = "black")
  ) +  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 8)
    )
  )
  #geom_rect(aes(xmin = 3.39, xmax = 3.95,
  #              ymin = 0.215, ymax = 0.269),
  #          fill = "white", color = "black", size = 0.5)
  
fig_1990
ggsave("/Users/mlaghi/Documents/GitHub/PrjCEC/res/res_region_prov/SINGLE_fig_4_a_triple_region_1990.pdf", plot = fig_1990, width = 11.69, height = 5, device = cairo_pdf)


fig_2020 <- region_plot %>%
  ggplot(aes(x = region_prov_name, y = year_2020, fill = UTCI)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "none"  # Hide the default legend
  ) +
  labs(
    x = "\nEconomic regions",
    y = "Share of time during 2019–21\n",
    title = "(b) 2019–21"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      margin = margin(b = 8)
    )
  )+  scale_y_continuous(labels = label_percent(scale = 100), limits = c(0, 0.3)) +
  scale_fill_discrete(
    name = "UTCI",
    labels = c("≥ 26 °C", "≥ 32 °C", "≥ 38 °C")
  ) +
  theme(
    legend.position = c(0.8, 0.7),  # Adjust these values to position the legend
    legend.justification = c(0.5, 0),
    legend.box.just = "left",
    legend.background = element_rect(fill = "white",color = "black")
  )
#+geom_rect(aes(xmin = 3.39, xmax = 3.95,
#                ymin = 0.215, ymax = 0.269),
#            fill = "white", color = "black", size = 0.5)
fig_2020
ggsave("/Users/mlaghi/Documents/GitHub/PrjCEC/res/res_region_prov/SINGLE_fig_4_b_triple_region_2020.pdf", plot = fig_2020, width = 11.69, height = 5, device = cairo_pdf)



# Combined figure for easy review

figure <- ggarrange(
  fig_1990,
  fig_2020,
  fig_pp,
  #labels = c("A", "B", "C"),
  ncol = 1,
  align = "v"
)

figure

ggsave(
  "/Users/mlaghi/Documents/GitHub/PrjCEC/res/res_region_prov/fig_4_triple_region_combined.pdf",
  plot = figure,
  width = 11.69,
  height = 15,
  device = cairo_pdf
)
