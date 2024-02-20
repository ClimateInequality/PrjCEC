library(tidyverse)
library(readxl)
library(scales)
library(ggpubr)

region <-
  read_excel(
    "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/cec_results/res_d_regional/region_raw_results_1990_2020.xlsx",
    sheet = "plot"
  )

diagonal  <- data.frame(x = c(-40, 40), y = c(1, 0))

fig1<-region %>%
  mutate(Year=as.factor(Year)) %>%
  ggplot() +
  geom_line(aes(
    x = utci,
    y = Value,
    col = Year,
    group = Year
  ), size = 1) +
  facet_wrap(~Region,nrow=2) +
  geom_line(data = diagonal, aes(x, y), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 26, color = 'black', size = 0.7,linetype=3)+
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size=12)
  ) +
  theme(legend.position = "top") +
  labs(x = "", y = "Proportion of Hours\n", col = "Year")+
  scale_x_continuous(breaks=seq(-40,40,10))+
  scale_color_manual(values=c("#005BBB", "#FF8C00","red"))




fig2<-region %>%
  mutate(Year=as.factor(Year)) %>%
  select(Year, utci,Value,Region) %>%
  pivot_wider(names_from = Year,values_from = Value) %>%
  mutate(gap = `2020`-`1990`) %>%
  filter(utci==26|utci==32) %>%
  mutate(UTCI=as.character(utci))%>%
  #arrange(rev(Year)) %>%
  ggplot(aes(x=Region,y=gap,fill=UTCI))+
  geom_bar(position = "dodge", stat="identity") +
  #facet_wrap(~Region)+
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    legend.text = element_text(size = 22),
    legend.title = element_text(size=20)
  ) +
  #theme(legend.position = "bottom") +
  labs(x = "\nRegion", y = "Percentage Point Deviation\n", col = "Year")+
  # scale_color_manual(values=c("#005BBB", "#FF8C00","red"))+
  # scale_x_continuous(breaks = seq(26,40,2))+
  scale_y_continuous(labels = percent_format(accuracy = 1L))+
  scale_fill_discrete(name = "UTCI(\u00B0C)")


figure <- ggarrange(fig1,fig2,
                    labels = c("A","B"),
                    ncol = 1)
figure



#ggsave("path", plot = name, width = 6.5, height = 6.5, dpi = 500,bg="white")
