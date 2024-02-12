# Implements step 3 of https://github.com/ClimateInequality/PrjCEC/issues/36
# Code below modified/copied from C:\Users\fan\UH-ECON Dropbox\Fan Wang\PIRE\team\kai_feng\cec_results\res_a_percentdays_30years\plot_a.R
library(readr)
library(ggpubr)
library(dplyr)
library(tidyr)


hour_cdf_1990_2020 <-
  read_excel(
    "~/dropbox_penn/Dropbox/PIRE/team/kai_feng/cec_results/res_a_percentdays_30years/hour_cdf_1990_2020.xlsx",sheet = "long_format"
  )


cdf <- hour_cdf_1990_2020 %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot() +
  geom_line(aes(
    x = utci,
    y = cdf,
    col = Year,
    group = Year
  ), size = 1) +
  geom_point(aes(
    x = utci,
    y = cdf,
    col = Year,
    group = Year))+
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
  theme(legend.position = "top") +
  labs(x = "\nUTCI(C°)", y = "Proportion\n", col = "Year") +
  scale_x_continuous(breaks=seq(-40,40,10))+
  ggtitle("Cumulative Density Curve")

pmf <- hour_cdf_1990_2020 %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot() +
  geom_line(aes(
    x = utci,
    y = pmf,
    col = Year,
    group = Year
  ), size = 1) +
  geom_point(aes(
    x = utci,
    y = pmf,
    col = Year,
    group = Year))+
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
  theme(legend.position = "top") +
  labs(x = "\nUTCI(C°)", y = "Proportion\n", col = "Year") +
  scale_x_continuous(breaks=seq(-40,40,10))+
  ggtitle("Probability Density Curve")



hour_cdf_1990_2020 %>%
  mutate(Year=as.factor(Year)) %>%
  filter(utci>26) %>%
  arrange(rev(Year)) %>%
  ggplot()+
  geom_col(aes(x=utci,y=cdf,fill=Year),position = "identity") +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  #theme(legend.position = "top") +
  labs(x = "\nUTCI", y = "Proportion\n", col = "Year") 

gap <- hour_cdf_1990_2020 %>%
  mutate(Year=as.factor(Year)) %>%
  filter(utci>=26) %>%
  arrange(rev(Year)) %>%
  select(-pmf) %>%
  pivot_wider(names_from = "Year",values_from = "cdf") %>%
  mutate(gap = `2020`-`1990`) %>%
  ggplot()+
  geom_col(aes(x=utci,y=gap),position = "identity") +
  theme_bw() +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18)
  ) +
  #theme(legend.position = "None") +
  labs(x = "\nUTCI (C°)", y = "Percentage Point Deviation\n", col = "Year")+
  scale_x_continuous(breaks=seq(26,40,2))+
  scale_y_continuous(labels = scales::percent)+
  ggtitle("")

figure <- ggarrange(cdf, pmf, gap,
                    ncol = 1,common.legend = TRUE)
figure2 <- ggarrange(cdf, pmf,
                    nrow = 1,common.legend = TRUE)