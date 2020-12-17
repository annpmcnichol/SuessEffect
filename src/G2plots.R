#Select Atlantic data; use expocodes

library(tidyverse)
library(lubridate)
library(here)

glodapv2_dic <- read_csv(here("data/glodapv2_dic.csv"))


Atlantic_WOCE <- filter(glodapv2_dic, expocode %in% c("33MW19910711", "33MW19930704.1",
                          "316N19970717", "316N19970815", "33RO19980123")) 

fill <- "#4271AE"
line <- "#1F3552"

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Atlantic_WOCE$expocode <- factor(Atlantic_WOCE$expocode, levels=c("33MW19910711", "33MW19930704.1",
                      "316N19970717", "316N19970815", "33RO19980123"), labels = c("A16S", "A16N",
                        "A20", "A22", "A05"))

Atlantic_AOU <- 
  filter(Atlantic_WOCE, G2aou > 0.0)

Atl_AOU_g <-
  ggplot(Atlantic_AOU, aes(x = G2aou, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "AOU") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 2.25)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) 


Atl_AOU_g


Atl_AOU_f <-
  ggplot(Atlantic_AOU, aes(x = G2aou, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "AOU") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 2.25)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(facets = vars(expocode))
 
Atl_AOU_f

Atl_PO4_g <-
  ggplot(Atlantic_WOCE, aes(x = G2phosphate, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Phosphate") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 2.25)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) 

Atl_PO4_g

Atl_PO4_f <-
  ggplot(Atlantic_WOCE, aes(x = G2phosphate, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Phosphate") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 2.25)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

Atl_PO4_f


A16S <- filter (Atlantic_WOCE, expocode == "A16S")

Atl_deep_phosphate <- filter(Atlantic_WOCE, G2pressure > 750)

Atl_deepP_g <-
  ggplot(Atl_deep_phosphate, aes(x = G2phosphate, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Phosphate") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.5,1.25, 0.10), limits = c(0.5, 1.25)) +
  ggtitle("Atlantic  WOCE, > 750m") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) 

Atl_deepP_g

#See where there are missing values for AOU and phosphate

Atl_13c_aou_po4 <- Atlantic_WOCE %>%
 select(G2cruise, G2station, G2cast, collection_time, 
         G2latitude, G2longitude, G2bottomdepth, G2aou, G2phosphate, G2c13, expocode) 
 
#lots of places are missing phosphate, the entire A16S, lots of A05 and A16N

#start running linear regressions on each line. Start with AOU.

d13C_all_fit <- lm(G2c13 ~ G2aou, data = Atlantic_AOU)

summary(d13C_all_fit)