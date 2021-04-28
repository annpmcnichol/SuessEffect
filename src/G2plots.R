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
write_csv(Atlantic_WOCE, here("data/Atlantic_WOCE.csv"))

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

#now let's do this for each cruise; then figure out how to do it with one command

A16S_AOU <- filter(Atlantic_AOU, expocode == "A16S")
A16S_AOU_fit <- lm(G2c13 ~ G2aou, data = A16S_AOU)
summary(A16S_AOU_fit)


A16N_AOU <- filter(Atlantic_AOU, expocode == "A16N")
A16N_AOU_fit <- lm(G2c13 ~ G2aou, data = A16N_AOU)
summary(A16N_AOU_fit)


A20_AOU <- filter(Atlantic_AOU, expocode == "A20")
A20_AOU_fit <- lm(G2c13 ~ G2aou, data = A20_AOU)
summary(A20_AOU_fit)


A22_AOU <- filter(Atlantic_AOU, expocode == "A22")
A22_AOU_fit <- lm(G2c13 ~ G2aou, data = A22_AOU)
summary(A22_AOU_fit)


A05_AOU <- filter(Atlantic_AOU, expocode == "A05")
A05_AOU_fit <- lm(G2c13 ~ G2aou, data = A05_AOU)
summary(A05_AOU_fit)



A_AOU <- filter(Atlantic_AOU, expocode == c("A16N","A20", "A22", "A05"))
A_AOU_fit <- lm(G2c13 ~ G2aou, data = A_AOU)
summary(A_AOU_fit)

#plot density and depth

Atl_sig0_depth_g <- 
  ggplot(Atlantic_WOCE, aes(x = G2depth, y = G2sigma0, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Depth") +
  scale_y_continuous(name = "sigma0") +
  ggtitle("Atlantic  WOCE, sigma0 vs depth") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 2) 

Atl_sig0_depth_g

#plot d13C vs AOU for samples deeper than 1500 m

Atlantic_AOU_deep <-
  filter(Atlantic_WOCE, G2depth > 1500)


Atl_AOU_deep_f <-
  ggplot(Atlantic_AOU_deep, aes(x = G2aou, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "AOU") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 1.5)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(facets = vars(expocode))

Atl_AOU_deep_f

#Now calculate the slopes for the deep water, i.e. water with  no influence of anthro CO2

A16S_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A16S")
A16S_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A16S_AOU_deep)
summary(A16S_AOU_deep_fit)


A16N_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A16N")
A16N_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A16N_AOU_deep)
summary(A16N_AOU_deep_fit)


A20_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A20")
A20_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A20_AOU_deep)
summary(A20_AOU_deep_fit)


A22_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A22")
A22_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A22_AOU_deep)
summary(A22_AOU_deep_fit)


A05_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A05")
A05_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A05_AOU_deep)
summary(A05_AOU_deep_fit)



A_AOU_deep <- filter(Atlantic_AOU_deep, expocode == c("A16N","A20", "A22", "A05"))
A_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A_AOU_deep)
summary(A_AOU_deep_fit)

#Now let's get the shallow data for ODV plots; right now I need A20 and A22



Atlantic_shallow <-
  filter(Atlantic_WOCE, G2depth < 1500)

A20_shallow <- filter(Atlantic_shallow, expocode == "A20")
write_csv(A20_shallow, here("data/A20_shallow.csv"))

A22_shallow <- filter(Atlantic_shallow, expocode == "A22")
write_csv(A22_shallow, here("data/A22_shallow.csv"))

#find the stations in A16S and A16N that are co-located or close

A16_colocate <-
  filter(Atlantic_WOCE, expocode== c("A16N", "A16S"), between(G2latitude, -10, 10))
write_csv(A16_colocate, here("data/A16_colocate.csv"))

#trying to use these data in ODV didn't work, so let's try it in R. Trying to 
#plot d13C vs pressure for A16 stations between 2°N and -5°S; need to be identified
#by cruise and by station. 

A16_compare <-
  filter(Atlantic_WOCE, expocode== c("A16N", "A16S"), between(G2latitude, -5.5, 2.5), +
           !(G2station %in% c(10,1,14)), G2pressure > 1000) 

A16_compare_g <-
  ggplot(A16_compare, aes(x = G2c13, y = G2pressure, group = expocode, shape=as.factor(G2station), color=expocode)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "d13C, o/oo", limits=c(0.25, 1.25)) +
  scale_y_reverse(name = "Pressure") +
  ggtitle("Atlantic  Overlap") +
  geom_jitter(alpha = 1.0, size = 4) 

A16_compare_g

A16_test <-
  filter(Atlantic_WOCE, expocode=="A16N", between(G2latitude, -5.5, 2.5))

A16_test_g <-
  ggplot(A16_test, aes(x = G2c13, y = G2pressure, group = as.factor(G2station), shape=as.factor(G2station))) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "d13C, o/oo") +
  scale_y_reverse(name = "Pressure") +
  ggtitle("Atlantic  WOCE") +
  geom_jitter(alpha = 1.0, size = 1) 

A16_test_g

## Need to calculate age so can plot AOU vs 14C at depth; example function below

###Calculate D14C (seawater_dc14), need to define a variable and a function
####lambda14 <- 0.00012097
####calcd14c <- function(fm, yc) {1000*( -1 + fm*exp(lambda14*(1950 - yc)))}

### for age: Rage = -8033*ln(fm); fm = (D14C + 1000)/(lambda*(1950 - yc))
### let's do it!
### First parse out year.

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(coll_yr = (format(as.Date(Atlantic_WOCE$collection_time, format="%Y-%m-%d %H:%M:%S"),"%Y")))
  Atlantic_WOCE$coll_yr <- as.numeric(as.character(Atlantic_WOCE$coll_yr))

lambda14 <- 0.00012097
calc_age <- function(D14C, year) {-8033*log(((D14C/1000) + 1)/exp(lambda14*(1950 - year)))}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Rage = calc_age(G2c14, coll_yr))

####Now for the plots

Atlantic_WOCE_deep <- filter(Atlantic_WOCE, G2pressure > 1000)

Atl_AOU_Age_f <-
  ggplot(Atlantic_WOCE_deep, aes(x = Rage, y = G2aou, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "AOU") +
  scale_y_continuous(name = "Rad Age, yr") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(facets = vars(expocode))

Atl_AOU_Age_f

###next plot 14C and SiO2




