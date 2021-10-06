#Select Atlantic data; use expocodes

##Run these at the beginning of a session

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

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(coll_yr = (format(as.Date(Atlantic_WOCE$collection_time, format="%Y-%m-%d %H:%M:%S"),"%Y")))
Atlantic_WOCE$coll_yr <- as.numeric(as.character(Atlantic_WOCE$coll_yr))

lambda14 <- 0.00012097
calc_age <- function(D14C, year) {-8033*log(((D14C/1000) + 1)/exp(lambda14*(1950 - year)))}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Rage = calc_age(G2c14, coll_yr))

calc_Palk <- function(Alk, nitrate, salinity) {((Alk + nitrate)*35)/salinity}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Palk = calc_Palk(G2talk, G2nitrate, G2salinity)) 

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Palk_mod = Palk - 2320)

P_star <- function(phosphate, oxygen) {phosphate - 1.95 + (oxygen/170)}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Pst = P_star(G2phosphate, G2oxygen))

####end of routines to run before starting

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
  geom_jitter(alpha = 1.0, size = 1) +
  stat_regline_equation(aes(label = ..eq.label..)) 

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

Atl_14C_SiO2_f <-
  ggplot(Atlantic_WOCE, aes(x = G2silicate, y = G2c14, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "SiO2") +
  scale_y_continuous(name = "D14C") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(intercept=-70, slope=-1, linetype = 3) +
  facet_wrap(facets = vars(expocode), nrow = 5)

Atl_14C_SiO2_f

###look at deep only

Atl_14C_SiO2_deep_f <-
  ggplot(Atlantic_WOCE_deep, aes(x = G2silicate, y = G2c14, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "SiO2") +
  scale_y_continuous(name = "D14C") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(intercept=-70, slope=-1, linetype = 3) +
  facet_wrap(facets = vars(expocode))

Atl_14C_SiO2_deep_f


###calculate potential Alk and then plot DI14C vs Palk;
### Palk = (Alk + nitrate)*35/Salinity

###Damn! I don't have nitrate in the file: got it!

calc_Palk <- function(Alk, nitrate, salinity) {((Alk + nitrate)*35)/salinity}
  
Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Palk = calc_Palk(G2talk, G2nitrate, G2salinity)) 

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Palk_mod = Palk - 2320)

Atl_14C_Palk_f <-
  ggplot(Atlantic_WOCE, aes(x = (Palk_mod), y = G2c14, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values= cbbPalette) +
  scale_fill_manual(values= cbbPalette) +
  scale_x_continuous(name = "Palk - 2320") +
  scale_y_continuous(name = "D14C") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(intercept=-53, slope=-1, linetype = 3) +
  facet_wrap(facets = vars(expocode), nrow = 5)

Atl_14C_Palk_f

#A16N_AOU_deep <- filter(Atlantic_AOU_deep, expocode == "A16N")
#A16N_AOU_deep_fit <- lm(G2c13 ~ G2aou, data = A16N_AOU_deep)
#summary(A16N_AOU_deep_fit)


A16N_Palk <- filter(Atlantic_WOCE, expocode == "A16N")
A16N_Palk_fit <- lm(G2c14 ~ Palk_mod, data = A16N_Palk)
summary(A16N_Palk_fit)


##use ggplot2 to add the equation; take function from stackoverflow

lm_eqn <- function(df){
  m <- lm(G2c14 ~ Palk_mod, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

A16N_Palk <- filter(Atlantic_WOCE, expocode == "A16N")

A16N_14C_Palk <-
  ggplot(A16N_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values= cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A16N") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#E69F00") +
  geom_smooth(method = "lm", col = "#E69F00" ) +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A16N_14C_Palk_reg <- A16N_14C_Palk + geom_text(x=0, y=-200,label = lm_eqn(A16N_Palk), parse = TRUE)

A16N_14C_Palk_reg

##I think the only way to do what I want is to make 4 plots 
##and put them on a grid (it would be 5 plots except A16S has no data)

###top of failed attempt
Atl_14C_Palk_f <-
  ggplot(Atlantic_WOCE, aes(x = (Palk_mod), y = G2c14, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320") +
  scale_y_continuous(name = "D14C") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm") +
  geom_abline(intercept=-53, slope=-1, linetype = 3) +
  geom_text(x=75, y=100,label = lm_eqn(Atlantic_WOCE,aes(x = (Palk_mod), y = G2c14, group = expocode)), parse = TRUE) +
  facet_wrap(facets = vars(expocode), nrow = 5)

Atl_14C_Palk_f 
###bottom of failed attempt

##I think the only way to do what I want is to make 4 plots 
##and put them on a grid (it would be 5 plots except A16S has no data)

A20_Palk <- filter(Atlantic_WOCE, expocode == "A20")

A20_14C_Palk <-
  ggplot(A20_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A20") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#56B4E9") +
  geom_smooth(method = "lm", col = "#56B4E9") +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A20_14C_Palk_reg <- A20_14C_Palk + geom_text(x=75, y=100,label = lm_eqn(A20_Palk), parse = TRUE)

A20_14C_Palk_reg

##A22

A22_Palk <- filter(Atlantic_WOCE, expocode == "A22")

A22_14C_Palk <-
  ggplot(A22_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A22") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#009E73") +
  geom_smooth(method = "lm", col = "#009E73") +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A22_14C_Palk_reg <- A22_14C_Palk + geom_text(x=75, y=100,label = lm_eqn(A22_Palk), parse = TRUE)

A22_14C_Palk_reg

##A05

A05_Palk <- filter(Atlantic_WOCE, expocode == "A05")

A05_14C_Palk <-
  ggplot(A05_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A05") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col ="#F0E442") +
  geom_smooth(method = "lm", col = "#F0E442") +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A05_14C_Palk_reg <- A05_14C_Palk + geom_text(x=75, y=100,label = lm_eqn(A05_Palk), parse = TRUE)

A05_14C_Palk_reg

##Start using PO4* instead of PO4

P_star <- function(phosphate, oxygen) {phosphate - 1.95 + (oxygen/170)}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(Pst = P_star(G2phosphate, G2oxygen))

Atl_PO4_star_f <-
  ggplot(Atlantic_WOCE, aes(x = Pst, y = G2c13, group = expocode, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Phosphate_star") +
  scale_y_continuous(name = "d13C, o/oo", breaks = seq(0.0,2.25, 0.25), limits = c(0.0, 2.25)) +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

Atl_PO4_star_f

##Calculate the fraction of northern and southern waters
##Broecker et al 1991


f_north <- function(pstar) {(1.67 - pstar)/0.94}

Atlantic_WOCE <- Atlantic_WOCE %>%
     mutate(f_n = f_north(Pst))

Atlantic_WOCE_deep2 <- filter(Atlantic_WOCE, G2pressure > 2000)

A_dum_f <-
     ggplot(Atlantic_WOCE_deep2, aes(x=G2longitude, y = G2oxygen, group = expocode, shape=expocode, color=expocode )) +
     theme_bw() + 
     geom_jitter(alpha = 1.0, size = 1) +
     facet_wrap(facets = vars(expocode))

A_dum_f

###try to calculate O2 utilization rate using Broecker's eqn

del_oxy <- function(f_north, oxygen) {250 + 30*f_north - oxygen}
del_radioc <- function(f_north, c14) {-1.58 +90*f_north - c14}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(O2_change = del_oxy(f_n, G2oxygen)) 
Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(C14_change = del_radioc(f_n, G2c14))

###select data for deep samples and samples between 41N and 30S

Atlantic_WOCE_deep2 <- filter(Atlantic_WOCE, (G2pressure > 2000) & (G2latitude < 41 & G2latitude > -30))

###This selects the data I want but there are some samples from A22 that have
###very low oxygen values relative to the rest of the cruise. Are these the
###Caribbean values that Broecker referred to? They result in very high values
###for the oxygen change value. Probably. Exclude them now. Need to confirm
###but for now I will exclude after I calculate oxygen change. Now exclude the 
###two points with very low oxygen and D14C change.

Atlantic_WOCE_deep2 <- filter(Atlantic_WOCE, (G2pressure > 2000) & (G2latitude < 41 & G2latitude > -30) & (O2_change < 55) &(C14_change >100) & (G2longitude < -20)) 
                                

A_our_g <-
  ggplot(Atlantic_WOCE_deep2, aes(x = C14_change, y = O2_change, color = G2pressure)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "D14C change, o/oo") +
  scale_y_continuous(name = "O2 change, units") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm")

A_our_g

A_our_deep_fit <- lm(O2_change ~ C14_change, data = Atlantic_WOCE_deep2)
summary(A_our_deep_fit)
  
A_our_whp_g <-
  ggplot(Atlantic_WOCE_deep2, aes(x = C14_change, y = O2_change, shape=expocode, color=expocode)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "D14C change, o/oo") +
  scale_y_continuous(name = "O2 change, units") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) 

A_our_whp_g

##Working with Palk again. Need to find what data points
##are above the natural line and, for each cruise, what
##depth this refers to. First get eqation for each leg.
##then plot DI14xs vs depth (pressure) for each leg. 

A16N_Palk <- filter(Atlantic_WOCE, expocode == "A16N")

A16N_14C_Palk <-
  ggplot(A16N_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values= cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A16N") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#E69F00") +
  geom_smooth(method = "lm", col = "#E69F00" ) +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A16N_14C_Palk_reg <- A16N_14C_Palk + geom_text(x=0, y=-200,label = lm_eqn(A16N_Palk), parse = TRUE)

A16N_14C_Palk_reg

A16N_Palk <- A16N_Palk %>%
  mutate(DI14Cxs = 69 - 1.7*Palk_mod)

A_dum_f <-
  ggplot(A16N_Palk, aes(x=G2depth, y = DI14Cxs, color = G2latitude )) +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

A_dum_f

##A20

A20_Palk <- filter(Atlantic_WOCE, expocode == "A20")

A20_14C_Palk <-
  ggplot(A20_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A20") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#56B4E9") +
  geom_smooth(method = "lm", col = "#56B4E9") +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A20_14C_Palk_reg <- A20_14C_Palk + geom_text(x=75, y=100,label = lm_eqn(A20_Palk), parse = TRUE)

A20_14C_Palk_reg

A20_Palk <- A20_Palk %>%
  mutate(DI14Cxs = 44 - 1.5*Palk_mod)

A_dum_f <-
  ggplot(A20_Palk, aes(x=G2depth, y = DI14Cxs, color = G2latitude )) +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

A_dum_f

##A22

A22_Palk <- filter(Atlantic_WOCE, expocode == "A22")

A22_14C_Palk <-
  ggplot(A22_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A22") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#56B4E9") +
  geom_smooth(method = "lm", col = "#56B4E9") +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A22_14C_Palk_reg <- A22_14C_Palk + geom_text(x=75, y=100,label = lm_eqn(A22_Palk), parse = TRUE)

A22_14C_Palk_reg

A22_Palk <- A22_Palk %>%
  mutate(DI14Cxs = 57 - 1.6*Palk_mod)

A_dum_f <-
  ggplot(A22_Palk, aes(x=G2depth, y = DI14Cxs, color = G2latitude )) +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

A_dum_f

##A05

A05_Palk <- filter(Atlantic_WOCE, expocode == "A05")

A05_14C_Palk <-
  ggplot(A05_Palk, aes(x = (Palk_mod), y = G2c14)) +
  scale_shape_manual(values=c(21:25)) +
  scale_color_manual(values= cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_x_continuous(name = "Palk - 2320", limits = c(-50,110)) +
  scale_y_continuous(name = "D14C", limits = c(-225, 125)) +
  ggtitle("A05") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1, col = "#E69F00") +
  geom_smooth(method = "lm", col = "#E69F00" ) +
  geom_abline(intercept=-53, slope=-1, linetype = 1) 

A05_14C_Palk_reg <- A05_14C_Palk + geom_text(x=0, y=-200,label = lm_eqn(A05_Palk), parse = TRUE)

A05_14C_Palk_reg

A05_Palk <- A05_Palk %>%
  mutate(DI14Cxs = 55 - 1.9*Palk_mod)

A_dum_f <-
  ggplot(A05_Palk, aes(x=G2depth, y = DI14Cxs, color = G2longitude )) +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  facet_wrap(facets = vars(expocode))

A_dum_f

##Go back to looking at 14C and O2
##Restrict to south of 41N and north of 30S )-30)
##deeper than 200m
##how to get rid of Caribbean results?

f_north <- function(pstar) {(1.67 - pstar)/0.94}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(f_n = f_north(Pst))

del_oxy <- function(f_north, oxygen) {250 + 30*f_north - oxygen}
del_radioc <- function(f_north, c14) {-1.58 +90*f_north - c14}

Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(O2_change = del_oxy(f_n, G2oxygen)) 
Atlantic_WOCE <- Atlantic_WOCE %>%
  mutate(C14_change = del_radioc(f_n, G2c14))

Atlantic_WOCE_deep2 <- filter(Atlantic_WOCE, (G2pressure > 2000) & (G2latitude < 41 & G2latitude > -30) & (O2_change < 55) &(C14_change >100)) 

A_our_g <-
  ggplot(Atlantic_WOCE_deep2, aes(x = C14_change, y = O2_change, color = G2longitude)) +
  scale_x_continuous(name = "D14C change, o/oo") +
  scale_y_continuous(name = "O2 change, units") +
  ggtitle("Atlantic  WOCE") +
  theme_bw() + 
  geom_jitter(alpha = 1.0, size = 1) +
  geom_smooth(method = "lm", col = "#E69F00" ) 

A_our_g

A_our_deep_fit <- lm(O2_change ~ C14_change, data = Atlantic_WOCE_deep2)
summary(A_our_deep_fit)

##Need to separate into east and west basins



