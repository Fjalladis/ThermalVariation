# install package from GitHub
install.packages("devtools")
remotes::install_github("padpadpadpad/rTPC")
install.packages("nls.multstart")
install.packages("tidyverse")

# load packagess
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(ggplot2)
library(readxl)

#load data
dataE <- read_xlsx("dataTPC.xlsx")

# show the data
ggplot(dataE, aes(Temp, Rate)) +
  geom_point() +
  theme_bw(base_size = 12)
  


#----------O Neill model----------------------------

# get start values and fit model
start_vals <- get_start_vals(dataE$Temp, dataE$Rate, model_name = 'oneill_1972')
# fit model
mod <- nls.multstart::nls_multstart(Rate~oneill_1972(temp = Temp, rmax, ctmax, topt, q10),
                                    data = dataE,
                                    iter = 500,
                                    start_lower = start_vals - 10,
                                    start_upper = start_vals + 10,
                                    lower = get_lower_lims(dataE$Temp, dataE$Rate, model_name = 'oneill_1972'),
                                    upper = get_upper_lims(dataE$Temp, dataE$Rate, model_name = 'oneill_1972'),
                                    supp_errors = 'Y',
                                    convergence_count = FALSE)

# look at model fit
start_vals
summary(mod)
calc_params(mod)
get_ctmin(mod)
get_lower_lims(dataE$Temp, dataE$Rate, model_name = "oneill_1972")
get_upper_lims(dataE$Temp, dataE$Rate, model_name = "oneill_1972")

# get the mean values
df.summary <- dataE %>%
  group_by(Temp) %>%
  summarise(
    sd = sd(Rate, na.rm = TRUE),
    Rate = mean(Rate))
df.summary 

ggplot(df.summary) +
  geom_point(aes(Temp, Rate)) +
  #geom_line(aes(Temp, Rate), col = 'blue') +
  theme_bw()

#used parameters from ONeill model above and equation (2) to calculate rate values

d <- read_excel("Oneill.xlsx")

plot(d$Temp, d$Rate)

ggplot() +
  geom_point(aes(Temp, Rate), df.summary, col="black", size=3, alpha=0.5) +
  geom_point(aes(Temp, Rate), dataE, col="grey", alpha=0.5)+
  geom_line(aes(Temp, Rate),size=1.5, d, col = '#66c2a5') +
  theme_classic(base_size = 15)+
  xlab("Temperature (°C)")+
  ylab(expression("Growth rate (mm day"^-1*")"))
  



