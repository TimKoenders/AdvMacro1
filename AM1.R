########### Advanced Macroeconomics 1 - Exercises ###########

## Question 1 --------------------------------------------------------------
# Set-up ------------------------------------------------------------------
rm(list=ls())
#libraries
pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  dplyr,
  magic,
  coda,
  car,
  paramhetero,
  stargazer,
  ggplot,
  magick,
  openxlsx,
  dplyr,
  stargazer,
)
#Seed for reproducability
set.seed(84)

pwt=read.xlsx("pwt1001.xlsx",sheet="Data")
#cgdpo=Output-side real GDP at current PPPs (in mil. 2017US$)
#csh_i= Share of gross capital formation at current PPPs
#pop = Population (in millions)
pwt0=pwt[,c("countrycode","year","cgdpo","csh_i","pop")]

# Unconditional beta-convergence ------------------------------------------
pwt1=pwt0%>%filter(year%in%c(1960,1990))%>%
  group_by(countrycode)%>%
  filter(all(!is.na(cgdpo)))%>%
  arrange(year)%>%
  summarise(lgGDPgrowth=log(last(cgdpo))-log(first(cgdpo))/30,lginitial=log(first(cgdpo)))
mod1=lm(lgGDPgrowth~lginitial,data=pwt1)
pwt2=pwt0%>%filter(year%in%c(1990,2019))%>%
  group_by(countrycode)%>%
  filter(all(!is.na(cgdpo)))%>%
  arrange(year)%>%
  summarise(lgGDPgrowth=(log(last(cgdpo))-log(first(cgdpo))/29),lginitial=log(first(cgdpo)))
mod2=lm(lgGDPgrowth~lginitial,data=pwt2)
table_plot <- stargazer(list(mod1, mod2),
                        header = FALSE,
                        column.labels = c("1960-1990", "1990-2019"),
                        type = "latex", float = FALSE,keep.stat = c("n","aic","rsq"),
                        omit.stat = "all")


# Conditional beta-convergence --------------------------------------------
popgr1=pwt0%>%
  group_by(countrycode)%>%
  arrange(year)%>%
  #mutate(popGrowth=(pop-lag(pop))/lag(pop))%>%
  mutate(popGrowth=log(pop)-log(lag(pop)))

pwt1.2=popgr1%>%
  filter(year%in%c(1960,1990))%>%
  group_by(countrycode)%>%
  filter(all(!is.na(cgdpo)))%>%
  arrange(year)%>%
  summarise(lgGDPgrowth=log(last(cgdpo))-log(first(cgdpo)),lginitial=log(first(cgdpo)),
            diffpopgr=(last(popGrowth)-first(popGrowth))/30,
            #lgPopgrowth=(log(last(pop))-log(first(pop)))/30,
            diffcsh=(last(csh_i)-first(csh_i))/30)

mod1.2=lm(lgGDPgrowth~lginitial+diffpopgr+diffcsh,data=pwt1.2)
pwt1.3=popgr1%>%
  filter(year%in%c(1990,2019))%>%
  group_by(countrycode)%>%
  filter(all(!is.na(cgdpo)))%>%
  arrange(year)%>%
  summarise(lgGDPgrowth=(log(last(cgdpo))-log(first(cgdpo)))/29,
            lginitial=log(first(cgdpo)),
            diffpopgr=(last(popGrowth)-first(popGrowth))/29,
            #lgPopgrowth=((log(last(pop))-log(first(pop)))/29),
            diffcsh=(last(csh_i)-first(csh_i))/29)

mod1.3=lm(lgGDPgrowth~lginitial+diffcsh+diffpopgr,data=pwt1.3)
stargazer(list(mod1,mod2,mod1.2,mod1.3), header = FALSE,
          column.labels = c("1960-1990", "1990-2019","1960-1990","1990-2019"),type="latex", float = FALSE,keep.stat = c("n","aic","rsq"))








# Question 2 --------------------------------------------------------------
# Set-up ------------------------------------------------------------------
rm(list=ls())
#libraries
pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  dplyr,
  magic,
  coda,
  car,
  paramhetero,
  stargazer,
  ggplot,
  magick,
  openxlsx,
  dplyr,
  stargazer
)
#Seed for reproducability
set.seed(84)

#Read in data
mrw <- read.csv("mrw.csv")

## Create variables for regression
mrw$log_gdp85 <-  log(mrw$rgdpw85)
mrw$log_gdp60 <-  log(mrw$rgdpw60)
mrw$diff_gdp <- log(mrw$rgdpw85 / mrw$rgdpw60)
mrw$iy_minus_ndg <- log(mrw$i_y/(mrw$popgrowth/100 + 0.05))
mrw$school_minus_ndg <- log(mrw$school / (mrw$popgrowth/100 + 0.05))
mrw$log_gdp85 <-  log(mrw$rgdpw85)
mrw$log_gdp60 <-  log(mrw$rgdpw60)
mrw_o <- subset(mrw,o==1)





# Table IV from MRW----------------------------------------------------------------
# Like MRW we assume g + delta = 0.05

# Non-oil countries
tablevi_n <- lm(diff_gdp ~ log(rgdpw60)+iy_minus_ndg+school_minus_ndg,data=subset(mrw,n==1))
coefficients(tablevi_n)

# Intermediate countries
tablevi_i <- lm(diff_gdp ~ log(rgdpw60)+iy_minus_ndg+school_minus_ndg,data=subset(mrw,i==1))
coefficients(tablevi_i)

# OECD countries
tablevi_o <- lm(diff_gdp ~ log(rgdpw60)+iy_minus_ndg+school_minus_ndg,data=subset(mrw,o==1))
coefficients(tablevi_o)

# Implied lambdas
# From the coefficient for log(rgdpw60) we can derive the implied value for lambda
# lambda = - ln(1+coef)/time
lambda_n <- - log(1+coefficients(tablevi_n)[2])/25
lambda_i <- - log(1+coefficients(tablevi_i)[2])/25
lambda_o <- - log(1+coefficients(tablevi_o)[2])/25

# Implied alpha and betas
# we can solve for them by solving the follwing two equations, which we got from solving the regression equation for alpha and beta
# alpha = coef_invest * beta / coef_school
# beta = coef_school / (coef_school + coef_invest + (1-exp(-lambda*time)))
beta_n <- coefficients(tablevi_n)[4] / (coefficients(tablevi_n)[4] + coefficients(tablevi_n)[3] + (1-exp(-lambda_n*25)))
alpha_n <- coefficients(tablevi_n)[3]*beta_n / coefficients(tablevi_n)[4]

beta_i <- coefficients(tablevi_i)[4] / (coefficients(tablevi_i)[4] + coefficients(tablevi_i)[3] + (1-exp(-lambda_i*25)))
alpha_i <- coefficients(tablevi_i)[3]*beta_i / coefficients(tablevi_i)[4]

beta_o <- coefficients(tablevi_o)[4] / (coefficients(tablevi_o)[4] + coefficients(tablevi_o)[3] + (1-exp(-lambda_o*25)))
alpha_o <- coefficients(tablevi_o)[3]*beta_o / coefficients(tablevi_o)[4]

table_n <- summary(tablevi_n)
table_i <- summary(tablevi_i)
table_o <- summary(tablevi_o)
table_n <- table_n$coefficients[2:4,1:2]
table_i <- table_i$coefficients[2:4,1:2]
table_o <- table_o$coefficients[2:4,1:2]

r_n <- summary(tablevi_n)$adj.r.squared
r_i <- summary(tablevi_i)$adj.r.squared
r_o <- summary(tablevi_o)$adj.r.squared
lab_n <- c(r_n,lambda_n, alpha_n, beta_n) # adj r squared, implied lambda, alpha and beta
lab_i <- c(r_i,lambda_i, alpha_i, beta_i)
lab_o <- c(r_o, lambda_o, alpha_o, beta_o)

table_VI <- matrix(c(table_n[,1],lab_n,table_n[,2],c(NA,NA,NA,NA),table_i[,1],lab_i,table_i[,2],c(NA,NA,NA,NA),table_o[,1],lab_o,table_o[,2],c(NA,NA,NA,NA)), ncol=6)

colnames_tables <- c("Non-oil", "SE", "Intermediate", "SE", "OECD", "SE")
colnames(table_VI) <- colnames_tables
rownames_table <- c("ln(Y60)","ln(I/GDP)-ln(n+d+g)","ln(School)-ln(n+d+g)","adj. R-Squared", "Implied lambda", "Implied alpha", "Implied beta")
rownames(table_VI) <- rownames_table
knitr::kable(table_VI, digits=3)

# The effect of human capital on economic growth --------------------------
# Again, like MRW we assume g + delta = 0.05

# African countries, in the data set the first 43 countries are African
tablevi_a <- lm(diff_gdp ~ log(rgdpw60)+iy_minus_ndg+school_minus_ndg,data=mrw[1:43,])
coefficients(tablevi_a)

# Rest of the world
tablevi_r <- lm(diff_gdp ~ log(rgdpw60)+iy_minus_ndg+school_minus_ndg,data=mrw[44:121,])
coefficients(tablevi_r)

mlist <- list(tablevi_a,tablevi_r)
table_hc <- as.matrix(compare_coefs(mlist)[,-c(6)])
knitr::kable(table_hc, digits=3)


# Speed of convergence ----------------------------------------------------
# Implied lambdas
# From the coefficient for log(rgdpw60) we can derive the implied value for lambda
# lambda = - ln(1+coef)/time
lambda_a <- - log(1+coefficients(tablevi_a)[2])/25
lambda_r <- - log(1+coefficients(tablevi_r)[2])/25
table_lambda <- matrix(c(lambda_a,lambda_r),ncol=2)
colnames_lambda <- c("Convergence rate in Africa", "Convergence rate in rest of the world")
colnames(table_lambda) <- colnames_lambda 
knitr::kable(table_lambda)

