# rm(list = ls())
set.seed(44042)

# replace with your working directory containing the data Lars handed out. 
setwd("D:/Dropbox (Personal)/UCL/ECON0072/Practical1")

install.packages("pacman")
pacman::p_load(tidyverse)

load("firmData0072.RData")

# Variable Names 
colnames(firmData)

# Variable Names, but without the year repetition 
unique(gsub("_[0-9]+$", "", colnames(firmData)))

summary(firmData)[7, ]

# keep only if does NOT have missing values for turnover in 2017
iDrop <- is.na(firmData$turnover_th_gbp_2017)
paste(sum(iDrop), "/", nrow(firmData)) 
firmData <- firmData[!iDrop, ]

# Easier to work with long version of panel data. 
names(firmData) <- gsub("([_a-zA-Z])_([0-9]+)$", "\\1\\.\\2", names(firmData))
firmData <- reshape(firmData, idvar = "company_name", timevar = "year", times = 2017:2021, direction = "long", varying = 21:90)

# Clean up
firmData <- firmData[order(firmData$company_name, firmData$year), ]
rownames(firmData) <- 1:nrow(firmData)
names(firmData) <- gsub("_th_gbp", "", names(firmData))

#### 3. Plot histograms of some key variables (turnover in some year, employees, fixed assets). ####

# Turnover
summary(firmData$turnover)
head(firmData[order(firmData$turnover, decreasing = T), c("company_name", "year", "turnover")])
head(firmData[order(firmData$turnover, decreasing = F), c("company_name", "year", "turnover")])

# Profits = Revenue - Costs -> Revenue = Profits + Costs (from suggestion in class)
firmData$revenue <- abs(firmData$operating_profit) - firmData$cost_of_sales - firmData$administration_expenses + firmData$remuneration

head(firmData[order(firmData$revenue, decreasing = F), c("company_name", "year", "revenue")])
head(firmData[order(firmData$revenue, decreasing = T), c("company_name", "year", "revenue")])

# re. measurement error: notice "Lasig Realisations 2" (restaurant chain Las Iguanas) paid an inordinate remuneration in 2019. 
# a proper analysis of this data set would spend much more time trying to weed out these anomalies. 

hist(firmData$revenue)
hist(log(firmData$revenue))

# Employees
summary(firmData$number_of_employees)
head(firmData[order(firmData$number_of_employees, decreasing = T), c("company_name", "year", "number_of_employees")])
head(firmData[order(firmData$number_of_employees, decreasing = F), c("company_name", "year", "number_of_employees")])
hist(firmData$number_of_employees)
hist(log(firmData$number_of_employees))

# Fixed Assets
summary(firmData$fixed_assets)
head(firmData[order(firmData$fixed_assets, decreasing = T), c("company_name", "year", "fixed_assets")])
head(firmData[order(firmData$fixed_assets, decreasing = F), c("company_name", "year", "fixed_assets")])
hist(firmData$fixed_assets)
hist(log(firmData$fixed_assets))

#### 4. Tangible assets is the sum of several of the other asset variables. Which ones? Similarly, fixed assets is the sum of which variables? ####

# annoying
firmData$full_overview <- NULL

summary(firmData[, c("tangible_assets", "land__buildings", "fixtures__fittings", "plant__vehicles", "other_fixed_assets")])

names(firmData)

# this may not be the case, but it seems FAME assumes so.
for (i in 22:36) {
  firmData[is.na(firmData[, i]), i] <- 0
}

# Tangible Assets = Land + Fixtures + Plant + Other Fixed.
firmData$LFPO <- firmData$land__buildings + firmData$fixtures__fittings + firmData$plant__vehicles + firmData$other_fixed_assets
summary(firmData[, c("tangible_assets", "LFPO")])

problematic <- firmData[(firmData$tangible_assets - firmData$LFPO) > 1, c("company_name", "year", "tangible_assets", "LFPO")]
problematic[order(problematic$tangible_assets - problematic$LFPO, decreasing = T), ]
# firm-year observations for which LFPO != Tangible Assets seem to be banks that report tangible_assets but no components for the latest years. 

# Aside for these companies (mostly banks): (i) tangible assets =~ land + fixtures + plant + other; (ii) FAME takes missing values as 0 towards these measures.

# Fixed Assets = Tangible + Intangible + Investments.
firmData[order(firmData$tangible_assets - firmData$fixed_assets, decreasing = T), 21:35]
firmData$TII <- firmData$tangible_assets + firmData$intangible_assets + firmData$investments_fixed_assets
summary(firmData[, c("fixed_assets", "TII")]) 

problematic <- firmData[(firmData$fixed_assets - firmData$TII) > .1, c("company_name", "year", "fixed_assets", "TII")]
problematic[order(problematic$fixed_assets - problematic$TII, decreasing = T), ]

hist(problematic$fixed_assets - problematic$TII)

#### Estimate a production function for each year. What are the inputs? What is output? ####

# This is by no means an easy question. 
# Matter of fact, one of the debates in recent applied production function estimation literature surrounds these questions exactly:
# De Loecker Eeckhout Unger (2020, https://academic.oup.com/qje/article/135/2/561/5714769) use cost of sales (COGS) to proxy variable costs in cost function estimation.
# Traina (2021, https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3120849) objects to that, and argues for including administrative costs (SGA). 
# Turns out this matters A LOT for the time series evidence in DLEU (2020) - see Section 4 in Traina (2021).

# Note we have variables in GBP, but no prices to deflated them and get quantities. 
# This is an issue we are often confronted with, and an issue that creates significant problems in intepretating our estimates: elasticity of revenue != output elasticity.
# E.g. in the case of constant elasticity of demand, revenue elasticity *underestimates* output elasticities (see Klette and Griliches, 1996), as the former depend on market power.

# Let's start with a simple Cobb Douglas production function. 
# Notice Revenue (= Revenue for the model), Fixed Assets (= Capital), and Number of Employees (= Employment) are often zero or negative. 
# We exclude those observations for Cobb Douglas estimation.
firmData$logR <- log(firmData$revenue)
firmData$logR[is.nan(firmData$logR) | is.infinite(firmData$logR)] <- NA 
firmData$logK <- log(firmData$fixed_assets)
firmData$logK[is.nan(firmData$logK) | is.infinite(firmData$logK)] <- NA
firmData$logL <- log(firmData$number_of_employees)
firmData$logL[is.nan(firmData$logL) | is.infinite(firmData$logL)] <- NA

cd2017 <- lm(data = firmData[(firmData$year == 2017), c("logR", "logK", "logL")], logR ~ logK + logL)
summary(cd2017)

hist(cd2017$residuals) 
qqnorm(cd2017$residuals) 
abline(a = 0, b = 1)

cd2018 <- lm(data = firmData[(firmData$year == 2018), c("logR", "logK", "logL")], logR ~ logK + logL)
summary(cd2018)

hist(cd2018$residuals) 
qqnorm(cd2018$residuals) 
abline(a = 0, b = 1)

cd2019 <- lm(data = firmData[(firmData$year == 2019), c("logR", "logK", "logL")], logR ~ logK + logL)

summary(cd2019)

hist(cd2019$residuals) 
qqnorm(cd2019$residuals) 
abline(a = 0, b = 1)

cd2020 <- lm(data = firmData[(firmData$year == 2020), c("logR", "logK", "logL")], logR ~ logK + logL)
summary(cd2020)

hist(cd2020$residuals) 
qqnorm(cd2020$residuals) 
abline(a = 0, b = 1)

# Coefficients (revenue - not output - elasticities) are stable over the last few years. 

data.frame(c2017 = cd2017$coefficients, c2018 = cd2018$coefficients, 
           c2019 = cd2019$coefficients, c2020 = cd2020$coefficients)

# We can also test whether the coefficients sum up to 1 - why do we care?

pacman::p_load(car)
linearHypothesis(cd2017, "logK + logL = 1")
linearHypothesis(cd2018, "logK + logL = 1")
linearHypothesis(cd2019, "logK + logL = 1")
linearHypothesis(cd2020, "logK + logL = 1")

# we always reject beta_K + beta_L = 1.

# What other specifications could we try? Translog, for example. 
# We here implement a simplified version of the translog; in theory, the translog has restrictions on the coefficients that ensure it is a proper approximation to a production function.

firmData$logK2 <- (firmData$logK)^2
firmData$logL2 <- (firmData$logL)^2
firmData$logKlogL <- firmData$logK * firmData$logL

tl2017 <- lm(data = firmData[(firmData$year == 2017), ], logR ~ logK + logL + logK2 + logL2 + logKlogL)
summary(tl2017)

tl2018 <- lm(data = firmData[(firmData$year == 2018), ], logR ~ logK + logL + logK2 + logL2 + logKlogL)
summary(tl2018)

tl2019 <- lm(data = firmData[(firmData$year == 2019), ], logR ~ logK + logL + logK2 + logL2 + logKlogL)
summary(tl2019)

tl2020 <- lm(data = firmData[(firmData$year == 2020), ], logR ~ logK + logL + logK2 + logL2 + logKlogL)
summary(tl2020)

data.frame(c2017 = tl2017$coefficients, c2018 = tl2018$coefficients, 
           c2019 = tl2019$coefficients, c2020 = tl2020$coefficients)

# Or (orthogonal) polynomials: harder to interpret but more flexible.

dt2017 <- firmData[(firmData$year == 2017) & !is.na(firmData$logK) & !is.na(firmData$logL), ]
op2017 <- lm(data = dt2017, logR ~ poly(logK, logL, degree = 2))
summary(op2017)

qqnorm(op2017$residuals)

# but do we gain anything over the translog?
qqplot(tl2017$residuals, op2017$residuals)
abline(a = 0, b = 1)

# Or cubic splines: just as hard to interpret, but also flexible.

pacman::p_load(splines)
sp2017 <- lm(data = firmData[(firmData$year == 2017), ], logR ~ bs(logK) + bs(logL))
summary(sp2017)

# but do we gain anything over the translog?
qqplot(sp2017$residuals, tl2017$residuals)
abline(a = 0, b = 1)

# from residual plots it does not look like these flexible alternatives beat a second order taylor approximation (the translog) for this data set.
# they are also harder to interpret.
# Still, flexible regressions are often used as part of production function estimation algorithms (e.g. Olley Pakes first stage; Ackerberg Caves Frazer first stage).

#### Estimate a production function using panel data methods ####

pacman::p_load(plm)

cdpols <- lm(data = firmData, logR ~ logK + logL)
summary(cdpols)

cdfe <- plm(data = firmData, logR ~ logK + logL, index = c("company_name", "year"), model = "within")
summary(cdfe)

cdfd <- plm(data = firmData, logR ~ logK + logL, index = c("company_name", "year"), model = "fd")
summary(cdfd)

cdre <- plm(data = firmData, logR ~ logK + logL, index = c("company_name", "year"), model = "between")
summary(cdre)

tlfe <- plm(data = firmData, logR ~ logK + logL + logK2 + logL2 + logKlogL, index = c("company_name", "year"), model = "within")
summary(tlfe)

tlfd <- plm(data = firmData, logR ~ logK + logL + logK2 + logL2 + logKlogL, index = c("company_name", "year"), model = "fd")
summary(tlfd)

tlre <- plm(data = firmData, logR ~ logK + logL + logK2 + logL2 + logKlogL, index = c("company_name", "year"), model = "between")
summary(tlre)

# If our CD prod fn is correctly specified, should the FD estimator return the same as pooled OLS? What about FE? Note they require different assumptions on the error terms...

#### Estimate the model using the method of Olley and Pakes discussed in class ####

firmData$K <- exp(firmData$logK)
firmData$logI <- log(firmData$investments_fixed_assets)
firmData$logI[is.nan(firmData$logI) | is.infinite(firmData$logI)] <- NA
firmData <- firmData[order(firmData$company_name, firmData$year), ] %>% group_by(company_name) %>% mutate(lagK = dplyr::lag(logK), lagL = dplyr::lag(logL), lagI = dplyr::lag(logI))

# Note the OP invertibility requirement means omega is strictly increasing in investment, which however in the data is incredibly lumpy:
hist(firmData$investments_fixed_assets)

dtt <- firmData[!is.na(firmData$logK) & !is.na(firmData$logL) & !is.na(firmData$logR) & !is.na(firmData$logI) & !is.na(firmData$lagK) & !is.na(firmData$lagL) & !is.na(firmData$lagI), ]

# Very Inefficient but legible implementation of OP with Bootstrapping.
# Block Bootstrap companies to preserve panel structure.

pacman::p_load(boot)

op <- function(dt, indices) {
  
  dt <- dtt[indices, ]
  # First Stage flexible third-order polynomial regression 
  op1 <- lm(data = dt, logR ~ logL + poly(K, logI, degree = 3))
  # summary(op1)
  
  phihat <- poly(dt$K, dt$logI, degree = 3) %*% op1$coefficients[3:length(op1$coefficients)]
  
  # Second Stage
  
  omegahat <- function(alpha) {phihat - alpha * dt$logK}
  
  xiK <- function(alpha) lm(omegahat(alpha) ~ poly(dt$lagI, dt$lagK, dt$lagL, degree = 2))$residuals * dt$K
  
  # W = identity matrix 
  GMMcriterion <- function(alpha) {
    xiKv <- matrix(xiK(alpha), nrow = 1)
    sum((xiKv / sum(xiKv))^2)
  }
  
    
  alphahat <- optim(.4, fn = GMMcriterion, method = "BFGS", hessian = F)
  
  return(c(op1$coefficients[2], logK = alphahat$par))
}

dtt$company <- as.factor(dtt$company_name)

R <- 100 
opboot <- boot(data = dtt, statistic = op, R = R, parallel = "multicore", ncpus = 4) # takes a bit...

opboot

OP_results <- data.frame(PointEstimate = opboot$t0, 
                         StandardError = c(sd(opboot$t[, 1]), sd(opboot$t[, 2])), 
                         Quantile025 = c(quantile(opboot$t[, 1], probs = .025), quantile(opboot$t[, 2], probs = .025)), 
                         Quantile975 = c(quantile(opboot$t[, 1], probs = .975), quantile(opboot$t[, 2], probs = .975)))

OP_results

hist(opboot$t[, 2])
# the coefficient on capital, obtained in the second step of OP, is very noisily estimated.
# it looks like sometimes the optimiser just wanders off. 
# There may very well be a mistake in my code - if you spot it, please let me know. 
# additional instruments may also help; with more than one moment, 2step GMM may help (why does 2step GMM not change things with one moment?).
