library(dplyr)
library(ggplot2)
library(car)
library(urca)
#Exercise 10.1 data from wooldrigde_data/INTDEF
load("C:/Users/ander/OneDrive/economia_5/econometria/datos/Wooldrigde_data/R data sets for 5e/barium.RData")
intDef <- tbl_df(data)
#Add column if years > 1979 = 1 else = 0
intDef <- intDef %>% 
  mutate(y79 = if_else (year<=1979,0,1))

#Model
model_c101 <- lm(i3 ~ inf + def, data = intDef)
summary(model_c101)

#Model with y79
model_79 <- lm(i3 ~ inf + def + y79, data = intDef)
summary(model_79)

#Exercise 10.2 data from wooldrigde_data/BARIUM
load("C:/Users/ander/OneDrive/economia_5/econometria/datos/Wooldrigde_data/R data sets for 5e/barium.RData")
# i) Model 10.22 with linear trend 
barium <- tbl_df(data)
model1022T <- lm(lchnimp ~ lchempi + lgas 
                + lrtwex + befile6 + affile6
                + afdec6 + t, data = barium)
summary(model1022)
#ii) linear Hypothesis for 
linearHypothesis(model1022T, c('lchempi','lgas','lrtwex','afdec6'))
#iii) add monthly binary variables
model1022T <- lm(lchnimp ~ lchempi + lgas 
                 + lrtwex + befile6 + affile6
                 + afdec6 + t + feb + mar + apr 
                 + may + jun + jul + aug + sep 
                 + oct + nov + dec, data = barium)
# if all months are equal to 0 there is no seasonality
linearHypothesis(model1022T, c('feb','mar','apr','may','jun','jul','aug','sep'
                               ,'oct','nov','dec'))

#Exercise 10.3 data from wooldridge_data/PRMINWGE
load('ruta/PRMINWGE')
prminwge <- tbl_df(data)
model1038 <- lm(lprepop ~ lmincov + lusgnp + t,data = prminwge )
summary(model1038)
#Add to model 10.38 log(prgnp)
model1038 <- update(model1038, . ~ . + lprgnp)
summary(model1038)

#Exercise 10.4 data from wooldridge_data/FERTIL3
load('ruta/fertil3')
fertil <- tbl_df(data)
# Model 10.19 Effects of personal exemption on fertility
model1019 <- lm(gfr ~ pe + pe_1 + pe_2 +ww2 + pill, data = fertil)
summary(model1019)
# Aux model to estimate standar error of Long-Run Propensity
modelAux <- lm(gfr ~ pe + I(pe_1-pe) + I(pe_2-pe) +ww2 + pill, data = fertil)
summary(modelAux)

#Exercise 10.6 data from wooldridge_data/FERTIL3
load('C:/Users/ander/OneDrive/economia_5/econometria/datos/Wooldrigde_data/R data sets for 5e/fertil3.Rdata')
fertil <- tibble::as_tibble(data)
#dikey fuller
gfr1 -> ur.df(fertil$gfr, lags = 4, selectlags = "BIC", type = "trend")

gfr2 -> ur.df(fertil$gfr, lags = 4, selectlags = "BIC", type = "drift")
gfr3 -> ur.df(fertil$gfr, lags = 4, selectlags = "BIC", type = "none")
#i model grf = t + t^2 + u
regGf <- lm(gfr ~ t + I(t^2), data = fertil)
#residuals
resid <- resid(regGf)
# ii  
model1035 <- lm(resid ~ pe + ww2 + pill + t + I(t^2), data = fertil)
summary(model1035)
# iii model 10.35 adding t^3
model1035 <- lm(gfr ~ pe + ww2 + pill + t + I(t^2) + I(t^3), data = fertil)
summary(model1035)

#Exercise 10.7 data from wooldridge_data/CONSUMP
load("C:/Users/ander/OneDrive/economia_5/econometria/datos/Wooldrigde_data/R data sets for 5e/consump.Rdata")
consump <- tibble::as_tibble(data)
# i) log c = log y
model107 <- lm(lc ~ ly, data = consump)
summary(model107)
# ii) lag ly_1
consump <- consump %>% 
  mutate(ly_1 = lag(consump$ly, 1))
# Model with ly_y
model107 <- update(model107, .~. + ly_1)
summary(model107)
# iii) model with r3
model107 <- update(model107, .~. + r3)
summary(model107)

#Exercise 10.8 data from wooldridge_data/FERTIL3
load('C:/Users/ander/OneDrive/economia_5/econometria/datos/Wooldrigde_data/R data sets for 5e/fertil3.Rdata')
fertil <- tibble::as_tibble(data)
# Model 10.19 Effects of personal exemption on fertility 
model1019 <- lm(gfr ~ pe + pe_1 + pe_2 + pe_3 + pe_4 +ww2 + pill, data = fertil)
summary(model1019)
linearHypothesis(model1019,c('pe_3','pe_4'))
#Long-Run Propensity = pe_1+pe_2+pe_3+pe_4
lrp <- sum(model1019$coefficients[3:6])
# Aux model to estimate standar error of Long-Run Propensity
modelAux <- lm(gfr ~ pe + I(pe_1-pe) + I(pe_2-pe) + I(pe_3-pe) 
               + I(pe_4-pe) +ww2 + pill, data = fertil)
summary(modelAux)
#iii) add columns z0, z1 & z2
fertil <- mutate(fertil, 
                 z0 = (pe + pe_1 + pe_2 + pe_3 + pe_4),
                 z1 = (pe_1 + 2*pe_2 +3*pe_3 + 4*pe_4),
                 z2 = (pe_1 + 4*pe_2 +9*pe_3 + 16*pe_4))
modelAux <- lm(gfr ~ z0 + z1 + z2 + ww2 + pill, data = fertil)
summary(modelAux)
s0 <- modelAux$coefficients['z0']
s1 <- sum(modelAux$coefficients[2:4])
s2 <- s0+2*modelAux$coefficients['z1']+4*modelAux$coefficients['z2']
s3 <- s0+3*modelAux$coefficients['z1']+9*modelAux$coefficients['z2']
lrp <- (s0 + s1 + s2 + s3)
lrp
#Exercise 10.9 data from wooldridge_data/VOLAT
load('ruta/volat.RData')
volat <- tibble::as_tibble(data)
#ii)
model109 <- lm(rsp500 ~ pcip + i3, data = volat)
summary(model109)
#iii) 
qplot(date,rsp500,geom = 'line', data = volat)
acf(volat$rsp500, na.action = na.omit)
#Exercise 10.10 data from wooldridge_data/INTDEF
load('ruta/intdef.RData')
intdef <- tibble::as_tibble(data)
# i) Correlation between inf ~ def 
model <- lm(inf ~ def, data = intdef)
r <- summary(model)$r.squared
model$call$formula
graph <- qplot(def, inf, data = intdef, geom = 'point')+
        geom_smooth( method = 'lm' )
graph+annotate("text", x = 5, y = 10, 
               label = paste("italic(R) ^ 2 ==",r),parse = TRUE)
# ii) 
model1015 <- lm(i3 ~ inf + inf_1 + def + def_1, data = intdef)
summary(model1015)
#iii) 
lrp <- sum(model1015$coefficients[2:3])
lrp
