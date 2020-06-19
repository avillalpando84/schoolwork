library(dplyr)
library(foreign)
library(plm)
library(DescTools)
library(ggplot2)
library(psych)
library(xtable)
library(corrgram)
library(ggfortify)
library(vars)
library(het.test)
library(lmtest)

setwd("~/Mis Documentos/CIDE DPP/2 Segundo semestre/4 Econometría/Trabajo final/Corrupción")
file.choose()
corrupT = read.spss("Base_Tesis_MPPC_SinPerdidos.sav", to.data.frame=TRUE)
corrupT <- dplyr::filter(corrupT, AÑO >="2002" & AÑO < "2012")

corrupT <- dplyr::select(corrupT, PAÍS, AÑO, IPC, PIB_PC_PPP_2011, DESEMPL, POBRE_1, GINI_1,
                         EDUINDEX_1, TRANS_1, LIBPRENSA_1, INDEP_JUDICIAL_1, CARGA_REG_1, 
                         CONF_POLIT_1, PROC_NEGOCIO_1, NUM_IMPUESTOS_1, ORG_AC, IND_ORG_AC)

corrupT <- dplyr::transmute(corrupT,
                            country = PAÍS,
                            year = AÑO,
                            CPI = IPC,
                            
                            transparency = TRANS_1,
                            pressfree = LIBPRENSA_1,
                            bredtape = PROC_NEGOCIO_1,
                            taxes = NUM_IMPUESTOS_1,
                            acagency = ORG_AC,
                            indep_acagency = IND_ORG_AC,
                            regburden = CARGA_REG_1,
                            judindep = INDEP_JUDICIAL_1,
                            
                            educ = EDUINDEX_1,
                            trustpols = CONF_POLIT_1,
                            
                            pc_gdp = PIB_PC_PPP_2011,
                            unemploy = DESEMPL,
                            poverty = POBRE_1,
                            gini = GINI_1
                            )


ggplot(data = corrupT, aes(x = year, y = CPI,)) + geom_line(color="red", size=2)+facet_wrap(~country) + labs(title = "TI's CPI for 18 Latin American Countries 2002-2011")

corrupINS <- dplyr::select(corrupT, transparency, pressfree, bredtape, taxes,
                           acagency, indep_acagency, regburden, judindep)
corrupSOC <- dplyr::select(corrupT, educ, trustpols)
corrupSTR <- dplyr::select(corrupT, pc_gdp, unemploy, poverty, gini)

corrupINS2 <- dplyr::select(corrupT, transparency, pressfree, bredtape, taxes, regburden, judindep)
corrupVAR <- dplyr::select(corrupT, transparency, educ, pc_gdp, unemploy, poverty, gini)

corrgram(corrupINS2, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations between six Group 1 variables")

pairs.panels(corrupINS2[,], method = "pearson", hist.col = "#00AFBB", density = TRUE,
             ellipses = TRUE, main="Pearson correlations between six Group 1 variables")

pairs.panels(corrupSOC[,], method = "pearson", hist.col = "#00AFBB", density = TRUE,
             ellipses = TRUE, main="Pearson correlations between Group 2 variables")

pairs.panels(corrupSTR[,], method = "pearson", hist.col = "#00AFBB", density = TRUE,
             ellipses = TRUE, main="Pearson correlations among Group 3 variables")

corrgram(corrupVAR, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations among assorted often problematic variables")

pairs.panels(corrupVAR[,], method = "pearson", hist.col = "#00AFBB", density = TRUE,
             ellipses = TRUE, main="Pearson correlations among assorted often problematic variables")

corruPanel <- pdata.frame(corrupT, index = c("country", "year"), drop.index = FALSE)

#Kitchen sink model a lo bestia
corrupKS_BEAST <- lm(data = corruPanel, CPI ~ transparency + pressfree + bredtape + taxes +
                     acagency + indep_acagency + regburden + judindep + educ + trustpols +
                     pc_gdp + unemploy + poverty + gini)

tabla1 <- print(xtable(corrupKS_BEAST), type="html", file="tabla1.html")

corrupKS_BEAST2 <- lm(data = corruPanel, CPI ~ transparency + bredtape + taxes +
                       acagency + indep_acagency + regburden + judindep + educ + trustpols +
                        unemploy + poverty + gini)

tabla2 <- print(xtable(corrupKS_BEAST2), type="html", file="tabla2.html")

#Institutional fijos, aleatorios, OLS
corrupINS_FIX <- plm(CPI ~ transparency + pressfree + bredtape + taxes +
                       acagency + indep_acagency + regburden + judindep, index = c("country", "year"), 
               data = corruPanel, model = "within")

corrupINS_RND <- plm(CPI ~ transparency + pressfree + bredtape + taxes +
                       acagency + indep_acagency + regburden + judindep, index = c("country", "year"), 
                     data = corruPanel, model = "random")

corrupINS_OLS <- plm(CPI ~ transparency + pressfree + bredtape + taxes +
                       acagency + indep_acagency + regburden + judindep, index = c("country", "year"), 
                     data = corruPanel, model = "pooling")

# EFECTOS ALEATORIOS VS AGRUPADOS
plmtest(corrupINS_OLS, type=c("bp"))

# EFECTOS AGRUPADOS VS FIJOS
pFtest (corrupINS_FIX, corrupINS_OLS)

# EFECTOS ALEATORIOS VS FIJOS
phtest (corrupINS_FIX, corrupINS_RND)

summary(corrupINS_RND)

#sociológicos-estructurales fijos, aleatorios, OLS
corrupSOCSTR_FIX <- plm(CPI ~ educ + trustpols + pc_gdp +
                          unemploy + poverty + gini, index = c("country", "year"), 
                        data = corruPanel, model = "within")

corrupSOCSTR_RND <- plm(CPI ~ educ + trustpols + pc_gdp +
                          unemploy + poverty + gini, index = c("country", "year"), 
                        data = corruPanel, model = "random")

corrupSOCSTR_OLS <- plm(CPI ~ educ + trustpols + pc_gdp +
                          unemploy + poverty + gini, index = c("country", "year"), 
                        data = corruPanel, model = "pooling")

# EFECTOS ALEATORIOS VS AGRUPADOS
plmtest(corrupSOCSTR_OLS, type=c("bp"))

# EFECTOS AGRUPADOS VS FIJOS
pFtest (corrupSOCSTR_FIX, corrupSOCSTR_OLS)

# EFECTOS ALEATORIOS VS FIJOS
phtest (corrupSOCSTR_FIX, corrupSOCSTR_RND)

summary(corrupSOCSTR_FIX)

corrupFULL_FIX <- plm(CPI ~ transparency + taxes + judindep + trustpols + 
                        unemploy + poverty, index = c("country", "year"), 
                      data = corruPanel, model = "within")

corrupFULL_RND <- plm(CPI ~ transparency + taxes + judindep + trustpols + 
                        unemploy + poverty, index = c("country", "year"), 
                      data = corruPanel, model = "random")

corrupFULL_RND_LN <- plm(CPI ~ log(transparency + taxes + judindep + trustpols + 
                        unemploy + poverty), index = c("country", "year"), 
                      data = corruPanel, model = "random")

corrupFULL_OLS <- plm(CPI ~ transparency + taxes + judindep + trustpols + 
                        unemploy + poverty, index = c("country", "year"), 
                      data = corruPanel, model = "pooling")

corrupFULL_LM <- lm(CPI ~ transparency + taxes + judindep + trustpols + 
                        unemploy + poverty, 
                      data = corruPanel)

corrupFULL_LM_LN <- lm(log(CPI) ~ transparency + taxes + judindep + trustpols + 
                      unemploy + poverty, 
                    data = corruPanel)

# EFECTOS ALEATORIOS VS AGRUPADOS
plmtest(corrupFULL_OLS, type=c("bp"))

# EFECTOS AGRUPADOS VS FIJOS
pFtest (corrupFULL_FIX, corrupFULL_OLS)

# EFECTOS ALEATORIOS VS FIJOS
phtest (corrupFULL_FIX, corrupFULL_RND)

summary(corrupFULL_RND)

# AUTOCORRELACIÓN SERIAL

pbgtest(corrupFULL_RND)
pdwtest (corrupFULL_RND_LN)

#HETEROSCEDASTICIDAD

pbgtest(corrupFULL_RND)

# TEST DE CAUSALIDAD DE GRANGER
grangertest(CPI ~ trustpols, order = 3, data = corruPanel)
grangertest(trustpols ~ CPI, order = 3, data = corruPanel)

# ESTADÍSTICA DESCRIPTIVA

DS <- describeBy(corruPanel, group = "country")

