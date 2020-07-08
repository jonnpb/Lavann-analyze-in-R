# Lavann-analyze-in-R
#Bibliotecas necessárias
library("lavaan")
library("semPlot")
######
#exemplo de como fazer
#Definindo o modelo, exemplo dado pelo pacote
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
              sex ~ visual+textual+speed'
#Gerando a análise confirmatória
fit <- cfa(HS.model, data = HolzingerSwineford1939)
#Plotando o modelo
semPaths(fit,intercepts=TRUE)
#Resultados da análise confirmatória
summary(fit, fit.measures = TRUE)
#Gerando a MEE
mee <- sem(HS.model, data = HolzingerSwineford1939)
#Resultados da MEE
summary(mee, standardized = TRUE ,fit.measures = TRUE)



############
#modelo firjan geral, apenas para plot, analise melhor isolada.
#masculino
modelFIRJAN_M ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
              T_Neop ~ cdv
              T_DEndoc ~ cdv
              T_DCirc ~ cdv
              T_DResp ~ cdv'
fit_M1 <- cfa(modelFIRJAN_M, data = Banco_MEE_M)#analise confirmatória
semPaths(fit1,intercepts = TRUE,rotation = 2) #plot
summary(fit_M1, fit.measures = TRUE)
meeFIRJAN1=sem(modelFIRJAN_M, data = Banco_MEE_M)#MEE
summary(meeFIRJAN1, standardized=TRUE,fit.measures=TRUE)

#Feminino
modelFIRJAN_F ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
              T_Neop ~ cdv
              T_DEndoc ~ cdv
              T_DCirc ~ cdv
              T_DResp ~ cdv'
fit_F1 <- cfa(modelFIRJAN_F, data = Banco_MEE_F)
semPaths(fit2, fit.measures = TRUE, rotation = 2)





############
#modelo firjan isolado NEOPLASIA
#masculino
modelFIRJAN_NeoM ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
                   T_Neop ~ cdv'
fit_NeoM <- cfa(modelFIRJAN_NeoM, data = Banco_MEE_M)
semPaths(meeFIRJAN_NeoM,intercepts = TRUE,rotation = 2)
summary(fit_NeoM, fit.measures = TRUE)
meeFIRJAN_NeoM=sem(modelFIRJAN_NeoM, data = Banco_MEE_M) 
summary(meeFIRJAN_NeoM, standardized=TRUE,fit.measures=TRUE,rsquare = TRUE)
fitMeasures(meeFIRJAN_NeoM, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino
modelFIRJAN_NeoF ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
                   T_Neop ~ cdv'
fit_NeoF <- cfa(modelFIRJAN_NeoF, data = Banco_MEE_F)
semPaths(fit_NeoF,intercepts = TRUE,rotation = 2,combineGroups = TRUE)
summary(fit_NeoF, fit.measures = TRUE)
meeFIRJAN_NeoF=sem(modelFIRJAN_NeoF, data = Banco_MEE_F) 
summary(meeFIRJAN_NeoF, standardized=TRUE,fit.measures=TRUE)
fitMeasures(meeFIRJAN_NeoF, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Não é possível utilizar o modelo firjan para Doenças endócrinas, no sexo masculino.
#O modelo não converge levando em contas os dados obtidos
#modelo firjan isolado Doenças endócrinas
#masculino
modelFIRJAN_DEndM ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DEndoc ~ cdv'
fit_DEndM <- cfa(modelFIRJAN_DEndM, data = Banco_MEE_M)
summary(fit_DEndM, fit.measures = TRUE)
meeFIRJAN_DEndM=sem(modelFIRJAN_DEndM, data = Banco_MEE_M) 
summary(meeFIRJAN_DEndM, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DEndM,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DEndM, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino
modelFIRJAN_DEndF ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DEndoc ~ cdv'
fit_DEndF <- cfa(modelFIRJAN_DEndF, data = Banco_MEE_F)
summary(fit_DEndF, fit.measures = TRUE)
meeFIRJAN_DEndF=sem(modelFIRJAN_DEndF, data = Banco_MEE_F) 
summary(meeFIRJAN_DEndF, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DEndF,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DEndF, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))

#modelo firjan isolado Doenças Circulatórias
#Masculino
#No caso da Doença Circulatória, o modelo produz variâncias negativas, para o sexo masculino.
#sendo assim, os dados da Firjan também não se adequam as Taxas de D.Circulatórias.
modelFIRJAN_DCircM ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DCirc ~ cdv'
fit_DCircM <- cfa(modelFIRJAN_DCircM, data = Banco_MEE_M)
summary(fit_DCircM, fit.measures = TRUE)
meeFIRJAN_DCircM=sem(modelFIRJAN_DCircM, data = Banco_MEE_M) 
summary(meeFIRJAN_DCircM, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DCircM,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DCircM, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino #adequação aceitável
modelFIRJAN_DCircF ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DCirc ~ cdv'
fit_DCircF <- cfa(modelFIRJAN_DCircF, data = Banco_MEE_F)
summary(fit_DCircF, fit.measures = TRUE)
meeFIRJAN_DCircF=sem(modelFIRJAN_DCircF, data = Banco_MEE_F) 
summary(meeFIRJAN_DCircF, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DCircF,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DCircF, c("chisq","df","cfi","pcfi", "gfi","tli", "pgfi", "rmsea", "aic", "bic","ecvi"))


#modelo firjan isolado Doenças Respiratórias
#Masculino
modelFIRJAN_DRespM ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DResp ~ cdv'
fit_DRespM <- cfa(modelFIRJAN_DRespM, data = Banco_MEE_M)
summary(fit_DRespM, fit.measures = TRUE)
meeFIRJAN_DRespM=sem(modelFIRJAN_DRespM, data = Banco_MEE_M) 
summary(meeFIRJAN_DRespM, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DRespM,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DRespM, c("chisq","df","cfi","pcfi", "gfi","tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino
modelFIRJAN_DRespF ='cdv  =~ IF_SAÚDE + IF_RENDA + IF_EDUC
T_DResp ~ cdv'
fit_DRespF <- cfa(modelFIRJAN_DRespF, data = Banco_MEE_F)
summary(fit_DRespF, fit.measures = TRUE)
meeFIRJAN_DRespF=sem(modelFIRJAN_DRespF, data = Banco_MEE_F) 
summary(meeFIRJAN_DRespF, standardized=TRUE,fit.measures=TRUE)
semPaths(meeFIRJAN_DRespF,intercepts = TRUE,rotation = 2)
fitMeasures(meeFIRJAN_DRespF, c("chisq","df","cfi","pcfi", "gfi","tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

############
#modelo dados DATASUS GERAL 
#Masculino, apenas para plot, analise separada
modelDATASUS_M = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
                  T_Neop ~ cdv
                  T_DEndoc ~ cvd
                  T_DCirc ~ cdv
                  T_DResp ~ cdv'

fit_GM = cfa(modelDATASUS_M, data = Banco_MEE_M)
semPaths(fit_GM,intercepts = TRUE,rotation = 2)

#Feminino, apenas para plot
modelDATASUS_F = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ + R_utero + Rz_mamografias
                  T_Neop ~ cdv
                  T_DEndoc ~ cdv
                  T_DCirc ~ cdv
                  T_DResp ~ cdv'
fit_GF = cfa(modelDATASUS_F, data = Banco_MEE_F)
semPaths(fit_GF,intercepts = TRUE,rotation = 2, title = TRUE)



############
#modelo dados DATASUS
#Masculino Neoplasia
modelDATASUS_NeoM = 'cdv =~  I_cond_s_at_b + Cob_ac_saúd_PBF  + Trab_SUS_públ
                     T_Neop ~ cdv'

fit_DM1 <- cfa(modelDATASUS_NeoM, data = Banco_MEE_M)
semPaths(fit_DM1, intercepts = TRUE, rotation = 2, title = TRUE)
meeDATASUS_NeoM = sem(modelDATASUS_NeoM, data = Banco_MEE_M)
summary(meeDATASUS_NeoM, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fitMeasures(meeDATASUS_NeoM, c("chisq","df","cfi","pcfi","tli", "gfi", "pgfi", "rmsea", "aic", "bic","ecvi"))


#Feminino Neoplasia
modelDATASUS_NeoF = 'cdv =~  I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit  + R_utero + Rz_mamografias
                     T_Neop ~ cdv'

fit_DF1 <- cfa(modelDATASUS_NeoF, data = Banco_MEE_F)
semPaths(fit_DF1, intercepts = TRUE, rotation = 2)
meeDATASUS_NeoF = sem(modelDATASUS_NeoF, data = Banco_MEE_F)
summary(meeDATASUS_NeoF, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_NeoF, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))


###
#Masculino D. Endocrinas#1,5,7,10
modelDATASUS_DEndM = 'cdv =~ I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit
T_DEndoc ~ cdv'

fit_DM2 <- cfa(modelDATASUS_DEndM, data = Banco_MEE_M)
semPaths(fit_DM2, intercepts = TRUE, rotation = 2)
meeDATASUS_DEndM = sem(modelDATASUS_DEndM, data = Banco_MEE_M)
summary(meeDATASUS_DEndM, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DEndM, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino D.Endócrinas
modelDATASUS_DEndF = 'cdv =~ Cob_eq_at_b +I_cond_s_at_b+ Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
T_DEndoc ~ cdv'

fit_DF2 <- cfa(modelDATASUS_DEndF, data = Banco_MEE_F)
semPaths(fit_DF2, intercepts = TRUE, rotation = 2)
meeDATASUS_DEndF = sem(modelDATASUS_DEndF, data = Banco_MEE_F)
summary(meeDATASUS_DEndF, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DEndF, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

###
#Masculino D.Circulatórias
modelDATASUS_DCircM = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
T_DCirc ~ cdv'

fit_DM3 <- cfa(modelDATASUS_DCircM, data = Banco_MEE_M)
semPaths(fit_DM3, intercepts = TRUE, rotation = 2)
meeDATASUS_DCircM = sem(modelDATASUS_DCircM, data = Banco_MEE_M)
summary(meeDATASUS_DCircM, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DCircM, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino D.Circulatórias
modelDATASUS_DCircF = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
T_DCirc ~ cdv'

fit_DF3 <- cfa(modelDATASUS_DCircF, data = Banco_MEE_F)
semPaths(fit_DF3, intercepts = TRUE, rotation = 2)
meeDATASUS_DCircF = sem(modelDATASUS_DCircF, data = Banco_MEE_F)
summary(meeDATASUS_DCircF, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DCircF, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

###
#Masculino D. Respiratórias
modelDATASUS_DRespM = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
T_DResp ~ cdv'

fit_DM4 <- cfa(modelDATASUS_DRespM, data = Banco_MEE_M)
semPaths(fit_DM4, intercepts = TRUE, rotation = 2)
meeDATASUS_DRespM = sem(modelDATASUS_DRespM, data = Banco_MEE_M)
summary(meeDATASUS_DRespM, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DRespM, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))

#Feminino D. Respiratórias
modelDATASUS_DRespF = 'cdv =~ Cob_eq_at_b + I_cond_s_at_b + Cob_ac_saúd_PBF + A_vig.sanit + Trab_SUS_públ
T_DResp ~ cdv'

fit_DF4 <- cfa(modelDATASUS_DRespF, data = Banco_MEE_F)
semPaths(fit_DF4, intercepts = TRUE, rotation = 2)
meeDATASUS_DRespF = sem(modelDATASUS_DRespF, data = Banco_MEE_F)
summary(meeDATASUS_DRespF, standardized = TRUE, fit.measures = TRUE)
fitMeasures(meeDATASUS_DRespF, c("chisq","df","cfi","pcfi", "gfi" ,"tli", "pgfi", "rmsea", "aic", "bic","ecvi"))
