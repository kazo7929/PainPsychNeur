#See the enclosed README file for an explanation of the models below.

require(GenomicSEM)
require(corrplot)

load("./LDSCoutput_PainPsychJune2023.RData")
load("./LDSCoutput_PainPsychNeurDA_W_June2023.RData")
load("./LDSCoutput_PainPsychNeurJune2023.RData")

#######################################
#Correlations:
#######################################
rownames(LDSCoutput_PainPsychNeur_DA_W_June2023$S) <- colnames(LDSCoutput_PainPsychNeur_DA_W_June2023$S)
corrmat<-cov2cor(LDSCoutput_PainPsychNeur_DA_W_June2023$S)
corrmat_F1load <- as.data.frame(corrmat[c(19,9,24,8,2,12,17,23,14,15,1,21,5,22,4,20,11,3,16,18,6,10,13,7),c(30,27,29,28,32,25,26,33,34,31,35,36,37,38)])
corrplot(as.matrix(corrmat_F1load), num)


#######################################
#Models:
#######################################

#a.
pain_psych_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD

#residual covariances
CIGS ~~ chDs + chPh
stmP ~~ SCZ + AN
IBS ~ THOUGHT2

'

pain_psych_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#b.
pain_psych_noresidcov_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD
'

pain_psych_noresidcov_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_noresidcov_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#c.
pain_psych_3fPsych_model <- 
'
#pain part

F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part

EXT =~ ADHD + CIGS + CUD + ALC 
THOUGHT =~ SCZ + BP + OCD + AN
INT =~ MDD + ANX + PTSD
'

pain_psych_3fPsych_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_3fPsych_model, estimation="DWLS", std.lv=T, imp_cov = T)


###################################################################################
###################################################################################

#d.
pain_psych_2fPsych_model <- 
'
#pain part

F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part

EXT =~ ADHD + CIGS + CUD + ALC 
INT =~ MDD + ANX + PTSD + SCZ + BP + OCD + AN
'

pain_psych_2fPsych_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_2fPsych_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#e.
pain_psych_Qtrait_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD

#Q-test part:
F1 ~~ 0*EXT + 0*INT + 0*THOUGHT1 +0*THOUGHT2
ADHD + CIGS + CUD + ALC + SCZ + BP + OCD + AN + MDD + PTSD + ANX ~ F1
'

pain_psych_Qtrait_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_Qtrait_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#f.
pain_psych_EXTINTequal_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD


F1 ~~ b*EXT
F1 ~~ b*INT
'

pain_psych_EXTINTequal_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_EXTINTequal_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#g.
pain_psych_EXTINTTH12equal_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD

F1 ~~ b*INT +b*EXT + b*THOUGHT1 +b*THOUGHT2
'

pain_psych_EXTINTTH12equal_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=pain_psych_EXTINTTH12equal_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#h.
EXT_model <- 
 '
 #pain part
 F1 =~ back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
 F2 =~ arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt
 
 chDs ~~ chPh
 hipA ~~ hipP
 kneA ~~ kneP
 mgrn ~~ hdch
 
 arth ~~ a*arth
 a > 0.001

 F1 ~~ 0*F2
 
 #psych part
 EXT=~ADHD + CIGS + CUD + ALC  
 THOUGHT1=~SCZ + BP
 THOUGHT2=~OCD + AN 
 INT=~MDD + ANX + PTSD
 
 #cholesky
 X1 =~ 1*INT + EXT + THOUGHT1 + THOUGHT2
 X2 =~ 1*EXT + THOUGHT1 + THOUGHT2
 X3 =~ 1*THOUGHT1 + THOUGHT2
 X4 =~ 1*THOUGHT2 

 INT ~~ 0*INT
 EXT ~~ 0*EXT
 THOUGHT1 ~~ 0*THOUGHT1
 THOUGHT2 ~~ 0*THOUGHT2

 
 X1 + X2 + X3 + X4 ~~ F1 + F2

 X1 ~~ 0*X2 + 0*X3 + 0*X4
 X2 ~~ 0*X3 + 0*X4
 X3 ~~ 0*X4
 '
 
EXT_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=EXT_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#i.
INT_model <- 
 '
 #pain part
 
 F1 =~ back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
 F2 =~ arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt
 
 chDs ~~ chPh
 hipA ~~ hipP
 kneA ~~ kneP
 mgrn ~~ hdch
 
 arth ~~ a*arth
 a > 0.001
 
 #psych part
  EXT=~ADHD + CIGS + CUD + ALC  
 THOUGHT1=~SCZ + BP
 THOUGHT2=~OCD + AN 
 INT=~MDD + ANX + PTSD
 
 #cholesky
 X1 =~ 1*EXT + INT + THOUGHT1 + THOUGHT2
 X2 =~ 1*INT + THOUGHT1 + THOUGHT2
 X3 =~ 1*THOUGHT1 + THOUGHT2
 X4 =~ 1*THOUGHT2 

 INT ~~ 0*INT
 EXT ~~ 0*EXT
 THOUGHT1 ~~ 0*THOUGHT1
 THOUGHT2 ~~ 0*THOUGHT2
 F1 ~~ 0*F2

 
 X1 + X2 + X3 + X4 ~~ F1 + F2

 X1 ~~ 0*X2 + 0*X3 + 0*X4
 X2 ~~ 0*X3 + 0*X4
 X3 ~~ 0*X4
 '
 
INT_fit <- usermodel(covstruc=LDSCoutput_PainPsychJune2023, model=INT_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#j.
pain_psych_neurDA_W_model <- 
'
#pain part
F1 =~ NA*back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ NA*arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part
EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD

#Neuroticism:
# pain with psych correlations
    EXT ~~ F1 + F2 
    INT ~~ F1 + F2
    THOUGHT1 ~~ F1 + F2
    THOUGHT2 ~~ F1 + F2

# NEUR with pain and psych correlations
    F1 ~~ NEUR_full + NEUR_DA + NEUR_W
    F2 ~~ NEUR_full + NEUR_DA + NEUR_W
    EXT ~~ NEUR_full + NEUR_DA + NEUR_W
    INT ~~ NEUR_full + NEUR_DA + NEUR_W
    THOUGHT1 ~~ NEUR_full + NEUR_DA + NEUR_W
    THOUGHT2 ~~ NEUR_full + NEUR_DA + NEUR_W

NEUR_full ~~ NEUR_DA + NEUR_W
NEUR_DA ~~ NEUR_W
'

pain_psych_neurDA_W_fit <- usermodel(covstruc=LDSCoutput_PainPsychNeur_DA_W_June2023, model=pain_psych_neurDA_W_model, estimation="DWLS", std.lv=T, imp_cov = T)

###################################################################################
###################################################################################

#k.
pain_psych_neur_model <- 
'
#pain part

F1 =~ back + chDs + chPh + cyst + gast + genP + gout + hipP + IBS + neck + oesp + otRA + stmP + arth + crpl + enLL + hipA + kneP + kneA + legP + enth + pnjt + hdch + mgrn
F2 =~ arth + crpl + enLL + hipP + hipA + kneP + kneA + legP + enth + otRA + pnjt

chDs ~~ chPh
hipA ~~ hipP
kneA ~~ kneP
mgrn ~~ hdch

arth ~~ a*arth
a > 0.001

F1 ~~ 0*F2

#psych part

EXT=~ADHD + CIGS + CUD + ALC  
THOUGHT1=~SCZ + BP
THOUGHT2=~OCD + AN 
INT=~MDD + ANX + PTSD

#Neuroticism:
NEUR =~ NeurNagel2018
NeurNagel2018 ~~0*NeurNagel2018

# pain psych correlations
    EXT ~~ c1*F1 + c2*F2 
    INT ~~ c3*F1 + c4*F2
    THOUGHT1 ~~ c5*F1 + c6*F2
    THOUGHT2 ~~ c7*F1 + c8*F2

# NEUR causing pain and psych
    F1 ~ a1*NEUR 
    F2 ~ a2*NEUR 
    EXT ~ b1*NEUR 
    INT ~ b2*NEUR 
    THOUGHT1 ~ b3*NEUR 
    THOUGHT2 ~ b4*NEUR 

# indirect effects (a*b)
             a1b1 := a1*b1
             a1b2 := a1*b2
             a1b3 := a1*b3
             a1b4 := a1*b4
             a2b1 := a2*b1   
             a2b2 := a2*b2   
             a2b3 := a2*b3   
             a2b4 := a2*b4   
# total effect
             total1 := c1 + a1b1
             total2 := c2 + a2b1
             total3 := c3 + a1b2
             total4 := c4 + a2b2 
             total5 := c5 + a1b3 
             total6 := c6 + a2b3 
             total7 := c7 + a1b4 
             total8 := c8 + a2b4 

#proportion explained by NEUR (look at the unstandardized estimate to get the correct proportion)
prop1 := a1b1/total1 
prop2 := a2b1/total2  
prop3 := a1b2/total3 
prop4 := a2b2/total4 
prop5 := a1b3/total5 
prop6 := a2b3/total6 
prop7 := a1b4/total7 
prop8 := a2b4/total8 
' 

pain_psych_neur_fit <- usermodel(covstruc=LDSCoutput_PainPsychNeurJune2023, model=pain_psych_neur_model, estimation="DWLS", std.lv=T, imp_cov = T)

#save(pain_psych_fit, pain_psych_noresidcov_fit, pain_psych_3fPsych_fit, pain_psych_2fPsych_fit, pain_psych_Qtrait_fit, pain_psych_EXTINTequal_fit, pain_psych_EXTINTTH12equal_fit, EXT_fit, INT_fit, pain_psych_neurDA_W_fit, pain_psych_neur_fit, file="model_output.RData")

