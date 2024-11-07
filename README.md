######
Files:
######
1. LDSCoutput_PainPsychJune2023.RData
1a. pain_psych_corr_fit.RData
1b. pain_psych_corr_fit_Qtrait.RData
1c. pain_psych_corr_fit_EXTINTequal.RData
1d. model_EXT_fit.RData
1e. model_INT_fit.RData

2. LDSCoutput_PainPsychNeurJune2023.RData
2a. pain_psych_neur_model_fit.RData

3. LDSCoutput_PainPsychNeurDA_W_June2023.RData
3a. pain_psych_neurDA_W_model_fit.RData

########
Scripts:
########
Script_models.R

- Figure 2A, genetic correlations between pain conditions, psychopathologies, and neuroticism scales
- GenomicSEM R code for (figure and table numbers provided where applicable): 
a. the best-fit pain-psychopathology model (pain_psych_model), which includes residual covariances:
		CIGS ~~ chDs + chPh
		stmP ~~ SCZ + AN
		IBS ~ THOUGHT2
b. Fig. 2B, the main pain-psychopathology model, without residual covariances (pain_psych_noresidcov_model)
c. SFig. S3A, the pain-psychopathology model with 3 psychopathology factors (pain_psych_corr_fact_3fPsych_model)
d. SFig. S3B, the pain-psychopathology model with 2 psychopathology factors (pain_psych_corr_fact_2fPsych_model)
e. SFig S4 and STab. S2, the Q-trait heterogeneity test of psychopathology conditions as Q traits (pain_psych_Qtrait_model)
f. the pain-psychopathology model with an equality constraint for EXT and INT factor correlations with F1 (pain_psych_corr_fact_EXTINTequal_model)
g. the pain-psychopathology model with an equality constraint for all four psychopathology factor correlations with F1 (pain_psych_corr_fact_EXTINTTH12equal_model)
h. Fig. 3A, Cholesky decomposition of the pain-psychopathology factor correlations, testing for the correlations between F1 and EXT independent of INT (EXT_model)
i. Fig. 3B, Cholesky decomposition of the pain-psychopathology factor correlations, testing for the correlations between F1 and INT independent of EXT (INT_model)
j. Fig. 4A, the model with estimates of correlations for neuroticism (3 subscales) with pain and psychiatric factors (pain_psych_neurDA_W_model).
k. Fig. 4B, the model with estimates of the proportion of pain-psychiatric factor correlations explained by neuroticism (pain_psych_neur_model).

- Abbreviations:
F1, General Pain factor
F2, Musculoskeletal Pain factor
EXT, Externalizing factor
INT, Internalizing factor
THOUGHT1, Psychotic Disorder factor
THOUGHT2, Compulsive Disorder factor
NEUR, neuroticism full scale)
NEUR_DA, neuroticism depressive affect subscale
NEUR_W, neuroticism worrying subscale	
