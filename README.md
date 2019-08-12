# recurrence risk shiny app
https://recurrencerisk.shinyapps.io/recurrence/ 

Help Documentation

1. Group Data

This web application has been developed to estimate the risk of progressing to distant recurrence using disease-specific survival typically provided by cancer registries. The disease-specific survival is assessed via cause-specific survival or relative survival using SEER\*Stat software. The cause-specific survival or relative survival is assumed to follow a mixture-cure model and the risk of recurrence is inferred from the survival among the non-cured fraction. The cure fraction and parametric survival distribution among those not cured are estimated using CanSurv software. The current version can handle Weibull and log-logistic distributions for the non-cured survival.


Input

•	SEER\*Stat Dic File: the dictionary file exported from SEER\*Stat software with .dic extension which contains the information describing the layout of the export data file. 

•	SEER\*Stat Data File: the cause-specific survival or relative survival data generated from SEER\*Stat in .txt format.

•	CanSurv CSV File: the CSV format output from CanSurv software including information on strata/covariates and estimated parameters for the mixture cure survival models.

•	Stage Variable: the stage variable defined in SEER\*Stat data. All the variable names in the data set will be listed after uploading the SEER\*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage.

•	Distant Stage Value: the user will need to select the numeric value of distant stage from the listed values of Stage Variable.

•	Adjustment Factor r: the factor used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.  

•	Years of Follow-up: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER\*Stat data is less than 25, then the default number will be set to the max.num.year. 

Output

•	link: the parametric survival distribution among those not cured specified in CanSurv. 

•	cure: the cure fraction estimated from the mixture cure survival model.

•	lambda/k: the estimated parameters of the survival distribution for those not cured.

•	theta: the exponential hazard of the time from recurrence to cancer death.

•	surv_curemodel: the survival estimated from the mixture cure survival model.

•	surv_notcure: the estimated survival for the non-cured fraction.

•	median_surv_notcured: the median survival time for the non-cured fraction.

•	s1_numerical: the numerical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.

•	G_numerical: the numerical estimated survival to recurrence.

•	CI_numerical: 1-G_numerical, the numerical estimated cumulative incidence of recurrence which is the probability of progressing to cancer recurrence.

•	s1_analytical: the analytical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.

•	G_analytical: the analytical estimated survival to recurrence.

•	CI_analytical: 1-G_analytical, the analytical estimated cumulative incidence of recurrence.

•	se_CI_analytical: the standard error of CI_analytical.

•	obs_surv: the observed survival from SEER\*Stat.

•	obs_dist_surv: the observed survival for distant stage from SEER\*Stat.


2. Individual Data

This web application has been extended to estimate the risk of progressing to distant recurrence using individual survival data. The cause-specific survival or relative survival is assumed to follow a mixture-cure model and the risk of recurrence is inferred from the survival among the non-cured fraction. The cure fraction and parametric survival distribution among those not cured will be estimated using R flexsurvcure package. The current version can handle Weibull and log-logistic distributions for the non-cured survival.

Input

• the case-listing unformatted CSV data file exported from SEER\*Stat software with modification of variables of interest or user-generated individual data by other statistical software in the CSV format. Note that, users may not directly use the original unformatted variables, but need to modify the case-listing SEER\*Stat data before uploading because 1) it may not contain the event variable for death due to cancer and the dead events due to other causes should be recoded as censoring events; 2) there may not be categorical but continuous variables for strata and covariates; 3) the stage/strata/covariates variables are categorical but with many levels or with numerical unknown values, the user will need to recode it to better categorize the group.

• Strata: the user-defined strata variables which should be categorical coded as integers. All variable names in the data set will be listed for single/multiple selection.

• Covariates: the covariates defined in the mixture cure survival model which should be categorical coded as integers.
    
• Time Variable: the variable specified as follow-up time in the survival model.
    
• Event Variable: the status indicator, 0=alive, 1=dead (due to cancer). Note that, dead events due to other causes should be defined as censoring events. the status indicator, 0=alive, 1=dead (due to cancer). Note that, dead events due to other causes should be defined as censoring events. For example, when an unformatted case-listing SEER\*Stat data is used, a patient is dead due to cancer if Vitalstatusrecodestudycutoffus=dead and End_Calc_Vital_Status_Adjusted=dead; and a patient is dead due to other causes if Vitalstatusrecodestudycutoffus=dead and End_Calc_Vital_Status_Adjusted=(untraced or alive).
    
• Distribution: the latency distribution for the cure model (non-cured survival). The current version can handle Weibull and log-logistic distributions.
    
• Stage Variable: the stage variable defined in SEER\*Stat data or recoded by the user, which should be the same variable selected in Strata if it is defined as a stratum.All variable names in the data set will be listed after uploading the SEER\*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage.
    
• Distant Stage Value: the user will need to select the numeric value of distant stage from the listed values of Stage Variable.
    
• Adjustment Factor r: the factor used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.
    
• Years of Follow-up: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER\*Stat data is less than 25, then the default number will be updated to the max.num.year. 

Output

•	link: the parametric survival distribution among those not cured specified in CanSurv. 

•	cure: the cure fraction estimated from the mixture cure survival model.

•	lambda/k: the estimated parameters of the survival distribution for those not cured.

•	theta: the exponential hazard of the time from recurrence to cancer death.

•	surv_curemodel: the survival estimated from the mixture cure survival model.

•	surv_notcure: the estimated survival for the non-cured fraction.

•	median_surv_notcured: the median survival time for the non-cured fraction.

•	s1_numerical: the numerical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.

•	G_numerical: the numerical estimated survival to recurrence.

•	CI_numerical: 1-G_numerical, the numerical estimated cumulative incidence of recurrence which is the probability of progressing to cancer recurrence.

•	s1_analytical: the analytical estimated survival to recurrence (recurrence-free survival) for the non-cured fraction.

•	G_analytical: the analytical estimated survival to recurrence.

•	CI_analytical: 1-G_analytical, the analytical estimated cumulative incidence of recurrence.

•	se_CI_analytical: the standard error of CI_analytical.

•	obs_surv: the observed survival from SEER\*Stat.

•	obs_dist_surv: the observed survival for distant stage from SEER\*Stat.


References
1. Mariotto AB, Zou Z, Zhang F, Howlader N, Kurian AW, Etzioni R. Can We Use Survival Data from Cancer Registries to Learn about Disease Recurrence? The Case of Breast Cancer. Cancer Epidemiol Biomarkers Prev. 2018 Nov; 27(11):1332-1341. 
2. Yu B, Tiwari RC, Cronin KA, McDonald C, Feuer EJ. CANSURV: A Windows program for population-based cancer survival analysis. Comput.Methods Programs Biomed. 2005;80(3):195-203.
3. Yu B, Tiwari RC, Cronin KA, Feuer EJ. Cure fraction estimation from the mixture cure models for grouped survival data. Stat.Med. 2004;23(11):1733-47.
4. De Angelis R, Capocaccia R, Hakulinen T, Soderman B, Verdecchia A. Mixture models for cancer survival analysis: application to population-based data with covariates. Stat.Med. 1999;18(4):441-54.
5. Capocaccia R. Relationships between incidence and mortality in non-reversible diseases. Stat Med. 1993;12(24):2395-415.
6. Li CS, Taylor JMG, Sy JP. Identifiability of cure models. Statistics & Probability Letters. 2001;54(4):389-95.
7. Howlader N, Ries LA, Mariotto AB, Reichman ME, Ruhl J, Cronin KA. Improved estimates of cancer-specific survival rates from population-based data. J Natl Cancer Inst. 2010;102(20):1584-98.


