# recurrence risk shiny app
https://recurrencerisk.shinyapps.io/recurrence/ 

Help Documentation

Input

This web application has been developed to estimate the risk of progressing to distant recurrence using disease-specific survival typically provided by cancer registries. The disease-specific survival is assessed via cause-specific survival using SEER\*Stat software. The cause-specific survival is assumed to follow a mixture-cure model and the risk of recurrence is inferred from the survival among the non-cured fraction. The cure fraction and parametric survival distribution among those not cured are estimated using CanSurv software. The current version can handle Weibull and log-logistic distributions for the non-cured survival.

SEER\*Stat dic File: the dictionary exported from SEER\*Stat software in .dic format which contains the information describing the layout of the export data file. 

SEER\*Stat data File: the cause-specific survival data generated from SEER\*Stat in .txt format.

CanSurv CSV File: the CSV format output from CanSurv software including parameters for the mixture cure survival models.

Stage Variable: the stage variable defined in SEER\*Stat data. All the variable names including “stage” will be listed after uploading the SEER\*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage or input the stage variable name manually. 

Distant Stage Value: the user will need to select the numeric value of distant stage from the listed values of Stage Variable or enter the value manually.

Adjustment Factor r: the factor which can be used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.  

Number of Follow-up Years: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER\*Stat data is less than 25, then the default number will be set to the max.num.year. 

Output

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

