# recurrence risk shiny app
https://recurrencerisk.shinyapps.io/recurrence/ 

Help Documentation

This web app has been developed to estimate the risk of progressing to distant breast cancer recurrence using disease-specific survival typically provided by cancer registries. 

SEER*Stat dic File: the dictionary exported from SEER*Stat software which contains the information describing the layout of the export data file. The user will need to view “all files” instead of “all supported types” to select this file in uploading popup window. Please see the below example for more details.

SEER*Stat data File: the cause-specific survival data generated from SEER*Stat.

CanSurv CSV File: the CSV format output from CanSurv software with parameters for the mixture cure survival models.

Stage Variable: the stage variable defined in SEER*Stat data. All the variable names including “stage” will be listed after uploading the SEER*Stat files. If there are more than 1 stage variable, the user will need to select the one which contains the distant stage or input the stage variable name manually. 

Distant Stage Value: the user will need to select the numeric value of distant stage from the listed values of Stage Variable or enter the value manually.

Adjustment Factor r: the factor which can be used to adjust the registry-based survival curves for sensitivity analysis. The user may click the up and down arrows to change the value or type in any value. The default value is 1.  

Number of Follow-up Years: the range of follow-up years in the output. The default number is 25. If the maximum number of follow-up years (max.num.year) in the SEER*Stat data is less than 25, then the default number will be set to the max.num.year. 
