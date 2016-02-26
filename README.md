# Moderated.Hierarchical.Regression

Use when you need to run LOTS of Moderations
Reads Excel or CSV files
Assign which variables to run , and the program will run every combinations you assigned and save the results.

There are currently 2 versions:
1.File: 2WayWithControlVariable.R : Does 3 step moderated step-wise multiple regression
    Step1: Dependent ~ Control variables 
    Step2: Dependent ~ Control variables + 1 Predictor + 1 moderator
    Step3: Dependent ~ Control variables + 1 Predictor + 1 moderator + predictor*moderator
  Then save 3 steps regression results to 3 csv files or single excel file with 3 sheets.
1.File: 2Way_ControlVar_SimpleSlope.R  : Does 3 step moderated step-wise multiple regression WITH simple slope analysis post-hoc
    Step1: Dependent ~ Control variables 
    Step2: Dependent ~ Control variables + 1 Predictor + 1 moderator
    Step3: Dependent ~ Control variables + 1 Predictor + 1 moderator + predictor*moderator
    Post-Hoc: Simple slope analysis(slope,se,t,p,4 points)
  Then save 4 steps regression results to 4 csv files or single excel file with 4 sheets.
