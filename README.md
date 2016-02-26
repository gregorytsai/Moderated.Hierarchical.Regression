# Moderated.Hierarchical.Regression
 <br />
Use when you need to run LOTS of Moderations <br />
Reads Excel or CSV files <br />
Assign which variables to run , and the program will run every combinations you assigned and save the results. <br />
 <br />
There are currently 2 versions: <br />
1.File: 2WayWithControlVariable.R : Does 3 step moderated step-wise multiple regression <br />
    Step1: Dependent ~ Control variables  <br />
    Step2: Dependent ~ Control variables + 1 Predictor + 1 moderator <br />
    Step3: Dependent ~ Control variables + 1 Predictor + 1 moderator + predictor*moderator <br />
  Then save 3 steps regression results to 3 csv files or single excel file with 3 sheets. <br />
1.File: 2Way_ControlVar_SimpleSlope.R  : Does 3 step moderated step-wise multiple regression WITH simple slope analysis post-hoc <br />
    Step1: Dependent ~ Control variables  <br />
    Step2: Dependent ~ Control variables + 1 Predictor + 1 moderator <br />
    Step3: Dependent ~ Control variables + 1 Predictor + 1 moderator + predictor*moderator <br />
    Post-Hoc: Simple slope analysis(slope,se,t,p,4 points) <br />
  Then save 4 steps regression results to 4 csv files or single excel file with 4 sheets. <br />
