This is the README file for a paper under review,

"A demonstration of incorporating risk-risk tradeoff outputs into reverse quantitative microbial risk asessments,"
by Amanda M. Wilson, Irene Mussio, Marc P. Verhougstraete, Yoonhee Jung, Ahamed Ashraf, Susan Chilton,
and Kerry Hamilton.

source code.R runs all code needed for confirming quantitative values given in the paper and 
generated figures in the paper. This file sources other files, such as
qmra_model.R, which is the code for our risk functions. newcastle_survey_data.R uses Data_raw13013.xlsx
to fit distributions to indifference points and then simulate data sets of 10,000 indifference points. 
threshold_calc.R then calculates risk thresholds based on generated indifference points.