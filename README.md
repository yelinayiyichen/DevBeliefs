README

#file/folder system and functions#

data to fit computational models can be found under: 
project data + code >  computational models > data, where
d_revMod_DesiOpp_Alpha.csv is data from the Desired Opposite condition
d_revMod_ObsPrefRvd_Alpha.csv and d_revMode_UnobsPrefRvd_Alpha.csv are data from the Preference Revealed condition. See Methods - Conditions - Preference Revealed section for more details
d_revMod_Opp_Alpha.csv is data from the Opposite condition
M_P1_Baseline_20210324.csv is data from the Baseline condition
M_P1_FP_20210324.csv is data from the Friend Predicted condition

code for computational modeling (using Matlab 2018b) can be found under:
project data + code >  computational models, where
orig_fmincon_driver.m, which calls orig_fmincon_model.m, is used to fit data from:
M_P1_Baseline_20210324.csv and  M_P1_FP_20210324.csv
revMod_driver.m, which calls revMod_model.m, is used to fit data from:
d_revMod_DesiOpp_Alpha.csv, d_revMod_ObsPrefRvd_Alpha.csv, d_revMode_UnobsPrefRvd_Alpha.csv,
d_revMod_Opp_Alpha.csv

To run these scripts, substitute the file name on line 16 with the condition data and press "Run"

data and code used for primary statistical analysis can be found under:
project data + code > statistical analysis > primary, which contains
an R script, a .rdata file, and a data key/dictionary spreadsheet

data and code used for supplementary statistical analysis can be found under:project data + code > statistical analysis > supplementary, which contains
an R script, a .rdata file, a data key/dictionary spreadsheet, and a pdf of the supplementary materials

#software versions and installations#

software used: Matlab 2018b, R version 4.1.2 (2021-11-01)
to install R: https://www.r-project.org/, no specific installation time is suggested, more info can be found on
https://cran.r-project.org/doc/manuals/r-release/R-admin.html
to install Matlab: https://www.mathworks.com/help/install/ug/install-products-with-internet-connection.html, which suggests 30-45 minutes for installation time
