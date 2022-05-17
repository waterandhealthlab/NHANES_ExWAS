The folder contains the scripts needed to reproduce the analysis described in the manuscript:
"An exposome-wide association study on body mass index in adolescents using the National Health and Nutrition Examination Survey (NHANES) 2003-2004 and 2013-2014 data"

All scripts can run at once using the "rendering_final.R" script which directs the output of the RMD to the folder "wordoutput".

List of the RMD scripts and notes on their content
1. S0_DataDownloadR1 -- download of the data in rds format and saving in the folder "rawdata"
2. S1_DataPrepR1 -- use of the "rawdata" for the creation of the final datasets used in the analysis
3. S2_RegAnalysisR1 -- use of data produced during in the previous scripts (input in the folder "produceddata" for the main statistical analysis
4. S3_SensitivityAnalysisR1 -- use of data produced during in the previous scripts (input in the folder "produceddata" for the sensitivity analysis

Additional files
template.docx --- word file with the theme used in the output
vars_summary_used.csv --- list with the variable names used in the scripts

Folders
wordoutput --- includes all the word files generated from the RMDs
CorrCoefficientsResults --- includes the csv files with the results of the correlation analysis
figures --- includes the figures generated in the scripts

Folders not included but generated
rawdata and produceddata --- these two folders have not been included in the supplementary material because the data used in the analysis are publicly available and can be downloaded using the scripts

Correspondence and requests for materials should be addressed to K.C. Makris (konstantinos.makris@cut.ac.cy) 

Date: 17/05/2021
