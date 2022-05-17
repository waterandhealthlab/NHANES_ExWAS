
if(!dir.exists("wordoutput/")){
  dir.create("wordoutput/")
}

print(paste0("Date: ", lubridate::today()))  
tictoc::tic()
rmarkdown::render("S0_DataDownloadR1.Rmd", 
                  output_file = "wordoutput/DataDownload.docx")
tictoc::toc()

tictoc::tic()

rmarkdown::render("S1_DataPrepR1.Rmd", 
                  output_file = paste0("wordoutput/S1_DataPrep.docx"))
tictoc::toc()

tictoc::tic()

rmarkdown::render("S2_RegAnalysisR1.Rmd", 
                  output_file = paste0("wordoutput/S2_RegAnalysis.docx"))
tictoc::toc()


tictoc::tic()

rmarkdown::render("S3_SensitivityAnalysisR1.Rmd", 
                  output_file = paste0("wordoutput/S3_SensitivityAnalysis.docx"))

tictoc::toc()
