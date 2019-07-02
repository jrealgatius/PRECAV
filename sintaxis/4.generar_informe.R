library(rmarkdown)

code_dir<-"sintaxis"
report_filename<-"EDICIO_PRECAV_V3.Rmd"
report_filename <- file.path(code_dir, report_filename)

output_dir <- "output/reports"
output <- file.path("..",output_dir)

render(report_filename, output_dir = output_dir, params = list(output_dir = output))

