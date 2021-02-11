library(DT)
library(data.table)
library(plotly)
library(readr)
options(width = 150)
'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
fullData <- setDT(read_csv("epi_data.csv"))

fullData <- fullData[, is_epi_trim_list:=grepl("\\|", epi_trim_list)]
unique_ia_trims <- unique(fullData$ia_trim_name)
unique_epi_trims <- unique(fullData[is_epi_trim_list == FALSE, epi_trim])
message("Total unique trims in IA:", length(unique_ia_trims),
  " in EPI:", length(unique_epi_trims))
message("Overlap:", length(intersect(unique_ia_trims, unique_epi_trims)))
new_trims_intro <- setdiff(unique_epi_trims, unique_ia_trims)
message("Total New Trims Introduced:", length(new_trims_intro))

fullData <- fullData[, is_epi_transmission_list:=grepl("\\|", epi_transmission_list)]
unique_ia_transmissions <- unique(fullData$ia_transmission)
unique_epi_transmissions <- unique(fullData[is_epi_transmission_list == FALSE, epi_transmission])
message("Total unique transmissions in IA:", length(unique_ia_transmissions),
  " in EPI:", length(unique_epi_transmissions))
message("Overlap:", length(intersect(unique_ia_transmissions, unique_epi_transmissions)))
new_transmissions_intro <- setdiff(unique_epi_transmissions, unique_ia_transmissions)
message("Total New transmissions Introduced:", length(new_transmissions_intro))
