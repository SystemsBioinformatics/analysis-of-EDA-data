source("scripts/fractiomate_parser.R")
folder <- "testdata/Antibiotics assay"

p1 <- parse_fractiomate(file.path(folder,"20201216_FractioMate_112957_EFF1 AB.txt"))

source("scripts/platereader_parser.R")
type_single_photometric_file <- "20201217_Kralingseveer fractionated_OD600_Plate 1.txt"
type_multiple_fluorimetric_file <- "20201217_Kralingseveer fractionated_Resazurin_Plate 1.txt"

file <- file.path(folder,type_single_photometric_file)
p2 <- parse_platereader(file)

file <- file.path(folder,type_multiple_fluorimetric_file)
p3 <- parse_platereader(file)
