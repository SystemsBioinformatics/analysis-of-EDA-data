source("scripts/fractiomate_parser.R")
folder1 <- "testdata/Antibiotics assay"
folder2 <- "testdata/Sediment TTR"

p1 <- parse_fractiomate(file.path(folder1,"20201216_FractioMate_112957_EFF1 AB.txt"))
p1a <- parse_fractiomate(file.path(folder2,"FractioMate_Sediment.txt"))

source("scripts/platereader_parser.R")
type_single_photometric_file <- "20201217_Kralingseveer fractionated_OD600_Plate 1.txt"
type_multiple_fluorimetric_file <- "20201217_Kralingseveer fractionated_Resazurin_Plate 1.txt"
type_single_fluorimetric_file <- "20200224 S4 TRIS.txt"

file <- file.path(folder1,type_single_photometric_file)
p2 <- parse_platereader(file)

file <- file.path(folder1,type_multiple_fluorimetric_file)
p3 <- parse_platereader(file)

file <- file.path(folder2,type_single_fluorimetric_file)
p4 <- parse_platereader(file)
