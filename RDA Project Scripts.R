library(readxl)
LEGO_RDA_Project_Mike_Sullivan <- read_excel("~/NYCDSA Training/RDA Project/LEGO RDA Project - Mike Sullivan.xlsx")
View(LEGO_RDA_Project_Mike_Sullivan)

LEGO_RDA_Project_Mike_Sullivan$Date = format(as.Date(paste(
  substr(LEGO_RDA_Project_Mike_Sullivan$Week, start = 1, stop = 4),
  substr(LEGO_RDA_Project_Mike_Sullivan$Week, start = 5, stop = 6),
  7), format = "%Y %U %u"), "%m/%d/%Y")

