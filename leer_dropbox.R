### Write the data as usual keeping the colnames
writeWorksheetToFile(file = "/tmp/file.xlsx", data = mtcars, sheet = "test",
                     startRow = 2)
id <- "1s_U5JH7OPqlNhfIMqyoINoIMd_a1hCEX" # google file ID
library(readxl)
read_xlsx(sprintf("https://docs.google.com/uc?id=%s&export=download", id),sheet = "Hoja1",header=FALSE)
