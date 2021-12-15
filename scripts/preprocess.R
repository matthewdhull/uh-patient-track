print('...preprocessing...')

file <- "data/UH AOT Data Report_ Oct 2021.xlsx"
sheets <- excel_sheets(file)

required_sheets_csv <- read.csv2("data/required_sheets.csv")['x']
required_sheets <- as.vector(as_vector(required_sheets_csv))
missing_sheets <- setdiff(required_sheets,sheets)
print(paste("found",length(missing_sheets), "missing sheets"))
