print('...preprocessing...')

file <- "data/UH AOT Data Report_ Oct 2021.xlsx"
sheets <- excel_sheets(file)

required_sheets_csv <- read.csv2("data/required_sheets.csv")['x']
required_sheets <- as.vector(as_vector(required_sheets_csv))
missing_sheets <- setdiff(required_sheets,sheets)
print(paste("found",length(missing_sheets), "missing sheets"))


# read in Sheets 2-12 and create dataframes
# these sheets have col names at row 5
# sheets 1, 13, 14 have col names at row 1

data = {}
sheet_idx <- c(2:12) # modify this to import another range of sheets, e.g., c(1, 2, 5)
for (i in sheet_idx){
  sheet <- read_excel(file, sheet = i)
  sheet_col_names <- unname(as_vector(sheet[5,]))  # colnames always on this row?
  sheet_data <- sheet[6:nrow(sheet),] # data always begins on this row?
  colnames(sheet_data) <- sheet_col_names
  data[[sheets[i]]] <- sheet_data
  write_excel_csv2(sheet_data, paste("data/",sheets[i],".csv",sep=""),delim=",")
}

print('preprocessing complete.')

