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

uh_data_to_df <- function (excel_path=NA, sheet_idxs=NA, header_row_loc=0, data_row_loc=1, write_csv=FALSE){
  
  #' Create UH AOT DataFrames
  #' @param excel_path character. Path of the excel file to be read
  #' @param sheet_idxs vector. Indexes of excel sheets to be imported
  #' @param header_row_loc integer. Location of header row. Defaults to 0.
  #' @param data_row_loc integer. Location of data start row. Defaults to 1.
  #' @param write_csv Boolean. Write .csv files for each sheet. Defaults to FALSE
  #' @usage data <- uh_data_to_df("data.xlsx", c(1:5), 5, 6, TRUE)
  #' @return Dictionary of data frames for each Sheet as the key.
  #' @details import an .xslx file of monthly UH AOT Data and construct a data frame for each sheet imported
    
  data = {}
    sheets <- excel_sheets(excel_path)
    for (i in sheet_idxs){
      sheet <- read_excel(excel_path, sheet = i)
      sheet_col_names <- unname(as_vector(sheet[header_row_loc,]))      # colnames always on this row?
      sheet_data <- sheet[data_row_loc:nrow(sheet),]                    # data always begins on this row?
      colnames(sheet_data) <- sheet_col_names
      data[[sheets[i]]] <- sheet_data
      if (write_csv) {
        write_excel_csv2(sheet_data, paste("data/",sheets[i],".csv",sep=""),delim=",")
      }
    } 
    return  (data)
}

idx <- c(2:12) # modify this to import another range of sheets, e.g., c(1, 2, 5)
uh_args <- list(file, idx, 5, 6, TRUE)
uh_data = do.call(uh_data_to_df, uh_args)

print('preprocessing complete.')

