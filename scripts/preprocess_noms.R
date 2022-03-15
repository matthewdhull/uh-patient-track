print('...preprocessing NOMS data...')

noms_file <- "data/NOMS.csv"
code_lookup_file <- "data/CMHSNOMsAdultClientLevelMeasuresCodebook_20210204.xlsx"
noms_df <- read.csv2(noms_file, sep=",")
code_lookup_sheet <- read_excel(code_lookup_file)

# TODO - 
# rm trailing ' from `ConsumerID`
# verify required cols
# import lookup table
# implement code lookup for req'd cols

# preprocess_args <- list(file, idx, 5, 6, TRUE)
# noms_data <- do.call(fun, preprocess_args) # write out preprocessed data

print('preprocessing NOMS data complete.')

