print('...preprocessing NOMS data...')

file <- "data/NOMS.csv"
noms_df <- read.csv2(file, sep=",")

# TODO - 
# rm trailing ' from 


preprocess_args <- list(file, idx, 5, 6, TRUE)
noms_data <- do.call(fun, preprocess_args)

print('preprocessing complete.')

