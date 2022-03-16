print('...preprocessing NOMS data...')

noms_file <- "data/NOMS.csv"
code_lookup_file <- "data/CMHSNOMsAdultClientLevelMeasuresCodebook_20210204.xlsx"
noms_df <- read.csv2(noms_file, sep=",")


code_lookup_df <- read_excel(code_lookup_file)
colnames(code_lookup_df) <- code_lookup_df[1,,] # first row contains cols

# rm trailing ' from `ConsumerID`
noms_df$ConsumerID <- substr(noms_df$ConsumerID, 0,6)

# TODO - 
# verify required cols
noms_cols <- colnames(noms_df)

# implement code lookup for req'd cols
# split value description for easier lookup
splits <- strsplit(code_lookup_df$`Value Descriptions`, " = ")
split_v <- c()
split_d <- c()
for (i in seq(1, length(splits))){
  v <- splits[i][[1]][1]
  d <- splits[i][[1]][2]
  split_v <- append(split_v, v)
  split_d <- append(split_d, d)
}

code_lookup_df$code <- split_v
code_lookup_df$code_description <- split_d
# rearrange columns so values descriptions are next to code / code description
code_lookup_df <- code_lookup_df[,c(1:7, 10:11, 8:9)] 

# get NOMS column index e.g., Gender
field = "Gender"
noms_gender_col_index <- which (noms_cols == field)

get_lookup_code_range <- function(field_name) {
  # returns a range of index values for a field and its codes/descriptions
  # operates on code_lookup_df values
  # get lookup Table row index 
  # step table row until not N/A to find end of codes for field
  # start at given index row +1
  lookup_t_row_index <- which (code_lookup_df$`Field Name` == field) 
  start_index <- lookup_t_row_index + 1
  v = code_lookup_df$`Field Name`[start_index]
  while (is.na(v)) {
    start_index <- start_index + 1
    v = code_lookup_df$`Field Name`[start_index]
  } 
  return (seq(lookup_t_row_index, start_index-1))
}

# examples usage of get_lookup_code_range()
# returns: > 219 220 221 222 223 224 225
codes_range <- get_lookup_code_range("Gender")

get_lookup_dict <- function(field, location_range) {
  # construct lookup for the codes/descriptions
  # operates on code lookup df
  field_codes <- code_lookup_df$code[c(codes_range)]
  field_codes_descriptions <- code_lookup_df$code_description[c(codes_range)] 
  field_lookup <- {}
  for (g in seq(1:length(field_codes))){
    field_lookup[field_codes[g]] <- field_codes_descriptions[g]
  }
  return (field_lookup)
}

# example usage of get_lookup_dict()
gender_lookup <- get_lookup_dict('Gender', codes_range)

lookup <- function(k){
  return (gender_lookup[k])
}

# make replacement for codes. 
noms_df$Gender <- lapply(noms_df$Gender, lookup)



# preprocess_args <- list(file, idx, 5, 6, TRUE)
# noms_data <- do.call(fun, preprocess_args) # write out preprocessed data

print('preprocessing NOMS data complete.')

