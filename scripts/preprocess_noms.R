print('...preprocessing NOMS data...')

noms_file <- "data/NOMS.csv"
code_lookup_file <- "data/CMHSNOMsAdultClientLevelMeasuresCodebook_20210204.xlsx"
df <- read.csv2(noms_file, sep=",")


code_lookup_df <- read_excel(code_lookup_file)

colnames(code_lookup_df) <- code_lookup_df[1,,] # first row contains cols
# ignore first row (colnames)
code_lookup_df <- code_lookup_df[2:nrow(code_lookup_df),]
# add row to mark doc end
end_lookup_doc <- code_lookup_df[nrow(code_lookup_df),]
end_lookup_doc$`Field Name` = "End"
code_lookup_df <- rbind(code_lookup_df, end_lookup_doc)


# TODO - 
# verify required cols
noms_cols <- colnames(df)

# implement code lookup for req'd cols
# split column "Value Description" for easier lookup
splits <- strsplit(code_lookup_df$`Value Descriptions`, " = ")
split_v <- c()
split_d <- c()
for (i in seq(1, length(splits))){
  v <- splits[i][[1]][1]
  d <- splits[i][[1]][2]
  split_v <- append(split_v, v)
  split_d <- append(split_d, d)
}

# format specific fields for easier lookup
# rm trailing ' from `ConsumerID`
df$ConsumerID <- substr(df$ConsumerID, 0,6)
# add leading '0' to NOMS Assessment field code to match lookup
df$Assessment <- as.character(df$Assessment)
df$Assessment[substr(df$Assessment, 1, 1)!="-"] = paste("0", df$Assessment, sep="")

# add new cols to NOMS for separate "value description" fields
# named as "code" and "code_description"
# rearrange columns so "Values Descriptions" are next to "code" / "code description"
code_lookup_df$code <- split_v
code_lookup_df$code_description <- split_d
code_lookup_df <- code_lookup_df[,c(1:7, 10:11, 8:9)] 

# collapse verbose "N/A .... " descriptions to be only NA
na_m1 <- which ( code_lookup_df$code == -1)
na_m4 <- which ( code_lookup_df$code == -4)
code_lookup_df$code_description[na_m1] = NA
code_lookup_df$code_description[na_m4] = NA

# get NOMS column index e.g., Gender
field = "Gender"
noms_gender_col_index <- which (noms_cols == field)

get_lookup_code_range <- function(field_name) {
  # returns a sequence vector of index values for a field and its codes/descriptions
  # operates on code_lookup_df values
  # get lookup Table row index 
  # step table row until not N/A to find end of codes for field
  # start at given index row +1
  lookup_t_row_index <- which (code_lookup_df$`Field Name` == field_name) 
  start_index <- lookup_t_row_index + 1
  v = code_lookup_df$`Field Name`[start_index]
  while (is.na(v)) {
    start_index <- start_index + 1
    v = code_lookup_df$`Field Name`[start_index]
  } 
  return (seq(lookup_t_row_index, start_index-1))
}

get_lookup_dict <- function(location_range) {
  # construct lookup for the codes/descriptions in 
  # code lookup df
  field_lookup <- {}
  field_codes <- code_lookup_df$code[c(location_range)]
  field_codes_descriptions <- code_lookup_df$code_description[c(location_range)] 
  for (g in seq(1:length(field_codes))){
    field_lookup[field_codes[g]] <- field_codes_descriptions[g]
  }
  return (field_lookup)
}

lookup <- function(x, d=NA) {
  # function used in lapply to replace values from lookup
  # dict in NOMS df
  return (d[x])
}

# example 1: De-code 'Gender' using lookup 
# examples usage of get_lookup_code_range()
# returns: > 219 220 221 222 223 224 225
# these values are row nums in lookup df for each code
# gender_codes_range <- get_lookup_code_range("Gender")

# example usage of get_lookup_dict()
# using the sequence, combine "codes" & "code_description"
# as a dictionary so we can lookup key value pairs
# gender_lookup <- get_lookup_dict(gender_codes_range)

# make replacement for codes. 
# use dictionary and replace all codes with their values in NOMS
# noms_df$Gender <- lapply(noms_df$Gender, d=gender_lookup)

# example 2: De-code 'Interview Type' using lookup
# de-code interview / assesment type
# interview_type_code_range <- get_lookup_code_range("InterviewType_07")
# interview_type_lookup <- get_lookup_dict(interview_type_code_range)
# noms_df$InterviewType_07 <- lapply(noms_df$InterviewType_07, lookup, d=interview_type_lookup)


# All NOMS cols to de-code
decode_cols <- noms_cols[c(4:12,18:41,43:84, 86:191,193, 196:218)]


decode_NOMS_fields <- function(df, fields){
  for (i in seq(1:length(fields))){
     field_lookup <- {}
     field = fields[i]
     field_code_range <- get_lookup_code_range(field)
     field_lookup <- get_lookup_dict(field_code_range)
     df[,field] <- as.character(df[,field])
     l <- df[, field]
     dlist <- lapply(l, lookup, d=field_lookup)
     dlist <- unlist(dlist)
     df[, field] <- dlist
  }
  return (df)
}

df <- decode_NOMS_fields(df, decode_cols)

# preprocess_args <- list(file, idx, 5, 6, TRUE)
# noms_data <- do.call(fun, preprocess_args) # write out preprocessed data

print('preprocessing NOMS data complete.')

