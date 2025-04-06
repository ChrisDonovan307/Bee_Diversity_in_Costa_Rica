# write_raw_csvs
# 2024.03.16

# Leaving original excel files alone, just rewriting them as non-proprietary
# format to work with as raw data. I started doing cute but ultimately 
# unnecessary things with embedding metadata and figured it wasn't worth the 
# trouble for now. I commented that out and am just leaving the part where we 
# rewrite as csv.



# # Appendix 1 --------------------------------------------------------------
# 
# 
# # Load both sheets
# path1 <- '1_raw/original_excel_files/Appendix 1. Visited farms.xlsx'
# dat <- map(1:2, ~ read_excel(path1, sheet = .))
# 
# get_str(dat)
# get_str(dat[[1]])
# get_str(dat[[2]])
# # looks good
# 
# unique(dat[[2]][[3]])
# # nothing in column 3 of dictionary, let's remove it
# 
# dat[[2]] <- dat[[2]][, -3]
# dim(dat[[2]])
# # good good
# 
# # Rename and get rid of empty lines in dictionary
# a1 <- dat[[1]]
# a1_dict <- dat[[2]][1:20, ]
# 
# # Save a1 as csv
# write_csv(a1, '1_raw/appendix_1_visited_farms.csv')
# 
# # Write metadata_lines
# metadata_lines <- readLines('metadata_template.csv')
# # Edit this manually here
# 
# # Pull in dictionary data to add
# df_lines <- apply(a1_dict, 1, function(row) paste(row, collapse = ": ")) |> 
#     str_replace_all(pattern = "\\r\\n", replacement = ' ')
# df_comment <- paste("#  ", df_lines, sep = " ")
# 
# # Insert dictionary into metadata
# new_meta <- c(
#     metadata_lines[1:11],
#     df_comment,
#     metadata_lines[12:length(metadata_lines)]
# )
# 
# # Save over metadata template
# writeLines(new_meta, 'metadata_template.csv')
# 
# # Embed metadata into appendix 1 csv
# save_metadata_to_csv('1_raw/appendix_1_visited_farms.csv')
# 
# remove_data_objects()
# 
# 
# 
# ### Functions ---------------------------------------------------------------
# 
# 
# dict_to_lines <- function(dict_object) {
#     lines <- apply(dict_object, 1, function(row) paste(row, collapse = ": ")) |> 
#         str_replace_all(pattern = "\\r\\n", replacement = ' ')
#     commented_lines <- paste("#  ", lines, sep = " ")
#     return(commented_lines)
# }
# 
# insert_into_metadata <- function(dict_as_lines,
#                                  metadata_path = 'metadata_template.csv') {
#     
#     old_meta <- readLines(metadata_path)
#     
#     new_meta <- c(
#         old_meta[1:11],
#         dict_as_lines,
#         old_meta[12:length(old_meta)]
#     )
#     
#     writeLines(new_meta, metadata_path)
# }



# Just save as csv --------------------------------------------------------


# Load paths and names
(file_paths <- list.files('1_raw/original_excel_files',
                          full.names = TRUE))

(new_csv_paths <- file_paths |> 
        str_replace_all('/original_excel_files', '') |> 
        str_sub_all(end = -6) |> 
        str_c('.csv'))

files <- map(file_paths, read_excel)
get_str(files)

# save as csv
map2(files, new_csv_paths, ~ write_csv(.x, .y))
