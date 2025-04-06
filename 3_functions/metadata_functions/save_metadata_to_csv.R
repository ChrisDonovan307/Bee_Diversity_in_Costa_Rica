# save_metadata_to_csv
# 2024.03.14

# Combine metadata with a csv and save it. Has an option to remove the template
# too, but it is probably risky. Use remove_metadata_template() once you 
# confirm that it worked.

save_metadata_to_csv <- function(csv_file_path,
                                 metadata_file = 'metadata_template.csv',
                                 remove_template = FALSE) {
    
    if (any(grepl(" METADATA ", readLines(csv_file_path), ignore.case = FALSE))) {
        stop('\n\nMetadata already exists for .csv file! ', 
             'Edit or remove existing metadata.\n\n',
             call. = FALSE)
    }
    
    metadata <- readLines(metadata_file)
    csv_data <- readLines(csv_file_path)
    combined <- c(metadata, csv_data)
    
    writeLines(combined, csv_file_path)
    
    if (remove_template == TRUE) {
        rm(metadata_file)
    }
    
    cat('\n\n* Metadata saved to', csv_file_path, '*\n\n')
    
}
