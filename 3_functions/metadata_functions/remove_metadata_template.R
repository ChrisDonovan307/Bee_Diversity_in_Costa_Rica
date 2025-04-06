# remove_metadata_template
# 2024.03.14

# separate function to remove template. There is an option within 
# save_metadata_to_csv.R to do remove it, but it seems dangerous. Having this
# separate just in case. 

remove_metadata_template <- function(template_filepath = 'metadata_template.csv') {
    
    if (file.exists(template_filepath)) {
        
        file.remove(template_filepath)
        cat("\n\n*", template_filepath, "deleted *\n\n", sep = " ")
        
    } else {
        
        cat("\n\n* No file found *\n\n")
        
    }
}

