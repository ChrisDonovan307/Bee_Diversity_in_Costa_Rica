# get_metadata_template
# 2024.03.14

# Creates metadata template in .csv and opens it for editing
# Adapted from function by Dr. Nicholas Gotelli, UVM

get_metadata_template <- function(file_name = NULL) {

if (is.null(file_name)) {
  file_name <- "metadata_template.csv"
}
    
if (file.exists(file_name)) stop(
    '\n\n', file_name, " already exists! ",
    'Choose a new filename or remove existing template.\n\n') else {
    
    write.table(
        cat("# *** METADATA ***", "\n",
            "#------------------------------", "\n",
            "# Title: ","\n",
            "# Metadata timestamp: ", format(Sys.time(), "%d %B %Y %X"),"\n",
            "# Author: ", "\n",
            "# Author email: ", "\n",
            "#------------------------------", "\n",
            "# Description: ", "\n",
            "# Data Collection: ", "\n",
            "# Variables: ", "\n",
            "# Missing Data or Quality Issues: ", "\n",
            "# Ownership: ", "\n",
            "#------------------------------", "\n",
            "# Changelog", "\n",
            "# Date: , Changes:", "\n",
            "#------------------------------", "\n",
            "# *** END OF METADATA ***", "\n\n",
            file = file_name,
            row.names = "",
            col.names = "",
            sep = ""))
}
    
    file.edit(file_name)
    
} 
