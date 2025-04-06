# explore
# 2024.03.16

# Make it easier to check out variables and values in datasets.

explore <- function(var, data = dat) {
    
    list <- list(
        Table = sort(table(dat[[var]], useNA = 'always'), decreasing = TRUE),
        Sum = sum(table(dat[[var]])),
        Unique_Values = sort(unique(dat[[var]])),
        Num_Unique_Values = length(unique(dat[[var]])))
     
    return(list)
}
