GetAllocation <- function(json_file) {
    allocation <- json_file$allocation
    if (is.null(allocation)) {
        return(NULL)
    }
    return(allocation)
}
