.check_range <- function(arg, range) {
    if (!isTRUE(arg %in% range)) {
        argname <- deparse(substitute(arg))
        stop(paste0(argname, " must be one of the following values: ", paste0(range, collapse = ", "), "."))
    }
    return(NULL)
}
.check_length <- function(arg, length) {
    if (length(arg) != length) {
        argname <- deparse(substitute(arg))
        stop(paste0(argname, " must be a vector of length ", length, "."))
    }
    return(NULL)
}
.check_type <- function(arg, types) {
    if (!isTRUE(typeof(arg) %in% types)) {
        argname <- deparse(substitute(arg))
        stop(paste0(argname, " must be of type ", types, "."))
    }
    return(NULL)
}
.check_stage <- function(design_object, stage) {
    if (typeof(stage) == "double") {
      stage <- as.integer(round(stage))
      warning("'stage' converted from double to integer.")
    }
    .check_type(stage, "integer")
    if (!isTRUE(stage %in% seq.int(length.out = n_stages(design_object))))
      stop(paste0("'design_object' has only ", n_stages(design_object), " stages. Cannot access stage ", stage, "."))
    return(NULL)
}
.check_test_stat_values  <- function(stage, test_stat_values) {
    .check_type(test_stat_values, c("double", "integer"))
    if (length(test_stat_values) != stage - 1L)
      stop(paste0("'test_stat_values' needs to be of length ", stage - 1L, " (#stages - 1)", "."))
    return(NULL)
}
.check_not_na_or_inf <- function(arg){
  if (any(is.na(arg))){
    argname <- deparse(substitute(arg))
    stop(paste0(argname, " must not contain NA's."))
  }
  if (any(is.infinite(arg))){
    stop(paste0(argname, " must not contain Inf or -Inf."))
  }
  return(NULL)
}
