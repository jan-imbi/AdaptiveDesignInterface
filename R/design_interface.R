# Register gsDesign as an S4 class
setOldClass("gsDesign")

# Wrapper generics to access commonly needed design objects
setGeneric(
  "n_stages",
  function(design_object, ...) standardGeneric("n_stages")
)
setGeneric(
  "n_groups",
  function(design_object, ...) standardGeneric("n_groups")
)
setGeneric(
  "sample_size",
  function(design_object, ...) standardGeneric("sample_size")
)
setGeneric(
  ".sample_size",
  function(design_object, ...) standardGeneric(".sample_size")
)
setGeneric(
  ".n_information",
  function(design_object, ...) standardGeneric(".n_information")
)
setGeneric(
  "max_sample_size",
  function(design_object, ...) standardGeneric("max_sample_size")
)
setGeneric(
  "min_sample_size",
  function(design_object, ...) standardGeneric("min_sample_size")
)
setGeneric(
  "constant_sample_size",
  function(design_object, ...) standardGeneric("constant_sample_size")
)
setGeneric(
  "critical_value_z",
  function(design_object, ...) standardGeneric("critical_value_z")
)
setGeneric(
  ".critical_value_z",
  function(design_object, ...) standardGeneric(".critical_value_z")
)
setGeneric(
  "futility_boundary_z",
  function(design_object, ...) standardGeneric("futility_boundary_z")
)

# Wrappers to translate between t, p and log(p)
setGeneric(
  "critical_value_t",
  function(design_object, ...) standardGeneric("critical_value_t")
)
setGeneric(
  "futility_boundary_t",
  function(design_object, ...) standardGeneric("futility_boundary_t")
)
setGeneric(
  "critical_value_p",
  function(design_object, ...) standardGeneric("critical_value_p")
)
setGeneric(
  "futility_boundary_p",
  function(design_object, ...) standardGeneric("futility_boundary_p")
)
setGeneric(
  "critical_value_logp",
  function(design_object, ...) standardGeneric("critical_value_logp")
)
setGeneric(
  "futility_boundary_logp",
  function(design_object, ...) standardGeneric("futility_boundary_logp")
)

#' @import stats
setMethod("futility_boundary_p",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            pnorm(futility_boundary_z(design_object, ...), lower.tail = FALSE)
          }
)
#' @import stats
setMethod("critical_value_logp",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            pnorm(critical_value_z(design_object, ...), log.p = TRUE, lower.tail = FALSE)
          }
)
#' @import stats
setMethod("futility_boundary_logp",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            pnorm(futility_boundary_z(design_object, ...), log.p = TRUE, lower.tail = FALSE)
          }
)
#' @import stats
setMethod("critical_value_t",
          signature(design_object = "ANY"),
          function(design_object, df, ...) {
            qt(do.call(critical_value_logp, c(list(design_object), list(...))), df = df, log.p = TRUE, lower.tail = FALSE)
          }
)
#' @import stats
setMethod("futility_boundary_t",
          signature(design_object = "ANY"),
          function(design_object, df, ...) {
            qt(do.call(futility_boundary_logp, c(list(design_object), list(...))), df = df, log.p = TRUE, lower.tail = FALSE)
          }
)

# Make sure virtual default methods throw meaningful errors
setMethod("n_stages",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'n_stages' not implemented for class ", class(design_object), "."))
          }
)
setMethod("n_groups",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'n_groups' not implemented for class ", class(design_object), "."))
          }
)
setMethod("sample_size",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'sample_size' not implemented for class ", class(design_object), "."))
          }
)
setMethod(".sample_size",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function '.sample_size' not implemented for class ", class(design_object), "."))
          }
)
setMethod(".n_information",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function '.sample_size' not implemented for class ", class(design_object), "."))
          }
)
setMethod("max_sample_size",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'max_sample_size' not implemented for class ", class(design_object), "."))
          }
)
setMethod("min_sample_size",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'min_sample_size' not implemented for class ", class(design_object), "."))
          }
)
setMethod("critical_value_z",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'critical_value_z' not implemented for class ", class(design_object), "."))
          }
)
setMethod(".critical_value_z",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function '.critical_value_z' not implemented for class ", class(design_object), "."))
          }
)
setMethod("futility_boundary_z",
          signature(design_object = "ANY"),
          function(design_object, ...) {
            stop(paste0("Function 'futility_boundary_z' not implemented for class ", class(design_object), "."))
          }
)


# n_stages for gsDesign, rpact, adoptr
setMethod("n_stages",
          signature(design_object = "gsDesign"),
          function(design_object, ...) {
            return(design_object$k)
          }
)
setMethod("n_stages",
          signature(design_object = "TrialDesignPlan"),
          function(design_object, ...) {
            return(design_object$.design$kMax)
          }
)
setMethod("n_stages",
          signature(design_object = "OneStageDesign"),
          function(design_object, ...) {
            return(1L)
          }
)
setMethod("n_stages",
          signature(design_object = "TwoStageDesign"),
          function(design_object, ...) {
            return(2L)
          }
)

#' @import stats
setMethod("critical_value_p",
  signature(design_object = "ANY"),
  function(design_object, ...) {
    pnorm(critical_value_z(design_object, ...), lower.tail = FALSE)
  }
)


# n_groups for gsDesign, rpact, adoptr
setMethod("n_groups",
  signature(design_object = "gsDesign"),
  function(design_object, ...) {
    return(NA_integer_)
  }
)
setMethod("n_groups",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, ...) {
    return(design_object$groups)
  }
)
#' @import methods
setMethod("n_groups",
  signature(design_object = "TwoStageDesign"),
  function(design_object, adoptr_DataDistribution = NULL, ...) {
    if (!missing(adoptr_DataDistribution)) {
      if (!is(adoptr_DataDistribution, getClassDef("DataDistribution", package = "adoptr")))
        stop(paste0("'adoptr_DataDistribution' needs to be an object of class 'DataDistribution' (from the 'adoptr' package)."))
      return(1L + adoptr_DataDistribution@two_armed)
    }
    # stop(paste0("In 'adoptr', the number of groups in a design is encoded in the DataDistribution object. Please provide this object."))
    return(NA_integer_)
  }
)

# .critical_value_z for gsDesign, rpact, adoptr
#' @import utils
setMethod(".critical_value_z",
          signature(design_object = "gsDesign"),
          function(design_object, stage, test_stat_z_values = numeric(), ...) {
            return(design_object$upper$bound[[stage]])
          }
)
#' @import utils
setMethod(".critical_value_z",
          signature(design_object = "TrialDesignPlan"),
          function(design_object, stage, test_stat_z_values = numeric(), ...) {
            return(design_object$.design$criticalValues[[stage]])
          }
)
setMethod(".critical_value_z",
          signature(design_object = "TwoStageDesign"),
          function(design_object, stage, test_stat_z_values = numeric(), ...) {
            return(
              switch(stage,
                     design_object@c1e,
                     adoptr::c2(d = design_object, x1 = test_stat_z_values[[stage - 1L]])
              )
            )
          }
)

# critical_value_z for gsDesign, rpact, adoptr
#' @import utils
setMethod("critical_value_z",
  signature(design_object = "gsDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    zes <- design_object$upper$bound
    if ("lower" %in% names(design_object))
      zfs <- c(design_object$lower$bound, tail(zes, 1L))
    else
      zfs <- c(rep(-Inf, length(zes) - 1L), tail(zes, 1L))
    for (i in seq.int(length.out = stage - 1L)) {
      if (test_stat_z_values[[i]] > zes[[i]])
        return(-Inf)
      if (test_stat_z_values[[i]] < zfs[[i]])
        return(Inf)
    }
    return(.critical_value_z(design_object, stage, test_stat_z_values))
  }
)
#' @import utils
setMethod("critical_value_z",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    zes <- design_object$.design$criticalValues
    if (!(design_object$.design$typeBetaSpending == "none"))
      zfs <- c(design_object$.design$futilityBounds, tail(zes, 1L))
    else
      zfs <- c(rep(-Inf, length(zes) - 1L), tail(zes, 1L))
    for (i in seq.int(length.out = stage - 1L)) {
      if (test_stat_z_values[[i]] > zes[[i]])
        return(-Inf)
      if (test_stat_z_values[[i]] < zfs[[i]])
        return(Inf)
    }
    return(.critical_value_z(design_object, stage, test_stat_z_values))
  }
)
setMethod("critical_value_z",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    return(.critical_value_z(design_object, stage, test_stat_z_values))
  }
)

# futility_boundary_z for gsDesign, rpact, adoptr
#' @import utils
setMethod("futility_boundary_z",
  signature(design_object = "gsDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    zes <- design_object$upper$bound
    if ("lower" %in% names(design_object))
      zfs <- c(design_object$lower$bound, tail(zes, 1L))
    else
      zfs <- c(rep(-Inf, length(zes) - 1L), tail(zes, 1L))
    for (i in seq.int(length.out = stage - 1L)) {
      if (test_stat_z_values[[i]] > zes[[i]])
        return(Inf)
      if (test_stat_z_values[[i]] < zfs[[i]])
        return(-Inf)
    }
    return(zfs[[stage]])
  }
)
#' @import utils
setMethod("futility_boundary_z",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    zes <- design_object$.design$criticalValues
    if (!(design_object$.design$typeBetaSpending == "none"))
      zfs <- c(design_object$.design$futilityBounds, tail(zes, 1L))
    else
      zfs <- c(rep(-Inf, length(zes) - 1L), tail(zes, 1))
    for (i in seq.int(length.out = stage - 1L)) {
      if (test_stat_z_values[[i]] > zes[[i]])
        return(Inf)
      if (test_stat_z_values[[i]] < zfs[[i]])
        return(-Inf)
    }
    return(zfs[[stage]])
  }
)
setMethod("futility_boundary_z",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    return(
      switch(stage,
        design_object@c1f,
        {
          if (test_stat_z_values[[1]] > design_object@c1e)
            -Inf
          else if (test_stat_z_values[[1]] < design_object@c1f)
            Inf
          else
            adoptr::c2(d = design_object, x1 = test_stat_z_values[[stage - 1L]])
        }
      )
    )
  }
)

# sample_size for gsDesign, rpact, adoptr
setMethod("sample_size",
  signature(design_object = "gsDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    if (group != 1L)
      stop(paste0("'gsDesign' objects do not support the concept of groups. 'group' needs to supplied as 1L."))
    zes <- sapply(seq.int(length.out = stage - 1L),
              \(stage) critical_value_z(design_object, stage, test_stat_z_values[seq.int(length.out = stage - 1L)]))
    zfs <- sapply(seq.int(length.out = stage - 1L),
              \(stage) futility_boundary_z(design_object, stage, test_stat_z_values[seq.int(length.out = stage - 1L)]))
    for (i in seq.int(length.out = stage - 1L))
      if (test_stat_z_values[[i]] > zes[[i]] | test_stat_z_values[[i]] < zfs[[i]])
        return(0L)
    if (stage == 1L)
      return(design_object$n.I[[stage]])
    else
      return(design_object$n.I[[stage]] - design_object$n.I[[stage - 1L]])
  }
)
setMethod("sample_size",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    if (n_groups(design_object) > 2)
      stop("Currently, this sample_size function only designs with 2 or less groups.")
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    .check_range(group, seq.int(length.out = n_groups(design_object)))
    zes <- sapply(seq.int(length.out = stage - 1L),
              \(stage) critical_value_z(design_object, stage, test_stat_z_values[seq.int(length.out = stage - 1L)]))
    zfs <- sapply(seq.int(length.out = stage - 1L),
              \(stage) futility_boundary_z(design_object, stage, test_stat_z_values[seq.int(length.out = stage - 1L)]))
    for (i in seq.int(length.out = stage - 1L))
      if (test_stat_z_values[[i]] > zes[[i]] | test_stat_z_values[[i]] < zfs[[i]])
        return(0L)
    return(
      switch(n_groups(design_object),
        {
        if (stage == 1L)
          design_object$numberOfSubjects[[stage]]
        else
            design_object$numberOfSubjects[[stage]] - design_object$numberOfSubjects[[stage - 1L]]
        },
        {
        if (stage == 1L)
          design_object[[gettextf("numberOfSubjects%d", group)]][[stage]]
        else
            design_object[[gettextf("numberOfSubjects%d", group)]][[stage]] - design_object[[gettextf("numberOfSubjects%d", group)]][[stage - 1L]]
        }
      )
    )
  }
)
setMethod("sample_size",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, adoptr_DataDistribution, ...) {
    .check_stage(design_object, stage)
    .check_test_stat_values(stage, test_stat_z_values)
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    .check_range(group, c(1L, 2L))
    if (group == 2L) {
      if (missing(adoptr_DataDistribution))
        stop(paste0("In 'adoptr', the number of groups in a design is encoded in the DataDistribution object. Please provide this object."))
      .check_range(group, seq.int(length.out = n_groups(design_object, adoptr_DataDistribution)))
    }
    return(
      switch(stage,
        adoptr::n1(d = design_object, round = FALSE),
        adoptr::n2(d = design_object, x1 = test_stat_z_values[[1]], round = FALSE)
      )
    )
  }
)

# fast and unsafe versions of sample_size for gsDesign, rpact, adoptr
setMethod(".sample_size",
          signature(design_object = "gsDesign"),
          function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
            if (stage == 1L)
              return(design_object$n.I[[stage]])
            else
              return(design_object$n.I[[stage]] - design_object$n.I[[stage - 1L]])
          }
)
setMethod(
  ".sample_size",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
    return(
      switch(n_groups(design_object),
        {
          if (stage == 1L) {
            design_object$numberOfSubjects[[stage]]
          } else {
            design_object$numberOfSubjects[[stage]] - design_object$numberOfSubjects[[stage - 1L]]
          }
        },
        {
          if (stage == 1L) {
            design_object[[gettextf("numberOfSubjects%d", group)]][[stage]]
          } else {
            design_object[[gettextf("numberOfSubjects%d", group)]][[stage]] - design_object[[gettextf("numberOfSubjects%d", group)]][[stage - 1L]]
          }
        }
      )
    )
  }
)
setMethod(
  ".sample_size",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, adoptr_DataDistribution, ...) {
    return(
      switch(stage,
        adoptr::n1(d = design_object, round = FALSE),
        adoptr::n2(d = design_object, x1 = test_stat_z_values[[1]], round = FALSE)
      )
    )
  }
)

# fast and unsafe versions of sample_size, providing the number of events for survival trials, for gsDesign, rpact, adoptr
setMethod(".n_information",
          signature(design_object = "gsDesign"),
          function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
            if (stage == 1L)
              return(design_object$n.I[[stage]])
            else
              return(design_object$n.I[[stage]] - design_object$n.I[[stage - 1L]])
          }
)
setMethod(
  ".n_information",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
    return(
      switch(n_groups(design_object),
             {
               if (stage == 1L) {
                 design_object$numberOfSubjects[[stage]]
               } else {
                 design_object$numberOfSubjects[[stage]] - design_object$numberOfSubjects[[stage - 1L]]
               }
             },
             {
               if (stage == 1L) {
                 design_object[[gettextf("numberOfSubjects%d", group)]][[stage]]
               } else {
                 design_object[[gettextf("numberOfSubjects%d", group)]][[stage]] - design_object[[gettextf("numberOfSubjects%d", group)]][[stage - 1L]]
               }
             }
      )
    )
  }
)
setMethod(
  ".n_information",
  signature(design_object = "TrialDesignPlanSurvival"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, ...) {
    ngrps <- n_groups(design_object)
    return(design_object$eventsPerStage[[stage]] / ngrps)
  }
)
setMethod(
  ".n_information",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, test_stat_z_values = numeric(), group = 1L, adoptr_DataDistribution, ...) {
    return(
      switch(stage,
             adoptr::n1(d = design_object, round = FALSE),
             adoptr::n2(d = design_object, x1 = test_stat_z_values[[1]], round = FALSE)
      )
    )
  }
)

# max_sample_size for gsDesign, rpact, adoptr
setMethod(
  "max_sample_size",
  signature(design_object = "gsDesign"),
  function(design_object, stage, group = 1L, ...) {
    .check_stage(design_object, stage)
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    if (group != 1L) {
      stop(paste0("'gsDesign' objects do not support the concept of groups. 'group' needs to supplied as 1L."))
    }
    if (stage == 1L) {
      return(design_object$n.I[[stage]])
    } else {
      return(design_object$n.I[[stage]] - design_object$n.I[[stage - 1L]])
    }
  }
)
setMethod(
  "max_sample_size",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage, group = 1L, ...) {
    .check_stage(design_object, stage)
    if (n_groups(design_object) > 2) {
      stop("Currently, this sample_size function only designs with 2 or less groups.")
    }
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    .check_range(group, seq.int(length.out = n_groups(design_object)))
    return(
      switch(n_groups(design_object),
        {
          if (stage == 1L) {
            design_object$numberOfSubjects[[stage]]
          } else {
            design_object$numberOfSubjects[[stage]] - design_object$numberOfSubjects[[stage - 1L]]
          }
        },
        {
          if (stage == 1L) {
            design_object[[gettextf("numberOfSubjects%d", group)]][[stage]]
          } else {
            design_object[[gettextf("numberOfSubjects%d", group)]][[stage]] - design_object[[gettextf("numberOfSubjects%d", group)]][[stage - 1L]]
          }
        }
      )
    )
  }
)
setMethod(
  "max_sample_size",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, group = 1L, adoptr_DataDistribution, ...) {
    .check_stage(design_object, stage)
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    .check_range(group, c(1L, 2L))
    if (group == 2L) {
      if (missing(adoptr_DataDistribution)) {
        stop(paste0("In 'adoptr', the number of groups in a design is encoded in the DataDistribution object. Please provide this object."))
      }
      .check_range(group, seq.int(length.out = n_groups(design_object, adoptr_DataDistribution)))
    }
    return(
      switch(stage,
        adoptr::n1(d = design_object),
        adoptr::n2(d = design_object, x1 = futility_boundary_z(design_object, stage = 1L) + .Machine$double.eps)
      )
    )
  }
)

setMethod(
  "min_sample_size",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage, group = 1L, adoptr_DataDistribution, ...) {
    .check_stage(design_object, stage)
    if (typeof(group) == "double") {
      group <- as.integer(round(group))
      warning("'group' converted from double to integer.")
    }
    .check_type(group, "integer")
    .check_range(group, c(1L, 2L))
    if (group == 2L) {
      if (missing(adoptr_DataDistribution)) {
        stop(paste0("In 'adoptr', the number of groups in a design is encoded in the DataDistribution object. Please provide this object."))
      }
      .check_range(group, seq.int(length.out = n_groups(design_object, adoptr_DataDistribution)))
    }
    return(
      switch(stage,
             adoptr::n1(d = design_object),
             adoptr::n2(d = design_object, x1 = futility_boundary_z(design_object, stage = 1L) + .Machine$double.eps)
      )
    )
  }
)

# constant_sample_size for gsDesign, rpact, adoptr
setMethod(
  "constant_sample_size",
  signature(design_object = "gsDesign"),
  function(design_object, stage = 1L) {
    TRUE
  }
)
setMethod(
  "constant_sample_size",
  signature(design_object = "TrialDesignPlan"),
  function(design_object, stage = 1L) {
    TRUE
  }
)
setMethod(
  "constant_sample_size",
  signature(design_object = "TwoStageDesign"),
  function(design_object, stage = 1L) {
    if (stage == 1L){
      TRUE
    } else{
      FALSE
    }
  }
)
setMethod(
  "constant_sample_size",
  signature(design_object = "GroupSequentialDesign"),
  function(design_object, stage = 1L) {
    TRUE
  }
)
