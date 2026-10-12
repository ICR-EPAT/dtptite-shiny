# Global coherence in escalation, as an escalation selector decorator.
#
# Do not recommend a dose above the current (last given) dose while the
# observed DLT rate at the current dose exceeds the target. Mirrors
# `global_coherent_esc` in dtpcrm::applied_titecrm(), which the legacy app
# used. Written to slot into an escalation/dtptite design chain, e.g.
#
#   get_dfcrm_tite(skeleton, target) |>
#     enforce_global_coherence() |>
#     apply_dtp(t_max, obswin)

#' Enforce global coherence in escalation
#'
#' @param parent_selector_factory A `selector_factory` object.
#' @param target DLT rate the observed rate is compared with. When `NULL`
#'   (default), the parent's `tox_target()` is used at fit time.
#' @return A `coherent_selector_factory` object.
enforce_global_coherence <- function(parent_selector_factory, target = NULL) {
  if (!is.null(target)) {
    stopifnot(is.numeric(target), length(target) == 1, target > 0, target < 1)
  }
  x <- list(parent = parent_selector_factory, target = target)
  class(x) <- c("coherent_selector_factory",
                "derived_dose_selector_factory",
                "selector_factory")
  x
}

fit.coherent_selector_factory <- function(selector_factory, outcomes, ...) {
  parent_selector <- selector_factory$parent |> escalation::fit(outcomes, ...)
  target <- selector_factory$target
  if (is.null(target)) target <- escalation::tox_target(parent_selector)
  x <- list(parent = parent_selector, target = target)
  class(x) <- c("coherent_selector", "derived_dose_selector", "selector")
  x
}

recommended_dose.coherent_selector <- function(x, ...) {
  rec <- escalation::recommended_dose(x$parent, ...)
  doses <- escalation::doses_given(x)
  if (is.na(rec) || length(doses) == 0) return(rec)

  current <- utils::tail(doses, 1)
  at_current <- doses == current
  observed_rate <- sum(escalation::tox(x)[at_current]) / sum(at_current)
  if (observed_rate > x$target && rec > current) {
    return(as.integer(current))
  }
  rec
}

# The app sources this file rather than installing it as a package, so
# register the methods with escalation's generics explicitly; dispatch from
# inside escalation and dtptite (e.g. DTP projection refits) then finds them.
registerS3method("fit", "coherent_selector_factory",
                 fit.coherent_selector_factory,
                 envir = asNamespace("escalation"))
registerS3method("recommended_dose", "coherent_selector",
                 recommended_dose.coherent_selector,
                 envir = asNamespace("escalation"))
