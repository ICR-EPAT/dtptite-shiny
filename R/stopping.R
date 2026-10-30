# Safety stopping at the lowest dose, as an escalation selector decorator.
#
# Stop the trial when, under a beta-binomial model, the posterior
# probability that the DLT rate at the lowest dose exceeds the target is
# greater than the confidence level, once enough patients have had that
# dose. Only the lowest dose is monitored: TITE-CRM does not eliminate
# doses. Mirrors the stopping rule the legacy app passed to
# dtpcrm::applied_titecrm(), with the Beta prior as given.
#
# dtptite 0.1.0's stop_for_beta_binomial_toxicity(dose = 1) is not used:
# besides stopping, it rules out any dose that fails the same check, and
# every dose above it.

#' Stop when the lowest dose is too toxic
#'
#' @param parent_selector_factory A `selector_factory` object.
#' @param target Target DLT rate.
#' @param confidence Stop when P(DLT rate at dose 1 > target | data) is
#'   greater than this.
#' @param a,b Beta prior for the DLT rate at dose 1.
#' @param n_min Patients needed at dose 1 before the rule applies.
#' @return A `lowest_dose_stop_selector_factory` object.
stop_for_lowest_dose_toxicity <- function(parent_selector_factory, target,
                                          confidence, a = target,
                                          b = 1 - target, n_min = 1) {
  stopifnot(is.numeric(target), length(target) == 1, target > 0, target < 1,
            is.numeric(confidence), length(confidence) == 1,
            confidence > 0, confidence < 1,
            is.numeric(a), length(a) == 1, a > 0,
            is.numeric(b), length(b) == 1, b > 0,
            is.numeric(n_min), length(n_min) == 1, n_min >= 0)
  x <- list(parent = parent_selector_factory, target = target,
            confidence = confidence, a = a, b = b, n_min = n_min)
  class(x) <- c("lowest_dose_stop_selector_factory",
                "derived_dose_selector_factory",
                "selector_factory")
  x
}

fit.lowest_dose_stop_selector_factory <- function(selector_factory, outcomes,
                                                  ...) {
  parent_selector <- selector_factory$parent |> escalation::fit(outcomes, ...)
  x <- c(list(parent = parent_selector),
         selector_factory[c("target", "confidence", "a", "b", "n_min")])
  class(x) <- c("lowest_dose_stop_selector", "derived_dose_selector",
                "selector")
  x
}

#' Is the lowest dose too toxic?
#'
#' @param x A fitted `lowest_dose_stop_selector`.
#' @return `TRUE` or `FALSE`.
lowest_dose_too_toxic <- function(x) {
  at_lowest <- escalation::doses_given(x) == 1
  n <- sum(at_lowest)
  if (n == 0 || n < x$n_min) return(FALSE)
  n_tox <- sum(escalation::tox(x)[at_lowest])
  stats::pbeta(x$target, x$a + n_tox, x$b + n - n_tox,
               lower.tail = FALSE) > x$confidence
}

recommended_dose.lowest_dose_stop_selector <- function(x, ...) {
  if (lowest_dose_too_toxic(x)) return(NA)
  escalation::recommended_dose(x$parent, ...)
}

continue.lowest_dose_stop_selector <- function(x, ...) {
  !lowest_dose_too_toxic(x) && escalation::continue(x$parent, ...)
}

# The app sources this file rather than installing it as a package, so
# register the methods with escalation's generics explicitly, as in
# coherence.R.
registerS3method("fit", "lowest_dose_stop_selector_factory",
                 fit.lowest_dose_stop_selector_factory,
                 envir = asNamespace("escalation"))
registerS3method("recommended_dose", "lowest_dose_stop_selector",
                 recommended_dose.lowest_dose_stop_selector,
                 envir = asNamespace("escalation"))
registerS3method("continue", "lowest_dose_stop_selector",
                 continue.lowest_dose_stop_selector,
                 envir = asNamespace("escalation"))
