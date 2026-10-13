no_stan <- function(...) {
  stop("Stan models (trialr) are not available in the browser.",
       call. = FALSE)
}
stan_crm <- no_stan
stan_efftox <- no_stan
stan_nbg <- no_stan
