# Colours shared by the bslib theme and the plots.
#
# Neutral theme with the ICR logo: navy accent, the logo's grey for
# secondary text, Okabe-Ito colours for decisions (distinguishable with
# colour-vision deficiency; legacy used a red/orange/green traffic light).

dtp_colours <- list(
  ink = "#212529",
  muted = "#616466",
  rule = "#CED4DA",
  accent = "#1F4E79",
  dlt = "#D55E00",
  decision = c(D = "#0072B2", S = "#E69F00", E = "#009E73",
               Stop = "#D55E00")
)

decision_labels <- c(D = "De-escalate", S = "Stay", E = "Escalate",
                     Stop = "Stop")

#' Sequential colours for dose levels, light to dark
#'
#' Purples keep dose levels apart from the decision colours; the lightest
#' still reads as text on white.
#'
#' @param num_doses Number of dose levels.
#' @return A character vector of `num_doses` colours.
dose_colours <- function(num_doses) {
  grDevices::colorRampPalette(c("#A08CD8", "#2E1A66"))(num_doses)
}
