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
#' has 4.5:1 contrast on white, so it reads as text (the "D1" labels).
#'
#' @param num_doses Number of dose levels.
#' @return A character vector of `num_doses` colours.
dose_colours <- function(num_doses) {
  grDevices::colorRampPalette(c("#7D68C4", "#2E1A66"))(num_doses)
}

#' WCAG contrast ratio between two colours
#'
#' @param a,b Colours.
#' @return The ratio, from 1 to 21.
contrast_ratio <- function(a, b) {
  luminance <- function(colour) {
    rgb <- grDevices::col2rgb(colour)[, 1] / 255
    rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * rgb)
  }
  l <- sort(c(luminance(a), luminance(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

#' Black or white, whichever contrasts more with each fill
#'
#' @param fill Colours.
#' @return `"#000000"` or `"#FFFFFF"` for each.
text_on <- function(fill) {
  vapply(fill, function(f) {
    if (contrast_ratio(f, "#FFFFFF") >= contrast_ratio(f, "#000000")) {
      "#FFFFFF"
    } else {
      "#000000"
    }
  }, character(1), USE.NAMES = FALSE)
}
