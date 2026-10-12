# Interactive plots (ggplot2 + ggiraph).
#
# Ports of the legacy base-graphics plots. The study timeline and the DTP
# plot were the same swimmer plot, so they are one function here: without
# a pathway it is the study timeline; with one, a "Next patient" row shows
# the recommendation day by day. Titles belong to the cards around the
# plots, so the plots carry none.

theme_dtp <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = dtp_colours$ink),
      axis.text = ggplot2::element_text(colour = dtp_colours$muted),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.justification = "left",
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.85))
    )
}

status_shapes <- c(Completed = 16, Ongoing = 21, Withdrawn = 8, DLT = 4)
status_colours <- c(Completed = dtp_colours$muted,
                    Ongoing = dtp_colours$muted,
                    Withdrawn = dtp_colours$muted,
                    DLT = dtp_colours$dlt)

#' Patient follow-up segments for the timeline
#'
#' Follow-up counts the entry day as day 1, so a patient followed for `d`
#' days was last observed on `entry_day + d - 1`.
#'
#' @param data Normalised trial data, see [normalise_trial_data()].
#' @param day_of_assessment Study day of the analysis (T_A).
#' @param obswin DLT observation window, in days.
#' @return `data` with `last_day` (last day observed; the day of
#'   assessment for Ongoing patients), `window_end` (last day of the
#'   window; projected follow-up runs from `last_day` to it for Ongoing
#'   patients) and a `tooltip`.
timeline_data <- function(data, day_of_assessment, obswin) {
  data$last_day <- data$entry_day + data$days_followed - 1
  data$window_end <- data$entry_day + obswin - 1
  data$tooltip <- sprintf(
    "<b>Patient %d</b><br>Dose level %d, entered day %g<br>%s: %g of %g days (weight %.2f)",
    data$patient, data$dose, data$entry_day, data$status,
    data$days_followed, obswin, data$weight
  )
  data
}

#' Study timeline, with the dose transition pathway when given
#'
#' @param data Normalised trial data, see [normalise_trial_data()].
#' @param day_of_assessment Study day of the analysis (T_A).
#' @param obswin DLT observation window, in days.
#' @param num_doses Number of dose levels (fixes the dose colours).
#' @param pathway Optional `dtp_pathway` for the same data.
#' @return A ggplot with ggiraph interactive layers.
plot_timeline <- function(data, day_of_assessment, obswin, num_doses,
                          pathway = NULL) {
  d <- timeline_data(data, day_of_assessment, obswin)
  d$dose_f <- factor(d$dose, levels = seq_len(num_doses))
  d$status <- factor(d$status, levels = names(status_shapes))
  d$id <- paste0("patient-", d$patient)
  n <- nrow(d)
  ongoing <- d[d$status == "Ongoing" & d$window_end > d$last_day, ,
               drop = FALSE]
  present <- intersect(names(status_shapes), as.character(d$status))

  p <- ggplot2::ggplot(d, ggplot2::aes(y = patient)) +
    ggiraph::geom_segment_interactive(
      ggplot2::aes(x = entry_day, xend = last_day, yend = patient,
                   colour = dose_f, tooltip = tooltip, data_id = id),
      linewidth = 1.6, lineend = "round", key_glyph = "path"
    ) +
    ggiraph::geom_segment_interactive(
      data = ongoing,
      ggplot2::aes(x = last_day, xend = window_end, yend = patient,
                   colour = dose_f, tooltip = tooltip, data_id = id),
      linewidth = 1.1, linetype = "22", alpha = 0.7, show.legend = FALSE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(x = last_day, shape = status,
                   colour = I(status_colours[as.character(status)]),
                   tooltip = tooltip, data_id = id),
      size = 2.6, stroke = 1.2, fill = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = entry_day, label = paste0("D", dose),
                   colour = dose_f),
      hjust = 1, nudge_x = -0.6, size = 3, fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(xintercept = day_of_assessment, linetype = "dashed",
                        colour = dtp_colours$ink, linewidth = 0.4) +
    # Labels above the first patient: assessment day on the top line,
    # change days on the line below, so close days do not overlap.
    ggplot2::annotate("text", x = day_of_assessment, y = -0.45,
                      label = sprintf("Assessment, day %d", day_of_assessment),
                      hjust = -0.05, vjust = 0, size = 3,
                      colour = dtp_colours$ink) +
    # Colours fixed by dose level; the legend lists the doses given.
    ggplot2::scale_colour_manual(
      "Dose level",
      values = stats::setNames(dose_colours(num_doses),
                               seq_len(num_doses)),
      breaks = as.character(sort(unique(d$dose))),
      guide = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_shape_manual(
      "DLT follow-up", values = status_shapes, breaks = present,
      guide = ggplot2::guide_legend(
        order = 3, override.aes = list(colour = unname(status_colours[present]))
      )
    ) +
    ggplot2::labs(x = "Study day", y = NULL) +
    theme_dtp() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  labels <- paste0("P", d$patient)
  if (!is.null(pathway)) {
    p <- add_pathway_row(p, pathway, row = n + 1)
    labels <- c(labels, "Next")
  }
  p + ggplot2::scale_y_reverse(breaks = seq_along(labels), labels = labels,
                               expand = ggplot2::expansion(add = c(0.6, 1.3)))
}

add_pathway_row <- function(p, pathway, row) {
  # Each run covers its days up to the start of the next; the last one
  # holds on after it, so it is drawn a day past its last projected day.
  runs <- pathway$runs
  runs$start <- runs$from_day
  runs$end <- runs$to_day + 1
  # "Dose 2", not "D2": on this row D reads as de-escalate.
  runs$label <- ifelse(runs$decision == "Stop", "Stop",
                       ifelse(runs$end - runs$start >= 6,
                              paste("Dose", runs$dose), runs$dose))
  runs$tooltip <- sprintf(
    "<b>Days %d to %d</b><br>%s%s",
    runs$from_day, runs$to_day,
    vapply(runs$dose, describe_dose, character(1),
           current_dose = pathway$current_dose),
    ifelse(runs$from_day > pathway$day_of_assessment,
           "<br>if no new DLTs are observed", "")
  )
  runs$id <- paste0("run-", seq_len(nrow(runs)))
  runs$decision <- factor(runs$decision, levels = names(decision_labels))
  changes <- runs[-1, , drop = FALSE]
  wide <- runs[runs$end - runs$start >= 2, , drop = FALSE]
  narrow <- runs[runs$end - runs$start < 2, , drop = FALSE]

  p +
    ggiraph::geom_rect_interactive(
      data = runs,
      ggplot2::aes(xmin = start, xmax = end, ymin = row - 0.35,
                   ymax = row + 0.35, fill = decision, tooltip = tooltip,
                   data_id = id),
      inherit.aes = FALSE
    ) +
    # Label on the bar, in black or white for contrast with its fill;
    # one-day runs are too narrow, so their label sits just above.
    ggplot2::geom_text(
      data = wide,
      ggplot2::aes(x = (start + end) / 2, y = row, label = label),
      inherit.aes = FALSE, size = 3, fontface = "bold",
      colour = text_on(dtp_colours$decision[as.character(wide$decision)])
    ) +
    ggplot2::geom_text(
      data = narrow,
      ggplot2::aes(x = (start + end) / 2, y = row - 0.5, label = label),
      inherit.aes = FALSE, size = 2.6, fontface = "bold",
      colour = dtp_colours$ink
    ) +
    ggplot2::geom_vline(
      data = changes, ggplot2::aes(xintercept = start),
      colour = dtp_colours$decision[as.character(changes$decision)],
      linetype = "dashed", linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = changes,
      ggplot2::aes(x = start, y = 0.15, label = paste("Day", from_day)),
      inherit.aes = FALSE, hjust = -0.05, vjust = 0, size = 3,
      colour = dtp_colours$ink
    ) +
    ggplot2::scale_fill_manual(
      "Next patient", values = dtp_colours$decision,
      labels = decision_labels, drop = TRUE,
      guide = ggplot2::guide_legend(order = 1)
    )
}

#' Prior DLT probability quantiles of the empiric working model
#'
#' Under the empiric model `p_d = skeleton_d ^ exp(beta)` with
#' `beta ~ N(0, variance)`, the quantile used by escalation for the
#' posterior, applied to the prior.
#'
#' @param skeleton Prior DLT probabilities (the design's skeleton).
#' @param variance Prior variance of `beta`.
#' @param p Probability.
prior_prob_tox_quantile <- function(skeleton, variance, p) {
  skeleton^exp(stats::qnorm(1 - p, mean = 0, sd = sqrt(variance)))
}

#' Prior and posterior dose-toxicity curves at the key days
#'
#' One panel per [key_fits()] entry, each with the prior for reference.
#'
#' @param pathway A `dtp_pathway`.
#' @param skeleton Prior DLT probabilities (the design's skeleton).
#' @param variance Prior variance of the model parameter (the design's).
#' @param target Target DLT rate.
#' @param level Coverage of the credible intervals.
#' @return A data frame with `panel` (factor of panel headers, in time
#'   order), `curve`, `role` (`prior`, `current`, `change`, `complete`),
#'   `dose`, `prob_tox` (the skeleton for the prior), `lower`, `upper`,
#'   `recommended` (the design's recommendation, after its rules),
#'   `mtd_estimate` (posterior mean closest to target) and `tooltip`.
#'   Prior rows repeat in every panel.
dose_toxicity_data <- function(pathway, skeleton, variance, target,
                               level = 0.9) {
  # As model_table().
  stopifnot(is.numeric(level), length(level) == 1, level > 0, level < 1)
  tail_p <- (1 - level) / 2
  doses <- seq_along(skeleton)
  fits <- key_fits(pathway)
  roles <- c("current", rep("change", length(fits) - 1))
  if (pathway$complete_day != pathway$day_of_assessment) {
    roles[length(roles)] <- "complete"
  }
  prior_lower <- prior_prob_tox_quantile(skeleton, variance, tail_p)
  prior_upper <- prior_prob_tox_quantile(skeleton, variance, 1 - tail_p)
  ci <- function(lower, upper) {
    sprintf("%g%% CrI [%.3f, %.3f]", 100 * level, lower, upper)
  }

  out <- do.call(rbind, Map(function(k, role) {
    rec <- escalation::recommended_dose(k$fit)
    mean <- escalation::mean_prob_tox(k$fit)
    panel <- sprintf("%s\n%s", k$curve,
                     if (is.na(rec)) "Stop: lowest dose too toxic" else
                       sprintf("Recommended dose %d", rec))
    posterior <- data.frame(
      panel = panel, curve = k$curve, role = role, dose = doses,
      prob_tox = mean,
      lower = escalation::prob_tox_quantile(k$fit, tail_p),
      upper = escalation::prob_tox_quantile(k$fit, 1 - tail_p),
      recommended = !is.na(rec) & doses == rec,
      mtd_estimate = doses == which.min(abs(mean - target))
    )
    note <- ifelse(
      posterior$recommended, "<br><b>Recommended dose</b>",
      ifelse(posterior$mtd_estimate,
             "<br>MTD estimate (closest to target), not recommended by the design's rules",
             "")
    )
    posterior$tooltip <- sprintf(
      "<b>%s</b><br>Dose level %d: %.3f<br>%s%s", k$curve, doses, mean,
      ci(posterior$lower, posterior$upper), note
    )
    prior <- data.frame(
      panel = panel, curve = "Prior", role = "prior", dose = doses,
      prob_tox = skeleton, lower = prior_lower, upper = prior_upper,
      recommended = FALSE, mtd_estimate = FALSE,
      tooltip = sprintf("<b>Prior</b><br>Dose level %d: %.3f<br>%s", doses,
                        skeleton, ci(prior_lower, prior_upper))
    )
    rbind(prior, posterior)
  }, fits, roles))
  out$panel <- factor(out$panel, levels = unique(out$panel))
  out
}

# Legend glyph drawn only in the key whose linetype is `keep`, so each
# layer appears once in the shared linetype legend (`...` sets fixed
# aesthetics for the glyph, e.g. a neutral fill for per-panel colours).
key_only_for <- function(keep, glyph, ...) {
  fixed <- list(...)
  function(data, params, size) {
    if (!identical(data$linetype, keep)) {
      return(grid::nullGrob())
    }
    data[names(fixed)] <- fixed
    glyph(data, params, size)
  }
}

#' Dose-toxicity plot: posterior and prior curves, one panel per key day
#'
#' Each panel shows the posterior and the prior (grey, dashed) with their
#' credible intervals as shaded bands, and the target (dotted). Only the
#' recommended dose is a filled point; it is also named in the header.
#' Posterior colours differ by panel, so the legend draws them in ink.
#'
#' @param pathway A `dtp_pathway`.
#' @param skeleton Prior DLT probabilities (the design's skeleton).
#' @param variance Prior variance of the model parameter (the design's).
#' @param target Target DLT rate.
#' @param level Coverage of the credible intervals.
#' @return A ggplot with ggiraph interactive layers.
plot_dose_toxicity <- function(pathway, skeleton, variance, target,
                               level = 0.9) {
  d <- dose_toxicity_data(pathway, skeleton, variance, target, level)
  role_colour <- c(current = dtp_colours$accent, change = "#CC79A7",
                   complete = "#1F7A8C")
  prior <- d[d$role == "prior", , drop = FALSE]
  post <- d[d$role != "prior", , drop = FALSE]
  post$colour <- role_colour[post$role]
  post$id <- paste(post$panel, post$dose)
  rec <- post[post$recommended, , drop = FALSE]

  keys <- c(sprintf("Prior, %g%% CrI", 100 * level),
            sprintf("Posterior mean, %g%% CrI", 100 * level),
            sprintf("Target %g", target))
  key_lines <- stats::setNames(c("22", "solid", "dotted"), keys)
  prior_fill <- grDevices::adjustcolor(dtp_colours$muted, alpha.f = 0.12)
  post_alpha <- 0.2

  ggplot2::ggplot(mapping = ggplot2::aes(dose, prob_tox)) +
    ggplot2::geom_ribbon(
      data = prior,
      ggplot2::aes(ymin = lower, ymax = upper, group = panel,
                   linetype = keys[1]),
      fill = prior_fill, colour = NA,
      key_glyph = key_only_for("22", ggplot2::draw_key_rect)
    ) +
    ggplot2::geom_ribbon(
      data = post,
      ggplot2::aes(ymin = lower, ymax = upper, group = panel,
                   fill = I(colour), linetype = keys[2]),
      alpha = post_alpha, colour = NA,
      key_glyph = key_only_for("solid", ggplot2::draw_key_rect,
                               fill = dtp_colours$ink, alpha = post_alpha)
    ) +
    ggplot2::geom_hline(
      data = data.frame(target = target),
      ggplot2::aes(yintercept = target, linetype = keys[3]),
      colour = dtp_colours$ink, linewidth = 0.4,
      key_glyph = key_only_for("dotted", ggplot2::draw_key_path)
    ) +
    ggplot2::geom_line(
      data = prior, ggplot2::aes(group = panel, linetype = keys[1]),
      colour = dtp_colours$muted, linewidth = 0.6,
      key_glyph = key_only_for("22", ggplot2::draw_key_path)
    ) +
    ggiraph::geom_point_interactive(
      data = prior, ggplot2::aes(tooltip = tooltip),
      colour = dtp_colours$muted, fill = "white", shape = 21, size = 1.4,
      stroke = 0.8
    ) +
    ggplot2::geom_line(
      data = post,
      ggplot2::aes(group = panel, colour = I(colour), linetype = keys[2]),
      linewidth = 0.8,
      key_glyph = key_only_for("solid", ggplot2::draw_key_path,
                               colour = dtp_colours$ink)
    ) +
    ggiraph::geom_point_interactive(
      data = post[!post$recommended, , drop = FALSE],
      ggplot2::aes(colour = I(colour), tooltip = tooltip, data_id = id),
      fill = "white", shape = 21, size = 2, stroke = 1.1
    ) +
    ggiraph::geom_point_interactive(
      data = rec,
      ggplot2::aes(colour = I(colour), fill = I(colour),
                   shape = "Recommended dose", tooltip = tooltip,
                   data_id = id),
      size = 2.8, stroke = 1.1
    ) +
    ggplot2::facet_wrap(~panel, ncol = 3) +
    ggplot2::scale_linetype_manual(
      NULL, values = key_lines, breaks = keys,
      guide = ggplot2::guide_legend(order = 1)
    ) +
    # No recommended dose (stopped): no legend entry for it.
    (if (nrow(rec) > 0) ggplot2::scale_shape_manual(
      NULL, values = c(`Recommended dose` = 21),
      guide = ggplot2::guide_legend(
        order = 2,
        override.aes = list(colour = dtp_colours$ink,
                            fill = dtp_colours$ink)
      )
    )) +
    ggplot2::scale_x_continuous(breaks = seq_along(skeleton)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Dose level", y = "P(DLT)") +
    theme_dtp() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
      panel.spacing = ggplot2::unit(1.2, "lines"),
      legend.box = "horizontal",
      legend.key.width = ggplot2::unit(2.2, "lines")
    )
}

#' Height for a dose-toxicity plot, in inches: three panels per row
#'
#' @param pathway A `dtp_pathway`.
dose_toxicity_height <- function(pathway) {
  1.2 + 3 * ceiling(length(key_fits(pathway)) / 3)
}

#' Render a plot as an interactive ggiraph widget
#'
#' @param plot A ggplot from [plot_timeline()] or [plot_dose_toxicity()].
#' @param width,height Size of the SVG, in inches; it rescales to fit.
#' @return A `girafe` htmlwidget.
as_girafe <- function(plot, width = 8, height = 5) {
  ggiraph::girafe(
    ggobj = plot, width_svg = width, height_svg = height,
    options = list(
      ggiraph::opts_tooltip(
        css = paste0("background:#fff;color:", dtp_colours$ink,
                     ";border:1px solid ", dtp_colours$rule,
                     ";border-radius:4px;padding:6px 8px;",
                     "font-size:0.85rem;line-height:1.35;"),
        opacity = 1
      ),
      ggiraph::opts_hover(css = "stroke-width:3px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.35;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = TRUE, position = "topright"),
      ggiraph::opts_sizing(rescale = TRUE)
    )
  )
}

#' Height for a timeline, in inches, growing with the number of rows
#'
#' @param rows Patients, plus one for the "Next patient" row.
timeline_height <- function(rows) {
  2.2 + 0.32 * rows
}
