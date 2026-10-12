# The editable patient grid: trial data <-> rhandsontable frame.
#
# The grid shows patients sorted by entry day (P1, P2, ... as in the plots)
# with derived columns: days followed for Ongoing and Completed patients
# (read-only) and the TITE weight.

grid_columns <- c("Dose", "Entry day", "Status", "Days followed", "Weight")

#' Canonical trial data: typed columns, sorted by entry day
#'
#' Days followed are kept only where they are entered (DLT, Withdrawn).
#'
#' @param trial A data.frame with `dose`, `entry_day`, `status` and
#'   optionally `days_followed`.
#' @return The same patients with numeric `dose`, `entry_day`,
#'   `days_followed` and character `status`, sorted by entry day (missing
#'   last, ties in input order).
canonical_trial <- function(trial) {
  days <- if (is.null(trial$days_followed)) NA_real_ else
    suppressWarnings(as.numeric(trial$days_followed))
  out <- data.frame(
    dose = suppressWarnings(as.numeric(as.character(trial$dose))),
    entry_day = suppressWarnings(as.numeric(trial$entry_day)),
    status = as.character(trial$status),
    days_followed = days
  )
  out$days_followed[!out$status %in% c("DLT", "Withdrawn")] <- NA_real_
  out <- out[order(out$entry_day, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Grid frame shown to the user
#'
#' @param trial Canonical trial data, see [canonical_trial()].
#' @inheritParams normalise_trial_data
#' @return A data.frame with [grid_columns]; days followed are derived for
#'   Ongoing and Completed patients, and the weight is `NA` when the row is
#'   incomplete or out of range.
grid_frame <- function(trial, day_of_assessment, obswin) {
  derived <- tryCatch(
    normalise_trial_data(trial, day_of_assessment, obswin),
    error = function(e) NULL
  )
  days <- trial$days_followed
  weight <- rep(NA_real_, nrow(trial))
  if (!is.null(derived) && nrow(derived) == nrow(trial)) {
    days <- derived$days_followed
    ok <- !is.na(derived$weight) & derived$weight >= 0 & derived$weight <= 1
    weight[ok] <- derived$weight[ok]
  }
  out <- data.frame(
    as.character(trial$dose), trial$entry_day, trial$status, days, weight
  )
  names(out) <- grid_columns
  out
}

#' Trial data from the edited grid
#'
#' @param grid A data.frame with [grid_columns], as from
#'   [rhandsontable::hot_to_r()].
#' @return Canonical trial data. A patient switched to DLT or Withdrawn
#'   keeps the days followed the grid showed for them.
trial_from_grid <- function(grid) {
  canonical_trial(data.frame(
    dose = grid[["Dose"]],
    entry_day = grid[["Entry day"]],
    status = grid[["Status"]],
    days_followed = grid[["Days followed"]]
  ))
}

#' A new grid row: next patient at the last dose, entering on T_A
#'
#' @param trial Canonical trial data.
#' @inheritParams normalise_trial_data
add_patient <- function(trial, day_of_assessment) {
  last_dose <- utils::tail(trial$dose[!is.na(trial$dose)], 1)
  canonical_trial(rbind(trial, data.frame(
    dose = if (length(last_dose)) last_dose else 1,
    entry_day = day_of_assessment, status = "Ongoing",
    days_followed = NA_real_
  )))
}

#' Editable rhandsontable for the grid
#'
#' @param frame A [grid_frame()].
#' @param num_doses Number of dose levels (dropdown choices).
render_grid <- function(frame, num_doses) {
  derived_rows <- which(frame$Status %in% c("Ongoing", "Completed"))
  hot <- rhandsontable::rhandsontable(
    frame, rowHeaders = paste0("P", seq_len(nrow(frame))),
    stretchH = "all", height = min(80 + 28 * nrow(frame), 420)
  ) |>
    rhandsontable::hot_col("Dose", type = "dropdown",
                           source = as.character(seq_len(num_doses)),
                           strict = TRUE) |>
    rhandsontable::hot_col("Status", type = "dropdown",
                           source = trial_statuses, strict = TRUE) |>
    rhandsontable::hot_col(c("Entry day", "Days followed"), format = "0") |>
    rhandsontable::hot_col("Weight", readOnly = TRUE, format = "0.00") |>
    rhandsontable::hot_cols(renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.getRenderer(cellProperties.type || 'text')
          .apply(this, arguments);
        if (cellProperties.readOnly) {
          td.style.background = '#F8F9FA';
          td.style.color = '#616466';
        }
      }") |>
    rhandsontable::hot_context_menu(allowRowEdit = TRUE,
                                    allowColEdit = FALSE)
  for (row in derived_rows) {
    hot <- rhandsontable::hot_cell(hot, row, "Days followed",
                                   readOnly = TRUE)
  }
  hot
}
