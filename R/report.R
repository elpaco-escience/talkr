#' Report corpus-level and conversation-level statistics
#'
#' Basic conversation statistics are reported to the console:
#' - Corpus-level statistics, reporting on the dataset as a whole;
#' - Conversation-level statistics, reporting per source.
#'
#' The input for this function must be a `talkr` dataset, containing
#' the columns `source`, `participant`, `begin`, and `end`. Time stamps in the
#' columns `begin` and `end` must be in milliseconds.
#' To easily transform a dataset to a `talkr` dataset, consult `talkr::init()`.
#'
#' @param data talkr dataset
#'
#' @export
report_stats <- function(data) {
  summary <- report(data)

  # combined
  combined <- report_combined(summary)
  print_single_line_table(header = "Corpus-level statistics", table = combined)

  # basics
  basics <- report_basics(summary)
  print_summary(header = "Conversation-level statistics (per source)", table = basics)

  # turns
  turns <- report_turns(summary)
  print_summary(header = "Turn statistics (per source)", table = turns)

}

report <- function(data) {
  # check if data is a talkr object
  check_talkr(data)

  # report summary
  summary <- data |>
    dplyr::mutate(XXX_turn_duration = .data$end - .data$begin) |>
    dplyr::group_by(.data$source) |>
    dplyr::summarize(start=min(.data$begin, na.rm=TRUE),
                     end=max(.data$end, na.rm=TRUE),
                     n_turns=dplyr::n(),
                     shortest_turn=min(.data$XXX_turn_duration, na.rm=TRUE),
                     longest_turn=max(.data$XXX_turn_duration, na.rm=TRUE),
                     avg_turn=mean(.data$XXX_turn_duration, na.rm=TRUE),
                     n_participants=dplyr::n_distinct(.data$participant),
                     totaltime = .data$end - .data$start,
                     total_speaking = sum(.data$XXX_turn_duration, na.rm=TRUE)
    )
  return(summary)
}


report_combined <- function(summary) {
  combined <- summary |>
    dplyr::summarize(`nr of sources` = dplyr::n(),
                     `nr of participants` = sum(.data$n_participants, na.rm=TRUE),
                     `nr of turns` = sum(.data$n_turns, na.rm=TRUE),
                     `mean turn duration (ms)` = round(sum(.data$total_speaking, na.rm=TRUE) / sum(.data$n_turns, na.rm=TRUE), 0),
                     `turns per hour` = round(sum(.data$n_turns, na.rm=TRUE) / (sum(.data$totaltime, na.rm=TRUE) / (60 * 60 * 1000)), 0),
                     `total recording (min)` = round((sum(.data$totaltime, na.rm=TRUE) / (60 * 1000)), 1),
                     `total recording (hours)` = round(sum(.data$totaltime, na.rm=TRUE) / (60 * 60 * 1000), 2),
                     `total speaking time (min)` = round(sum(.data$total_speaking, na.rm=TRUE) / (60 * 1000), 1),
                     `total speaking time (hours)` = round(sum(.data$total_speaking, na.rm=TRUE) / (60 * 60 * 1000), 2)
    )
}

report_basics <- function(summary) {
  basics <- data.frame(source = summary$source)
  basics$`nr of participants` = summary$n_participants
  basics$`total recording (ms)` = summary$totaltime
  basics$`total recording (min)` = round(summary$totaltime / (60 * 1000), 1)
  basics$`total speaking time (ms)` = summary$total_speaking
  basics$`total speaking time (min)` = round(summary$total_speaking / (60 * 1000), 1)
  return(basics)
}

report_turns <- function(summary) {
  turns <- data.frame(source = summary$source)
  turns$`nr of turns` = summary$n_turns
  turns$`mean turn duration (ms)` = round(summary$avg_turn, 0)
  turns$`shortest turn (ms)` = summary$shortest_turn
  turns$`longest turn (ms)` = summary$longest_turn
  turns$`turns per hour` = round(summary$n_turns / (summary$totaltime / (60 * 60 * 1000)), 0)
  return(turns)
}


print_summary <- function(header, table){
  cat(paste("###", header))
  print(knitr::kable(table))
  cat("\n")
}

print_single_line_table <- function(header, table){
  cat(paste("###", header, "\n\n"))
  for (n in names(table)){
    item <- table[1,n]
    cat(paste(n, item, sep = ": "))
    cat("\n")
  }
  cat("\n")
}

