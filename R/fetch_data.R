#' @title fetch_match
#'
#' @description
#' MISSING MISSING
#'
#' @param league [\code{character(1L)}]\cr
#'   Shortcut specifying the desired league (e.g. bl1 for the German Bundesliga).
#'   See \url{https://www.openligadb.de/Datenhaushalt/}.
#' @param season [\code{integer(1L)}]\cr
#'   E.g. 2016
#' @param group_id [\code{integer(1L)}]\cr
#'   E.g. 8 for the 8th matchday
#' @param match_id [\code{integer(1L)}]\cr
#'   Optional; If not specified returns all matches
#' @return A nested list of data frames
#' @export

fetch_match <- function(league, season, group_id, match_id) {

  # check <- !missing(match_id) & (!missing(league) | !missing(season) | !missing(group_id))

  if (!missing(match_id)) {

    url <- do.call(build_query, list("getmatchdata", match_id))
    result <- query_openligadb(url)
    return(parse_match(result))

  } else {

    if (!missing(group_id) & (missing(season) | missing(league))) {
      stop("Arguments league & season are mandatory if group_id is specified.")
    }
    if (!missing(season) & missing(league)) {
      stop("Argument league is mandatory if season is specified.")
    }

    args <- as.list(match.call())[-1]
    url <- do.call(build_query, args = c("getmatchdata", args))

    raw <- query_openligadb(url)
    result <- purrr::map(raw, ~ parse_match(.x))
    list(matches = purrr::map_df(result, ~ data.frame(.x$match, stringsAsFactors = FALSE)),
         goals = purrr::map_df(result, ~ data.frame(.x$goals, stringsAsFactors = FALSE)))

  }

}

#' @describeIn fetch_match Get team data, returns a data.frame
#' @export
fetch_teams <- function(league, season) {
  url <- build_query("getavailableteams", league, season)
  result <- query_openligadb(url, simplify2dataframe = TRUE)
  colnames(result) <- c("team_id", "team", "team_short", "team_icon_url",
                        "team_group_name")
  result
}

#' @describeIn fetch_match Get goal scorer data, returns a data.frame
#' @export
fetch_goal_getters <- function(league, season) {
  url <- build_query("getgoalgetters", league, season)
  result <- query_openligadb(url, simplify2dataframe = TRUE)
  colnames(result) <- c("goal_getter_id", "goal_getter", "goal_count")
  result$goal_getter_name <- stringi::stri_trim(result$goal_getter)
  result
}

#' @describeIn fetch_match Get table, returns a data.frame
#' @export
fetch_current_table <- function(league, season) {
  url <- build_query("getbltable", league, season)
  result <- query_openligadb(url)
  data.frame(
    team_id = purrr::map_int(result, "TeamInfoId"),
    team = purrr::map_chr(result, "TeamName"),
    points = purrr::map_int(result, "Points"),
    opponent_goals = purrr::map_int(result, "OpponentGoals"),
    goals = purrr::map_int(result, "Goals"),
    matches = purrr::map_int(result, "Matches"),
    won = purrr::map_int(result, "Won"),
    lost = purrr::map_int(result, "Lost"),
    draw = purrr::map_int(result, "Draw"),
    goal_diff = purrr::map_int(result, "GoalDiff"),
    stringsAsFactors = FALSE)
}

#' @describeIn fetch_match Show available groups
#' @export
fetch_available_groups <- function(league, season) {
  url <- build_query("getavailablegroups", league, season)
  result <- query_openligadb(url, simplify2dataframe = TRUE)
  colnames(result) <- c("group", "group_order_id", "group_id")
  result
}

#' @describeIn fetch_match Get most recent group
#' @export
fetch_current_group <- function(league) {
  url <- build_query("getcurrentgroup", league)
  result <- data.frame(query_openligadb(url), stringsAsFactors = FALSE)
  colnames(result) <- c("group", "group_order_id", "group_id")
  result
}

#' @title fetch_last_update_time
#'
#' @description
#' Returns date and time when data was updated to avoid unnecessary api calls
#'
#' @param league [\code{character(1L)}]\cr
#'   Shortcut specifying the desired league (e.g. bl1 for the German Bundesliga).
#'   See \url{https://www.openligadb.de/Datenhaushalt/}.
#' @param season [\code{integer(1L)}]\cr
#'   E.g. 2016
#' @param group_id [\code{integer(1L)}]\cr
#'   E.g. 8 for the 8th matchday
#'
#' @return an object of class POSIXct
#' @export

fetch_last_update_time <- function(league, season, group_id) {
  url <- build_query("getlastchangedate", league, season, group_id)
  lubridate::as_datetime(query_openligadb(url))
}
