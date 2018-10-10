#' @title build_query
#'
#' @description
#' Internal function to construct urls for the api calls
#'
#' @param ...
#'
#' @return character(1L)
#' @export
build_query <- function(...) {
  base_url <- "https://www.openligadb.de/"
  arguments <- as.list(match.call())[-1]

  # notwendig wenn objekte übergeben werden.
  symbols <- sapply(arguments, is.symbol)

  if (any(symbols)) {
    arguments[symbols] <- lapply(arguments[symbols], eval, envir = parent.frame())
  }

  paster <- function(x, y) {paste(x, y, sep = "/")}

  path <- Reduce(paster, x = c("api", arguments))
  httr::modify_url(base_url, path = path)
}

#' @title query_openligadb
#'
#' @description
#' MISSING
#'
#' @param ...
#'
#' @return list
#' @export

query_openligadb <- function(url, simplify2dataframe = FALSE) {
  tryCatch(
    jsonlite::fromJSON(url, simplifyDataFrame = simplify2dataframe),
    error = function(e) { warning(paste0("Problem fetching ", url)); NA }
  )
}

#' @title parse_match
#'
#' @description
#' MISSING
#'
#' @param x [\code{list(2L)}]\cr
#'
#' @return list of two data.frames (matches & goals)
#' @export

parse_match <- function(x) {

  results <- purrr::map_df(x$MatchResults, ~ data.frame(.x, stringsAsFactors = FALSE))
  colnames(results) <- c("result_id", "result_time", "score1", "score2",
                         "result_order_id", "result_type_id", "result_description")
  results <- results[order(results$result_order_id), ]
  results <- data.frame(score1_ht = results$score1[[1]],
                        score2_ht = results$score2[[1]],
                        score1 = results$score1[[2]],
                        score2 = results$score2[[2]],
                        stringsAsFactors = FALSE)

  #group <- x$Group
  match <- data.frame(
    league_id = x$LeagueId,
    league = x$LeagueName,
    match_id = x$MatchID,
    group_id = x$Group$GroupID,
    group = x$Group$GroupName,
    date =  lubridate::as_datetime(x$MatchDateTime, tz = "Europe/Berlin"),
    team1_id = x$Team1$TeamId,
    team2_id = x$Team2$TeamId,
    team1 = x$Team1$TeamName,
    team2 = x$Team2$TeamName,
    location_id = ifelse(is.null(x$Location$LocationID), NA, x$Location$LocationID),
    location_city = ifelse(is.null(x$Location$LocationCity), NA, x$Location$LocationCity),
    viewer_count = ifelse(is.null(x$NumberOfViewers), NA, x$NumberOfViewers),
    last_update_time =  lubridate::as_datetime(x$LastUpdateDateTime, tz = "Europe/Berlin"),
    stringsAsFactors = FALSE
  )

  match <- cbind(match[ , 1:10], results, match[ , 11:ncol(match)])
  goals <- x$Goals
  if (length(goals) == 0) {
    goals <- data.frame(
      match_id = match$match_id,
      goal_id = NA,
      score1 = NA,
      score2 = NA,
      minute = NA,
      goal_getter_id = NA,
      goal_getter = NA,
      penalty = NA,
      own_goal = NA,
      overtime = NA,
      comment = NA,
      stringsAsFactors = FALSE
    )
  } else {
    goals <- data.frame(
      match_id = match$match_id,
      goal_id = purrr::map_int(goals, "GoalID", .default = NA_integer_),
      score1 = purrr::map_int(goals, "ScoreTeam1", .default = NA_integer_),
      score2 = purrr::map_int(goals, "ScoreTeam2", .default = NA_integer_),
      minute = purrr::map_int(goals, "MatchMinute", .default = NA_integer_),
      goal_getter_id = purrr::map_int(goals, "GoalGetterID", .default = NA_integer_),
      goal_getter = purrr::map_chr(goals, "GoalGetterName", .default = NA_character_),
      penalty = purrr::map_lgl(goals, "IsPenalty", .default = NA),
      own_goal = purrr::map_lgl(goals, "IsOwnGoal", .default = NA),
      overtime = purrr::map_lgl(goals, "IsOvertime", .default = NA),
      # comment = map_chr(goals, "Comment"),
      stringsAsFactors = FALSE
    )
  }

  list(match = match, goals = goals)
}