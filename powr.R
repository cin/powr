library("rjson")

load <- function() {
  url <- "http://games.espn.go.com/ffl/api/v2/teams?leagueId=709331&seasonId=2013"
  res <- readLines(url, warn = "F")
  fromJSON(res)
}

wasWLT <- function (isHome, matchup) {
  x <- if (isHome) c(matchup$homeTeamScores, matchup$awayTeamScores) else c(matchup$awayTeamScores, matchup$homeTeamScores)
  if (x[[1]] > x[[2]]) 1
  else if (x[[1]] == x[[2]]) 3
  else 2
}

getGameResult <- function(team, matchup) {
  isHome <- matchup$homeTeamId == team$teamId
  gmRes <- wasWLT(isHome, matchup)
  score <- if (isHome) matchup$homeTeamScores else matchup$awayTeamScores
  oppScore <- if (isHome) matchup$awayTeamScores else matchup$homeTeamScores
  c(gmRes, score, oppScore)
}

powr <- function(js, numGames) {
  powr0(js, numGames)
}

powrSeason <- function() {
  js <- load()
  for (i in 1:16) powr(js, i)
}

calcPowr <- function(numGames, team, scores, oppScores, wlt) {
  tw <- wlt[[1]]
  wp <- tw / numGames
  mm <- unlist(range(scores))
  tp <- sum(scores)
  tpa <- sum(oppScores)
  avgScore <- tp / numGames
  avgOppScore <- tpa / numGames
  minScore <- mm[[1]]
  maxScore <- mm[[2]]
  powerRanking <- ((avgScore * 6) + ((minScore + maxScore) * 2) + (wp * 400))/10
  wins <- wlt[[1]]
  losses <- wlt[[2]]
  ties <- wlt[[3]]
  data.frame(team$teamAbbrev, team$teamId, tp, tpa, minScore, maxScore, wp, wins, losses, ties, avgScore, avgOppScore, powerRanking)
}

printPowr <- function(r) {
  out <- sprintf("%15s, %2d, %7.2f, %7.2f, %7.2f, %7.2f, %5.2f, %2d - %2d - %2d, %7.2f, %7.2f, %7.2f", 
    substr(r[[1]][[1]], 1, 5), r[[2]], r[[3]], r[[4]], r[[5]], r[[6]], r[[7]], r[[8]], r[[9]], r[[10]], r[[11]], r[[12]], r[[13]])
  print(out)
}

powr0 <- function(js, numGames) {
  wtf <- data.frame(check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  for (team in js$teams) {
    ts <- 0
    wlt <- list(0, 0, 0)
    scores <- list()
    oppScores <- list()
    for (i in 1:numGames) {
      matchup <- team$scheduleItems[[i]]$matchups[[1]]
      res <- getGameResult(team, matchup)
      wlt[[res[[1]]]] <- wlt[[res[[1]]]] + 1
      scores <- c(unlist(scores), res[[2]])
      oppScores <- c(unlist(oppScores), res[[3]])
    }
    pr <- calcPowr(numGames, team, scores, oppScores, wlt)
    wtf <- rbind(wtf, pr)
#     printPowr(pr)
  }
  print(wtf)
}
