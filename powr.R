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

# getGameResult <- function(matchup) {
#   
# }

powr <- function(js, numGames) {
  wtf <- matrix()
  for (team in js$teams) {
    ts <- 0
    wlt <- list(0, 0, 0)
    scores <- list()
    oppScores <- list()
    for (i in 1:numGames) {
      matchup <- team$scheduleItems[[i]]$matchups[[1]]
      isHome <- matchup$homeTeamId == team$teamId
      gmRes <- wasWLT(isHome, matchup)
      wlt[[gmRes]] <- wlt[[gmRes]] + 1
      score <- if (isHome) matchup$homeTeamScores else matchup$awayTeamScores
      scores <- c(unlist(scores), score)
      oppScore <- if (isHome) matchup$awayTeamScores else matchup$homeTeamScores
      oppScores <- c(unlist(oppScores), oppScore)
    }
    tw <- wlt[[1]]
    wp <- tw / numGames
    mm <- unlist(range(scores))
    tp <- sum(scores)
    tpa <- sum(oppScores)
    avgScore <- tp / numGames
    avgOppScore <- tpa / numGames
    powerRanking <- ((avgScore * 6) + ((mm[[1]] + mm[[2]])*2) + (wp * 400))/10
    vv <- c(substr(team$teamAbbrev, 1, 5), tp, tpa, unlist(mm), wp, unlist(wlt), avgScore, avgOppScore, powerRanking)
#     wtf <- rbind(wtf, vv)
    out <- sprintf("%15s, %7.2f, %7.2f, %7.2f, %7.2f, %5.2f, %2d - %2d - %2d, %7.2f, %7.2f, %7.2f", 
      substr(team$teamAbbrev, 1, 5), 
      tp, tpa,
      mm[[1]], mm[[2]], 
      wp, 
      wlt[[1]], wlt[[2]], wlt[[3]],
      avgScore, avgOppScore,
      powerRanking)
    print(out)
  }
}
