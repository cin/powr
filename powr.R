library("rjson")

load <- function() {
  url <- "http://games.espn.go.com/ffl/api/v2/teams?leagueId=709331&seasonId=2013"
  res <- readLines(url, warn = "F")
  fromJSON(res)
}

# wlt <- list(0, 0, 0)

# addWLT <- function (isThisTeamHome, mup) {
#   if (isThisTeamHome) {
#     if (mup$homeTeamScores > mup$awayTeamScores) wlt[[1]] = wlt[[1]] + 1
#     else if (mup$homeTeamScores == mup$awayTeamScores) wlt[[3]] = wlt[[3]] + 1
#     else wlt[[2]] = wlt[[2]] + 1
#   } else {
#     if (mup$homeTeamScores < mup$awayTeamScores) wlt[[1]] = wlt[[1]] + 1
#     else if (mup$homeTeamScores == mup$awayTeamScores) wlt[[3]] = wlt[[3]] + 1
#     else wlt[[2]] = wlt[[2]] + 1
#   }
# }

foo <- function(js, numGames) {
  wtf <- matrix()
  for (team in js$teams) {
    ts <- 0
    wlt <- list(0, 0, 0)
    i <- 1
    thisTeamsScores <- list()
    oppScores <- list()
    while (i <= numGames) {
      mup <- team$scheduleItems[[i]]$matchups[[1]]
      isThisTeamHome <- mup$homeTeamId == team$teamId
#       addWLT(isThisTeamHome, mup)
      if (isThisTeamHome) {
        if (mup$homeTeamScores > mup$awayTeamScores) wlt[[1]] = wlt[[1]] + 1
        else if (mup$homeTeamScores == mup$awayTeamScores) wlt[[3]] = wlt[[3]] + 1
        else wlt[[2]] = wlt[[2]] + 1
      } else {
        if (mup$homeTeamScores < mup$awayTeamScores) wlt[[1]] = wlt[[1]] + 1
        else if (mup$homeTeamScores == mup$awayTeamScores) wlt[[3]] = wlt[[3]] + 1
        else wlt[[2]] = wlt[[2]] + 1
      }
      thisTeamsScore <- if (isThisTeamHome) mup$homeTeamScores else mup$awayTeamScores
      oppScore <- if (isThisTeamHome) mup$awayTeamScores else mup$homeTeamScores
      oppScores = c(unlist(oppScores), oppScore)
      thisTeamsScores = c(unlist(thisTeamsScores), thisTeamsScore)
      i = i + 1
    }
    tw <- wlt[[1]]
    wp <- tw / numGames
    mm <- unlist(range(thisTeamsScores))
    tp <- sum(thisTeamsScores)
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
