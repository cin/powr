library("rjson")
library("RColorBrewer")

load <- function(url) {
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

powr <- function(js, week) {
  powr0(js, week)
}

powrSeason <- function(js, numGames = 16) {
  myprks <- data.frame()
  for (week in 1:numGames) {
    print(sprintf("Week %d", week))
    prks <- powr(js, week)
    print(prks[with(prks, order(-powerRanking)),])
    cols <- prks[["powerRanking"]]
    myprks <- rbind(myprks, cols)
    if (week == 1) colnames(myprks) <- prks[["team.teamAbbrev"]]
  }
  myprks
}

plotPowr <- function(prks) {
  numWeeks <- nrow(prks)
  rng <- range(prks[3:numWeeks,])
  shps <- as.integer(runif(10, 21, 25))
  clrs <- brewer.pal(ncol(prks), "Paired")
  plot(prks[3:numWeeks,1], type = "o", col = clrs[[1]], pch = shps[[1]], axes = FALSE, ann = FALSE, ylim = rng)
  title(main = "Power Rankings by Week")
  title(xlab = "Week")
  title(ylab = "Power Ranking")
  axis(1, at = 1:14, labels = 3:16)
  axis(2)
  box()
  for (p in 2:10) lines(prks[3:numWeeks,p], type = "o", col = clrs[[p]], pch = shps[[p]])
  legend(1.9, 135, colnames(prks), col = clrs, cex = 0.6, pch = shps, lty = 1:2)
}

calcPowr <- function(week, team, scores, oppScores, wlt) {
  tw <- wlt[[1]]
  wp <- tw / week
  mm <- unlist(range(scores))
  tpf <- sum(scores)
  tpa <- sum(oppScores)
  avgScore <- tpf / week
  avgOppScore <- tpa / week
  minScore <- mm[[1]]
  maxScore <- mm[[2]]
  powerRanking <- ((avgScore * 6) + ((minScore + maxScore) * 2) + (wp * 400))/10
  wins <- wlt[[1]]
  losses <- wlt[[2]]
  ties <- wlt[[3]]
  pf <- tail(scores, n = 1)
  pa <- tail(oppScores, n = 1)
  data.frame(team$teamAbbrev, team$teamId, pf, pa, tpf, tpa, minScore, maxScore, wp, wins, losses, ties, avgScore, avgOppScore, powerRanking)
}

powr0 <- function(js, numGames) {
  powerRankings <- data.frame()
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
    powerRankings <- rbind(powerRankings, pr)
  }
  powerRankings
}

breakdown <- function(prks) {
  mypf <- data.frame()
  for (i in 1:length(prks) + 1) {
    print(sprintf("Week %d", i))
    print(prks[with(prks, order(-pf)),])
    cols <- prks[["pf"]]
    mypf <- rbind(mypf, cols)
    if (i == 1) colnames(mypf) <- prks[["team.teamAbbrev"]]
  }
  mypf
}

# js <- load("http://games.espn.go.com/ffl/api/v2/teams?leagueId=709331&seasonId=2014")
# foo <- powrSeason(js, 3)
# plotPowr(foo)
