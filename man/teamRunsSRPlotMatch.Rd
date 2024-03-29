% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamRunsSRPlotMatch.R
\name{teamRunsSRPlotMatch}
\alias{teamRunsSRPlotMatch}
\title{Team Runs vs SR in  match}
\usage{
teamRunsSRPlotMatch(match,theTeam, opposition, plot=1)
}
\arguments{
\item{match}{All matches of the team in all matches with all oppositions}

\item{theTeam}{The team for which the the batting partnerships are sought}

\item{opposition}{The opposition team}

\item{plot}{plot=1 (static),plot=2(interactive)}
}
\description{
This function computes and plots the Runs vs SR  of a team in matches
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{

# Top batsman is displayed in descending order of runs
teamRunsSRPlotMatch(matches,theTeam="India",opposition="England")

}

}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{teamBatsmenVsBowlersAllOppnAllMatchesPlot}}\cr
\code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
\code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
\code{\link{teamBowlingWicketRunsAllOppnAllMatches}}
}
\author{
Tinniam V Ganesh
}
