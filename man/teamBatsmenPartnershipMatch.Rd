% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamBatsmenPartnershipMatch.R
\name{teamBatsmenPartnershipMatch}
\alias{teamBatsmenPartnershipMatch}
\title{Team batting partnerships of batsmen in a match}
\usage{
teamBatsmenPartnershipMatch(match,theTeam,opposition, plot=1)
}
\arguments{
\item{match}{The match between the teams}

\item{theTeam}{The team for which the the batting partnerships are sought}

\item{opposition}{The opposition team}

\item{plot}{Plot=1 (static),plot=2(interactive),plot=3(table)}
}
\value{
df
The data frame of the batsmen partnetships
}
\description{
This function plots the partnerships of batsmen in a match against an opposition or it can return
the data frame
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# Get athe match between England and Pakistan
a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
batsmenPartnershipMatch(a,"Pakistan","England")
batsmenPartnershipMatch(a,"England","Pakistan", plot=TRUE)
m <-batsmenPartnershipMatch(a,"Pakistan","England", plot=FALSE)
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
\code{\link{teamBatsmenVsBowlersOppnAllMatches}}\cr
}
\author{
Tinniam V Ganesh
}
