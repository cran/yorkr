% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTeamBattingDetails.R
\name{getTeamBattingDetails}
\alias{getTeamBattingDetails}
\title{Get team batting details}
\usage{
getTeamBattingDetails(team,dir=".",save=FALSE,odir=".")
}
\arguments{
\item{team}{The team for which batting details is required}

\item{dir}{The source directory}

\item{save}{Whether the data frame needs to be saved as RData or not. It is recommended to set save=TRUE
as the data can be used for a lot of analyses of batsmen}

\item{odir}{The output directory to store saved files}
}
\value{
battingDetails
The dataframe with the batting details
}
\description{
This function  gets the batting details of a team in all matchs against all
oppositions. This gets all the details of the batsmen balls faced,4s,6s,strikerate, runs, venue etc.
This function is then used for analyses of batsmen. This function calls teamBattingPerfDetails()
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
a <- getTeamBattingDetails("India",dir="../data", save=TRUE)
}

}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{getBatsmanDetails}}\cr
\code{\link{getBowlerWicketDetails}}\cr
\code{\link{batsmanDismissals}}\cr
\code{\link{getTeamBowlingDetails}}\cr
}
\author{
Tinniam V Ganesh
}
