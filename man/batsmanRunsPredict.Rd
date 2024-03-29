% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batsmansRunsPredict.R
\name{batsmanRunsPredict}
\alias{batsmanRunsPredict}
\title{Predict deliveries to runs scored}
\usage{
batsmanRunsPredict(df, name= "A Leg Glance",dateRange)
}
\arguments{
\item{df}{Data frame}

\item{name}{Name of batsman}

\item{dateRange}{Date interval to consider}
}
\value{
None
}
\description{
This function  uses a classification tree to predict the number of deliveries required for
the batsman to score the runs. It uses the package rpart to perform the classification
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
#Get the data frame for Kohli
kohli <- getBatsmanDetails(team="India",name="Kohli",dir=pathToFile)
batsmanRunsVsStrikeRate(kohli,"Kohli",dateRange)
}

}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{batsmanDismissals}}\cr
\code{\link{batsmanRunsVsDeliveries}}\cr
\code{\link{batsmanRunsVsStrikeRate}}\cr
\code{\link{batsmanRunsPredict}}\cr
\code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
}
\author{
Tinniam V Ganesh
}
