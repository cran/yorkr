##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 14 Apr 2016
# Function: bowlerCumulativeAvgWickets
# This function computes and plots the cumulative average wickets of a bowler
#
###########################################################################################
#' @title
#' Bowler's cumulative average wickets
#'
#' @description
#' This function computes and plots the cumulative average wickets of a bowler
#'
#' @usage
#' bowlerCumulativeAvgWickets(df,name,dateRange,staticIntv1=1)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
#'
#' @param dateRange
#' Date interval to consider
#'
#' @param staticIntv1
#' Static or interactive -staticIntv1 =1 (static plot) &  staticIntv1 =2 (interactive  plot)
#'
#' @return None
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{)
#' #'Get the data frame for RA Jadeja
#' jadeja <- getBowlerWicketDetails(team="India",name="Jadeja",dir=pathToFile)
#' bowlerCumulativeAvgWickets(jadeja,"RA Jadeja",dateRange)
#' }
#' @seealso
#' \code{\link{batsmanCumulativeAverageRuns}}
#' \code{\link{bowlerCumulativeAvgEconRate}}
#' \code{\link{batsmanCumulativeStrikeRate}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsPredict}}
#'
#' @export
#'
bowlerCumulativeAvgWickets <- function(df,name,dateRange,staticIntv1=1){
    wickets=cs=no=NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    b <- select(df,wickets)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,wickets)

    d <- mutate(c,cs=cumsum(wickets)/no)
    plot.title= paste(name,"- Cumulative avg wkts vs No innings")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            xlab("No of innings") + ylab("Cumulative Avg. wickets") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else {
       g <- ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            xlab("No of innings") + ylab("Cumulative Avg. wickets") +
           ggtitle(plot.title)
       ggplotly(g)
    }
}
