##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Nov 2021
# Function: teamWicketsERPlotOppnAllMatches
# This function computes the  wickets vs ER of team against all opposition in all matches
#
###########################################################################################
#' @title
#' Team wickets vs ER against  all opposition all matches
#'
#' @description
#' This function computes wickets vs ER against all oppositions in all matches
#'
#' @usage
#' teamWicketsERPlotOppnAllMatches(matches,t1,t2,plot=1)
#'
#' @param matches
#' The matches of the team against all oppositions and all matches
#'
#' @param t1
#' The 1st team of the match
#'
#' @param t2
#' the 2nd team in the match
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
#'
#'
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' teamWicketsERPlotOppnAllMatches(matches,t1,t2,plot=1)
#'}
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'

teamWicketsERPlotOppnAllMatches <- function(matches,t1,t2,plot=1){
    noBalls=wides=team=runs=bowler=wicketKind=wicketPlayerOut=ER=quantile=quadrant=NULL
    team=bowler=ball=wides=noballs=runsConceded=overs=ggplotly=NULL
    over=wickets=maidens=NULL
    a <-filter(matches,team!=t1)

    a1 <- unlist(strsplit(a$ball[1],"\\."))
    # Create a string for substitution 1st or 2nd
    a2 <- paste(a1[1],"\\.",sep="")

    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        #mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub(a2,"",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))

    #Calculate the number of maiden overs
    c <- summarise(group_by(b,bowler,over),sum(runs,wides,noballs))
    names(c) <- c("bowler","over","runsConceded")
    d <-summarize(group_by(c,bowler),maidens=sum(runsConceded==0))

    #Compute total runs conceded (runs_wides+noballs)
    e <- summarize(group_by(c,bowler),runs=sum(runsConceded))

    # Calculate the number of overs bowled by each bwler
    f <- select(c,bowler,over)
    g <- summarise(group_by(f,bowler),overs=length(unique(over)))


    #Compute number of wickets
    h <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    i <- summarise(group_by(h,bowler),wickets=length(wicketPlayerOut))

    #Join the over & maidens
    j <- full_join(g,d,by="bowler")
    # Add runs
    k <- full_join(j,e,by="bowler")
    # Add wickets
    l <- full_join(k,i,by="bowler")

    # Set NAs to 0 if there are any
    if(sum(is.na(l$wickets)) != 0){
        l[is.na(l$wickets),]$wickets=0
    }
    # Arrange in descending order of wickets and runs and ascending order for maidens
    l <-arrange(l,desc(wickets),desc(runs),maidens)
    l$ER = l$runs/l$overs

    x_lower <- quantile(l$wickets,p=0.66,na.rm = TRUE)
    y_lower <- quantile(l$ER,p=0.66,na.rm = TRUE)



    plot.title <- paste("Wickets-ER of ", t1, " in all matches against ", t2)
    if(plot == 1){ #ggplot2
        l %>%
            mutate(quadrant = case_when(wickets > x_lower & ER > y_lower   ~ "Q1",
                                        wickets <= x_lower & ER > y_lower  ~ "Q2",
                                        wickets <= x_lower & ER <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(wickets,ER,color=quadrant)) +
            geom_text(aes(wickets,ER,label=bowler,color=quadrant)) + geom_point() +
            xlab("Wickets") + ylab("Economy rate") +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

    } else if(plot == 2){ #ggplotly
        g <- l %>%
            mutate(quadrant = case_when(wickets > x_lower & ER > y_lower   ~ "Q1",
                                        wickets <= x_lower & ER > y_lower  ~ "Q2",
                                        wickets <= x_lower & ER <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(wickets,ER,color=quadrant)) +
            geom_text(aes(wickets,ER,label=bowler,color=quadrant)) + geom_point() +
            xlab("Wickets") + ylab("Economy rate") +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

        ggplotly(g)
    }

}
