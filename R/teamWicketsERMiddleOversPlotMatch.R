##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: teamWicketsERMiddleOversPlotMatch
# This function computes the  wickets vs ER of team in middle overs against  opposition in  match
#
###########################################################################################
#' @title
#' Team wickets vs ER in middle overs against   opposition in match
#'
#' @description
#' This function computes wickets vs ER in middle overs against  oppositions in all match
#'
#' @usage
#' teamWicketsERMiddleOversPlotMatch(match,t1, t2, plot=1)
#'
#' @param match
#' The match of the team against  opposition
#'
#' @param t1
#' Team for which bowling performance is required
#'
#' @param t2
#' Opposition Team
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
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
#' teamWicketsERMiddleOversPlotMatch(match, t1,t2, plot=1)
#'}
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'
teamWicketsERMiddleOversPlotMatch <- function(match,t1,t2,plot=1) {
  team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=str_extract=quantile=quadrant=ERMiddleOvers=NULL

  # Filter the performance of team1
  a <-filter(match,team!=t1)
  # Middle overs
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsMiddleOvers=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERMiddleOvers=total/count *6)
  a41 <- a31 %>% select(bowler,ERMiddleOvers)
  a42=inner_join(a4,a41,by="bowler")


  x_lower <- quantile(a42$wicketsMiddleOvers,p=0.66,na.rm = TRUE)
  y_lower <- quantile(a42$ERMiddleOvers,p=0.66,na.rm = TRUE)

  plot.title <- paste("Wickets-ER in Middle overs of ", t1, " against ", t2, " in middle overs")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsMiddleOvers > x_lower & ERMiddleOvers > y_lower   ~ "Q1",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers > y_lower  ~ "Q2",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsMiddleOvers,ERMiddleOvers,color=quadrant)) +
      geom_text(aes(wicketsMiddleOvers,ERMiddleOvers,label=bowler,color=quadrant)) + geom_point() +
      xlab("Wickets - Middle overs") + ylab("Economy rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  a42 %>%
      mutate(quadrant = case_when(wicketsMiddleOvers > x_lower & ERMiddleOvers > y_lower   ~ "Q1",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers > y_lower  ~ "Q2",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsMiddleOvers,ERMiddleOvers,color=quadrant)) +
      geom_text(aes(wicketsMiddleOvers,ERMiddleOvers,label=bowler,color=quadrant)) + geom_point() +
      xlab("Wickets - Middle overs") + ylab("Economy rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}


