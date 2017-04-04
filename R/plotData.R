#'
#' The plotData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main functions to provide several plots for analyzing operating reserve data.
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'




#' @title plotXminAVGMWvs4secMW
#'
#' @description This method plots the 1min average of the operating reserve needs, 15min negative and positive needs, as well as the 15min calls against the original 4sec operating reserve needs
#'
#' @param dataframe - data.frame with the operating reserve needs (in MW), the type ("RZBedarf"), a DateTime (POSIXct object) and the averages (1min, 15min neg. and pos.) operating reserve needs and the 15min calls.
#'
#' @return a line chart with the 4sec operating reserve needs (grey)...
#'
#' @examples
#' dataframe <- buildCorrectingCallsDF(df.1min, df.avg.15min.neg, df.avg.15min.pos, df.calls.preprocessed)
#' plotXminAVGMWvs4secMW(dataframe)
#'
#' @export
#'
plotAVGMWvs4secMW <- function(dataframe) {

  library(ggplot2)

  g2 <- ggplot(dataframe, aes(DateTime)) +
    geom_line(aes(y = MW, colour = "MW")) +
    geom_step(aes(y = avg_1min_MW, colour = "avg. 1min needs")) +
    geom_step(aes(y = avg_15min_MW_NEG, colour = "avg. neg. 15min needs")) +
    geom_step(aes(y = avg_15min_MW_POS, colour = "avg. pos. 15min needs")) +
    geom_step(aes(y = neg_MW, colour = "avg. neg. calls")) +
    geom_step(aes(y = pos_MW, colour = "avg. pos. calls")) +
    scale_colour_manual(values = c("#79c5dc", "#fb7474", "#de1b1b", "#77d49c", "#5cb26c", "#ababab")) +
    labs(x = "Date and Time", y = "Power (in MW)") +
    ggtitle('Operating Reserve Needs') +
    theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)
    ) +
    scale_y_continuous(label = function(x){return(paste( x, " MW"))})

  g2

}






#' @title plotCorrectedNeeds
#'
#' @description This method plots the 1min average of the operating reserve needs, 15min negative and positive needs, as well as the 15min calls against the original 4sec operating reserve needs
#'
#' @param dataframe - data.frame with the operating reserve needs (in MW), and the averages (1min, 15min neg. and pos.) operating reserve needs and the 15min calls and the corrected needs.
#'
#' @return a line chart with the averages of operating reserve needs (grey)...
#'
#' @examples
#' dataframe <- approxOperatingReserveCalls(s.needs, s.calls)
#' plotCorrectedNeeds(dataframe)
#'
#' @export
#'
plotCorrectedNeeds <- function(dataframe){

  library(ggplot2)

  g2 <- ggplot(dataframe, aes(DateTime)) +
    # geom_line(aes(y = MW, colour = "MW")) +
    geom_step(aes(y = avg_1min_MW, colour = "avg. 1min needs")) +
    geom_step(aes(y = avg_15min_MW_NEG, colour = "avg. neg. 15min needs")) +
    geom_step(aes(y = avg_15min_MW_POS, colour = "avg. pos. 15min needs")) +
    geom_step(aes(y = neg_MW, colour = "avg. neg. calls")) +
    geom_step(aes(y = pos_MW, colour = "avg. pos. calls")) +
    geom_step(aes(y = Corrected, colour = "Corrected")) +
    scale_colour_manual(values = c("#79c5dc", "#fb7474", "#de1b1b", "#77d49c", "#5cb26c", "#ababab")) +
    labs(x = "Date and Time", y = "Power (in MW)") +
    ggtitle('Operating Reserve Needs') +
    theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)
    ) +
    scale_y_continuous(label = function(x){return(paste( x, " MW"))})

  g2
}






