#' @name plot.foreccomb_res
#' @aliases plot.foreccomb_res
#'
#' @title Plot results from forecast combination model
#' @description Produces plots for the results of a forecast combination method. Either
#' an actual vs. fitted plot (\code{which = 1}) or a barplot of the combination weights
#' (\code{which = 2}).
#'
#' @param x An object of class 'foreccomb_res'.
#' @param which Type of plot: 1 = actual vs. fitted, 2 = combination weights.
#' @param ... Other arguments passing to \code{\link[graphics]{plot.default}}.
#'
#' @return A plot for the foreccomb_res class.
#'
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' fit <- comb_EIG1(data)
#' plot(fit)
#'
#' @seealso
#' \code{\link[GeomComb]{foreccomb}},
#' \code{\link[GeomComb]{summary.foreccomb_res}}
#'
#' @author Christoph E. Weiss and Gernot R. Roetzer
#'
#' @import ggplot2
#' @importFrom graphics barplot
#'
#' @method plot foreccomb_res
#' @export
plot.foreccomb_res <- function(x, which=1,...) {
    if (class(x) != "foreccomb_res")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    method <- x$Method
    models <- x$Models
    weights <- x$Weights
    fit <- x$Fitted
    forec <- x$Forecasts_Test
    observed_vector <- x$Input_Data$Actual_Train
    newobs_vector <- x$Input_Data$Actual_Test
    Index <- NULL  #Hack to satisfy CRAN check.

    if (which == 1){
    if (is.null(forec) & is.null(newobs_vector)) {
        cols <- c(ACTUAL = "black", `COMBINED (FIT)` = "#F04546")

        pl <- as.data.frame(matrix(NA, ncol = 3, nrow = length(observed_vector)))
        colnames(pl) <- c("Index", "Actual", "Combined_Fit")
        pl[, 1] <- 1:nrow(pl)
        pl[, 2] <- c(observed_vector)
        pl[, 3] <- fit

        p <- ggplot(data = pl, aes(x = Index)) + geom_line(aes(y = pl$Actual, colour = "ACTUAL"), size = 0.5) + geom_line(aes(y = pl$Combined_Fit, colour = "COMBINED (FIT)"),
            size = 0.8) + scale_x_continuous(breaks = round(seq(0, max(pl$Index), by = nrow(pl)/10), 0)) + scale_colour_manual(name = "Series", values = cols) + guides(colour = guide_legend(override.aes = list(size = c(0.5,
            0.8)))) + xlab("Index") + ylab(paste0(method, "\n Fitted Values/Forescasts")) + ggtitle(paste0(method, " Forecast Combination \n Actual vs. Fitted/Test Set Forecasts")) +
            theme(plot.title = element_text(size = 16, face = "bold")) + theme(legend.title = element_text(colour = "black", size = 12, face = "bold"))
        p
    } else {

        cols <- c(ACTUAL = "black", `COMBINED (FIT)` = "#F04546", `COMBINED (FORECAST)` = "#F04546")

        if (is.null(newobs_vector) == FALSE) {
            pl <- as.data.frame(matrix(NA, ncol = 4, nrow = (length(observed_vector) + length(newobs_vector))))
            colnames(pl) <- c("Index", "Actual", "Combined_Fit", "Combined_Forecast")
            pl[, 1] <- 1:nrow(pl)
            pl[, 2] <- c(observed_vector, newobs_vector)
            pl[, 3] <- c(fit, rep(NA, length(forec)))
            pl[, 4] <- c(rep(NA, length(fit)), forec)
            pl[length(observed_vector), 4] <- pl[length(observed_vector), 3]

            p <- ggplot(data = pl, aes(x = Index)) + geom_line(aes(y = pl$Actual, colour = "ACTUAL"), size = 0.5) + geom_line(aes(y = c(pl$Combined_Fit), colour = "COMBINED (FIT)"),
                na.rm = TRUE, size = 0.8) + geom_line(aes(y = c(pl$Combined_Forecast), colour = "COMBINED (FORECAST)"), na.rm = TRUE, size = 1.5) + scale_x_continuous(breaks = round(seq(0,
                max(pl$Index), by = nrow(pl)/10), 0)) + scale_colour_manual(name = "Series", values = cols) + guides(colour = guide_legend(override.aes = list(size = c(0.5,
                0.8, 1.5)))) + xlab("Index") + ylab(paste0(method, "\n Fitted Values/Forecasts")) + ggtitle(paste0(method, " Forecast Combination \n Actual vs. Fitted/Test Set Forecast")) +
                theme(plot.title = element_text(size = 16, face = "bold")) + theme(legend.title = element_text(colour = "black", size = 12, face = "bold")) + geom_vline(xintercept = length(observed_vector),
                size = 1, linetype = "longdash", colour = "black")
            p
        } else {
            pl <- as.data.frame(matrix(NA, ncol = 4, nrow = (length(observed_vector) + length(forec))))
            colnames(pl) <- c("Index", "Actual", "Combined_Fit", "Combined_Forecast")
            pl[, 1] <- 1:nrow(pl)
            pl[, 2] <- c(observed_vector, rep(NA, length(forec)))
            pl[, 3] <- c(fit, rep(NA, length(forec)))
            pl[, 4] <- c(rep(NA, length(fit)), forec)
            pl[length(observed_vector), 4] <- pl[length(observed_vector), 3]

            p <- ggplot(data = pl, aes(x = Index)) + geom_line(aes(y = pl$Actual, colour = "ACTUAL"), na.rm = TRUE, size = 0.5) + geom_line(aes(y = c(pl$Combined_Fit),
                colour = "COMBINED (FIT)"), na.rm = TRUE, size = 0.8) + geom_line(aes(y = c(pl$Combined_Forecast), colour = "COMBINED (FORECAST)"), na.rm = TRUE, size = 1.5) +
                scale_x_continuous(breaks = round(seq(0, max(pl$Index), by = nrow(pl)/10), 0)) + scale_colour_manual(name = "Series", values = cols) + guides(colour = guide_legend(override.aes = list(size = c(0.5,
                0.8, 1.5)))) + xlab("Index") + ylab(paste0(method, "\n Fitted Values")) + ggtitle(paste0(method, " Forecast Combination \n Actual vs. Fitted")) + theme(plot.title = element_text(size = 16,
                face = "bold")) + theme(legend.title = element_text(colour = "black", size = 12, face = "bold")) + geom_vline(xintercept = length(observed_vector),
                size = 1, linetype = "longdash", colour = "black")
            p
        }
    }
    } else {
      if (which == 2){
        if (is.numeric(weights)){
          graphics::barplot(weights, main=paste0(method, "\nCombination Weights"), ylab="Combination Weight",
                            names.arg = models, ylim=c(min(1.1*min(weights), 0), 1.1*max(weights)), las=3, cex.names=0.8)
        } else {
        message(paste0(method, " produces time-varying weights among input models. Cannot plot weights."))
      }
    } else stop("Parameter 'which' must be either 1 or 2.", call. = FALSE)
    }
}
