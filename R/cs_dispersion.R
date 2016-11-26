#' @title Compute Cross-Sectional Dispersion
#'
#' @description Computes (time-varying) dispersion measures for the cross section of individual model forecasts that are the input of forecast combination.
#'
#' @details
#' The available measures of scale are defined as in Davison (2003). Let \eqn{y_{(i)}}{y_(i)} denote the i-th order statistic of the sample, then:
#'
#' \deqn{Range_t = y_{(n), t} - y_{(1), t}}
#'
#' \deqn{IQR_t = y_{(3n/4),t} - y_{(n/4),t}}
#'
#' \deqn{SD_t = \sqrt{\frac{1}{n-1} \Sigma_{i=1}^n \left(y_{i,t} - \bar{y}_t \right)}}{SD_t = sqrt(1/(n-1) \Sigma_{i=1}^n (y_{i,t} - \bar{y}_t))}
#'
#' Previous research in the forecast combination literature has documented that regression-based combination methods tend to have relative advantage when one or more individual model forecasts are better than the rest, while eigenvector-based methods tend to have relative advantage when individual model forecasts are in the same ball park.
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param measure Cross-sectional dispersion measure, one of: \code{"SD"} = standard deviation (default); \code{"IQR"} = interquartile range; or \code{"Range"} = range.
#' @param plot logical. If \code{TRUE}, evolution of cross-sectional forecast dispersion is plotted as \code{ggplot}.
#'
#' @return Returns a vector of the evolution of cross-sectional dispersion over the sample period (using the selected dispersion measure)
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
#' cs_dispersion(data, measure = "IQR")
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link[stats]{sd}},
#' \code{\link[stats]{IQR}},
#' \code{\link[base]{range}}
#'
#' @references
#' Davison, A. C. (2003). Statistical Models. \emph{Cambridge University Press}.
#'
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords ts
#'
#' @importFrom stats IQR sd
#'
#' @export
cs_dispersion <- function(x, measure = "SD", plot = FALSE) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)

    if (!is.null(x$Forecasts_Test)) {
        forecast_data <- rbind(x$Forecasts_Train, x$Forecasts_Test)
    } else {
        forecast_data <- x$Forecasts_Train
    }

    cs_data <- list()
    if (is.null(measure))
        stop("Dispersion measure must be 'SD', 'IQR', or 'Range'.", call. = FALSE)
    cs_data$Dispersion_Measure <- measure
    cs_data$CS_Dispersion <- rep(NA, nrow(forecast_data))
    if (measure == "SD") {
        for (i in 1:nrow(forecast_data)) {
            cs_data$CS_Dispersion[i] <- sd(forecast_data[i, ])
        }
    } else {
        if (measure == "IQR") {
            for (i in 1:nrow(forecast_data)) {
                cs_data$CS_Dispersion[i] <- IQR(forecast_data[i, ])
            }
        } else {
            if (measure != "Range")
                stop("Dispersion measure must be 'SD', 'IQR', or 'Range'.", call. = FALSE)

            for (i in 1:nrow(forecast_data)) {
                cs_data$CS_Dispersion[i] <- max(forecast_data[i, ]) - min(forecast_data[i, ])
            }
        }
    }

    if (class(plot)!="logical") stop("Plot parameter is not logical. Must be 'TRUE' or 'FALSE'.")

    if (plot == FALSE){
        return(cs_data)
    } else{
        pckg <- c("ggplot2")
        temp <- unlist(lapply(pckg, require, character.only = TRUE))
        if (!all(temp == 1))
            stop("This function requires package \"ggplot2\".\n Use install.packages(\"ggplot2\") if it is not yet installed.\n", call. = FALSE)
        Index <- NULL  #Hack to satisfy CRAN check.

        pl <- as.data.frame(matrix(NA, ncol = 2, nrow = length(cs_data$CS_Dispersion)))
        colnames(pl) <- c("Index", "Value")
        pl[, 1] <- 1:nrow(pl)
        pl[, 2] <- cs_data$CS_Dispersion

        p <- ggplot(data = pl, aes(x = Index)) + geom_line(aes(y = pl$Value), colour = "blue", na.rm = TRUE, size = 0.5) + scale_x_continuous(breaks = round(seq(0,
            max(pl$Index), by = nrow(pl)/10), 0)) + xlab("Index") + ylab(paste0(measure)) + theme(legend.position = "none") + ggtitle("Cross-Sectional Dispersion \n of Individual Forecasts") +
            theme(plot.title = element_text(size = 16, face = "bold")) +
            if (!is.null(x$Forecasts_Test)){
              geom_vline(xintercept = nrow(x$Forecasts_Train), size = 1, linetype = "longdash", colour = "black")
            }
        print(p)
        return(cs_data)
    }
}
