\name{electricity}
\docType{data}
\alias{electricity}

\title{UK Electricity Supply 2007 - 2017}

\description{
   The \code{electricity} dataset contains monthly data on the total UK electricity
   supply in GWh from January 2007 to March 2017, as well as univariate time series
   forecasts for this series.
}
\usage{data(electricity)}
\format{
   A multivariate time series of 123 observations; monthly, 2007-2017.

   This data contains the following columns:
   \describe{
      \item{arima}{(ARIMA 1-month forecasts)}
      \item{ets}{(ETS 1-month forecasts)}
      \item{nnet}{(Neural Network 1-month forecasts)}
      \item{dampedt}{(Damped Trend 1-month forecasts)}
      \item{dotm}{(Dynamic Optimized Theta 1-month forecasts)}
      \item{Actual}{(Observed values)}
   }
}

\source{

International Energy Agency (2017). IEA Monthly Electricity Statistics.
Available at \url{http://www.iea.org/statistics/monthlystatistics/monthlyelectricitystatistics/}
}

\keyword{datasets}
