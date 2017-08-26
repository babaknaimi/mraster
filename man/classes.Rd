\name{mRaster-classes}

\docType{class}

\alias{mmapRaster-class}
\alias{characterORmissing-class}


\title{mRaster classes}

\description{

A set of S4, and R6 class to handle what is reuired by the package.

}

\section{Slots}{
Slots for sdmModels objects:
	\describe{
\item{\code{data}}{a sdmdata object}

\item{\code{recordIDs}}{Contains the species data}

\item{\code{setting}}{A data.frame containing predictor variables}

\item{\code{run.info}}{a data.frame containing info on runs}

\item{\code{replicates}}{The names of categorical variables (if any)}

\item{\code{models}}{a list contains all fitted objects and relevant information (e.g., evaluation) }



\bold{ Slots for sdmEvaluate objects:}
\item{\code{observed}}{a numeric vector of observed values}

\item{\code{predicted}}{a numeric vector of predicted values}

\item{\code{statistics}}{a list of threshold-independent statistics}
\item{\code{threshold_based}}{a data.frame of threshold-based statistics}

\bold{ Slots for sdmFormula objects:}
\item{\code{formula}}{input formula}

\item{\code{vars}}{character, name of variables}

\item{\code{model.terms}}{the formula terms used in model fitting}
\item{\code{data.terms}}{the formula terms used to manipulate data}

  }


}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}

\url{http://biogeoinformatics.org}
}



\keyword{classes}
\keyword{spatial}
