\name{CheckNormality}
\alias{CheckNormality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check Normality of set(s) of observations
%%  ~~function to do ... ~~
}
\description{Checks normaility of a set of data which may or may not be broken out by treatments. Provides IQR, SD, and IQR/SD.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CheckNormality(observation, treatment)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{observation}{vector of numeric values}
  \item{treatment}{optional value providing factors by which to separate observation values}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Prints a series of summary statistics to quickly check how normal the data is for either a single set of observations or for observations broken down by factors.
\item{IQR}{Inter Quartile Range}
\item{Stand Dev}{Standard Deviation}
\item{IQR/Sigma}{Provides numeric value to check normality}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Also returns Histogram for single factor sets, and boxplots regardless of number of factors.
}
\references{My github -> https://github.com/ftkjr/R
%% ~put references to the literature/web site here ~
}
\author{Frederick Kaesmann Jr
%%  ~~who you are~~
}
\note{I hope you like it
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{SummaryStats.Rd, DistMod.Rd
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Single or no factor
data("CO2")

CheckNormality(CO2$uptake)

#####

## Multiple treatment factors
data("CO2")

CheckNormality(CO2$uptake,CO2$Plant)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
