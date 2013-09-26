\name{paternity-package}
\alias{paternity-package}
\alias{paternity}
\docType{package}
\title{
Paternity testing using SNP markers 
}
\description{
The package calculates composite paternity index based on SNP markers. 
}
\details{
\tabular{ll}{
Package: \tab paternity\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-15\cr
License: \tab GPL (>= 2) \cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Umesh Rosyara, Michigan State University 
Maintainer: Umesh Rosyara <rosyara@msu.edu>
}
\references{
Elston, R.C. 1986. Probability and paternity testing. 
      Am.. J.. Human Genetics 39: 112-122
Evett, I.W., and B.S. Weir.1998. Interpreting DNA evidence. Sunderland,
 MA: Sinauer Associates, Inc.
}
\keyword{ package }

\examples{
data(cherryped)
paternityIndex(dataframe = cherryped, marker = "SNPmarker", mother = "BlackRepublican", 
child = "Bing_INRA", AF ="Ambrunes" , Afreq = "a", Bfreq = "b")
paternityIndex (dataframe = cherryped, marker = "SNPmarker", mother = "BlackRepublican",
 child = "Bing_INRA", AF ="Napoleon" , Afreq = "a", Bfreq = "b")
paternityIndex (dataframe = cherryped, marker = "SNPmarker", mother = "BlackRepublican",
 child = "Bing_INRA", AF ="NY54" , Afreq = "a", Bfreq = "b") 
}
