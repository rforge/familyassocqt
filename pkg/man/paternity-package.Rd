\name{paternity-package}
\alias{paternity-package}
\alias{paternity}
\docType{package}
\title{
Paternity testing using SNP markers 
}
\description{
The package calculates composite paternity index based on SNP markers. 
The paternity index are calculation based on no mutations 
or no technical error. High quality data, preferably confirmed genotypes with 
multiple assays are desirable. The package is for research purpose only not for 
legal or diagnostic purpose.  
}
\details{
\tabular{ll}{
Package: \tab paternity\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2013-09-26\cr
License: \tab GPL (>= 2) \cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Umesh Rosyara, Michigan State University 
Maintainer: Umesh Rosyara <rosyara@msu.edu> or 
<rosyaraur@gmail.com>
}
\references{
Elston, R.C. 1986. Probability and paternity testing. 
      Am.. J.. Human Genetics 39: 112-122
Evett, I.W., and B.S. Weir.1998. Interpreting DNA evidence. Sunderland,
 MA: Sinauer Associates, Inc.
}
\keyword{ package }

\examples{
data(patdatSNP)
 pind <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
 mother = "TrueMother",  child = "Child", AF = "AFather1", Afreq = "afeq", 
 Bfreq = "bfeq", afcalcol = NA)
 # log composite paternity index 
 log(prod(pind$PI))
 
 # allele calculated from the dataset provided in columns afcalcol
pind1 <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
mother = "TrueMother", child = "Child", AF = "AFather1", Afreq = "calculate",
 Bfreq = "calculate", afcalcol = 4:15)
}
