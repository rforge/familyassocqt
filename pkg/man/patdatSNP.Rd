\name{patdatSNP}
\alias{patdatSNP}
\docType{data}
\title{
Cherry paternity analysis dataset
}
\description{
The dataset uses a subset of makers from 6K SNP genotyping array to compute 
paternity index. 
}
\usage{data(patdatSNP)}
\format{
  A data frame with 39 observations on the following 20 variables.
  \describe{
    \item{\code{SNPmarker}}{Name of SNP markers}
    \item{\code{Chr}}{Chromsome where SNP is located}
    \item{\code{a}}{A alelle frequency}
    \item{\code{b}}{B allele frequency}
    \item{\code{Child}}{SNP marker scores for individual named Child (AA, AB, BB)}
    \item{\code{Mother}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather1}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather2}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather3}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather4}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather5}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather6}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather7}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather8}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather9}}{SNP marker scores (AA, AB, BB)}
    
    }
}
\source{

Simulated dataset 

}
\references{
Rosyara UR, Sebolt AM, Peace C and Iezzoni AF.Identification of the paternal 
parent for the sweet cherry (Prunus avium L.) cultivar 'Bing' and confirmation 
of descendants using SNP markers 
}
\examples{
data(patdatSNP)
 pind <- paternityIndex (dataframe =patdatSNP , marker = "SNPmarker", 
 mother = "Mother",  child = "Child", AF = "AFather1", Afreq = "a", 
 Bfreq = "b", afcalcol = NA)
 # log composite paternity index 
 log(prod(pind$PI))
 
 # second case for AFather2 
  pind1 <- paternityIndex (dataframe =patdatSNP , marker = "SNPmarker", 
 mother = "Mother",  child = "Child", AF = "AFather2", Afreq = "a", 
 Bfreq = "b", afcalcol = NA)
 # log composite paternity index 
 log(prod(pind1$PI))
 
 # allele calculated from the dataset provided in columns afcalcol
pind2 <- paternityIndex (dataframe =patdatSNP , marker = "SNPmarker", 
mother = "Mother", child = "Child", AF = "AFather1", Afreq = "calculate",
 Bfreq = "calculate", afcalcol = 4:15)
  log(prod(pind2$PI))
  
}
\keyword{datasets}
