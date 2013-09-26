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
    \item{\code{SNPname}}{Name of SNP markers}
    \item{\code{afeq}}{A alelle frequency}
    \item{\code{bfeq}}{B allele frequency}
    \item{\code{Child}}{SNP marker scores for individual named Child (AA, AB, BB)}
    \item{\code{TrueMother}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather1}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather2}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather3}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather4}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather5}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather6}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather7}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather8}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather9}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather10}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather11}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather12}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather13}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather14}}{SNP marker scores (AA, AB, BB)}
    \item{\code{AFather15}}{SNP marker scores (AA, AB, BB)}
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
 pind <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
 mother = "TrueMother",  child = "Child", AF = "AFather1", Afreq = "afeq", 
 Bfreq = "bfeq", afcalcol = NA)
 # log composite paternity index 
 log(prod(pind$PI))
 
 # allele calculated from the dataset provided in columns afcalcol
pind1 <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
mother = "TrueMother", child = "Child", AF = "AFather1", Afreq = "calculate",
 Bfreq = "calculate", afcalcol = 4:20)
  
}
\keyword{datasets}
