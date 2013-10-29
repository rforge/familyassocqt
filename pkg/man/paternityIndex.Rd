\name{paternityIndex}
\alias{paternityIndex}
\title{
Calculation of composite paternity index using SNP data 
}
\description{
The function computes composite paternity index based on SNP data useful for 
confirming paternity. The paternity index are calculation based on no mutations 
or no technical error. High quality data, preferably confirmed genotypes with 
multiple assays are desirable. The package is for research purpose only not for 
legal or diagnostic purpose.  
}
\usage{
paternityIndex(dataframe, marker = "marker", mother = "mother", child = "child",
      AF = "AF", Afreq = "a", Bfreq = "b", afcalcol = NA)
}
\arguments{
  \item{dataframe}{
name of dataframe used in the analysis, should have information on marker name,
SNP data in AB format, Allele frequencies (A and B columns)
}
  \item{marker}{
name of marker column
}
  \item{mother}{
Name of mother or known parent column used in analysis 
}
  \item{child}{
Name of child column used in analysis 
}
  \item{AF}{
Alleged parent (father or mother) used in analysis  
}
  \item{Afreq}{
Frequency A alleles (0 to 1), "calculate" with use the specified data range in 
afcalcol to calculate alelele frequency. Note that the individuals are in columns 
the markers are in rows.   
}
  \item{Bfreq}{
Frequency of B alleles (0 t0 1), "calculate" with use the specified data range in 
afcalcol to calculate alelele frequency. Note that the individuals are in columns 
the markers are in rows.   
}
  \item{afcalcol}{
column range (start:end), where the data for calculating allele frequency is stored
Afreq or Bfreq should be "calculate", in case where the allele frequency are estimated
from the data itself. 
}
}
\value{
Returns log (composite paternity index) between the Alleged parent (father) and child 
}
\references{
Elston, R.C. 1986. Probability and paternity testing. Am.. J.. 
Human Genetics 39: 112-122

Evett, I.W., and B.S. Weir.1998. Interpreting DNA evidence. Sunderland, 
MA: Sinauer Associates, Inc.
}
\author{
Umesh Rosyara 
}
\examples{
data(patdatSNP)
 pind <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
 mother = "TrueMother", child = "Child", AF = "AFather1", Afreq = "afeq",
  Bfreq = "bfeq", afcalcol = NA)
 # log composite paternity index 
 log(prod(pind$PI))
 
 # allele calculated from the dataset provided in columns afcalcol
pind1 <- paternityIndex (dataframe =patdatSNP , marker = "SNPname", 
mother = "TrueMother",  child = "Child", AF = "AFather1", Afreq = "calculate",
 Bfreq = "calculate", afcalcol = 4:15)
  
 }

\keyword{ paternity }
\keyword{ parentage }% __ONLY ONE__ keyword per line
