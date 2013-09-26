paternityIndex <-
function (dataframe, marker = "marker", mother = "mother", child = "child", AF = "AF", Afreq = "a", Bfreq = "b", afcalcol =  NA) {
 childn = child
 AFn = AF
 mothern = mother 
 if (Afreq == "calculate" | Bfreq == "calculate") {
 df1 <- t(dataframe[, afcalcol])
 
allefreqA <- function (x) {
 aft <- table (x)
AAfreq = aft["AA"] 
ABfreq =  aft ["AB"]
BBfreq =   aft ["BB"]
AAfreq[is.na (AAfreq)]<-  0
ABfreq[is.na (ABfreq)]<-  0
BBfreq[is.na (BBfreq)]<-  0
totalfq = AAfreq + BBfreq + ABfreq 
afreq =  (AAfreq  + 0.5* ABfreq) / totalfq
return (afreq) 
}
mark_A_freq <- apply(df1,2, allefreqA)

 allefreqB <- function (x) {
aft <- table (x)
AAfreq = aft["AA"]
ABfreq =  aft ["AB"]
BBfreq =   aft ["BB"]
AAfreq[is.na (AAfreq)]<-  0
ABfreq[is.na (ABfreq)]<-  0
BBfreq[is.na (BBfreq)]<-  0
totalfq = AAfreq + BBfreq + ABfreq 
bfreq =  (BBfreq + 0.5* ABfreq) / totalfq
return (bfreq)
}
mark_B_freq <- apply(df1,2, allefreqB)

alldata = data.frame(marker = dataframe [, marker], mother = dataframe [, mother],
 child = dataframe[,child], AF = dataframe [ , AF], a = mark_A_freq,
  b=mark_B_freq, stringsAsFactors=FALSE)
} else {
# dataframe tp be applied to
alldata = data.frame(marker = dataframe [, marker], mother = dataframe [, mother], child = dataframe[,child], 
AF = dataframe [ , AF], a = dataframe [, Afreq], b=dataframe [, Bfreq], stringsAsFactors=FALSE)
}
# conditions dataframe
mother <-  c("AA", "AA",  "AB",  "AB",   "BB",  "AA", "AA",  "AA",   "AA",     "AB",    "AB",     "BB",        "AA",  "AA",  "AA", "AA", "AB",  "AB",      "BB",   "AA",    "AA")
child <-   c("AA",  "AB", "AA",  "AB",   "BB",  "AB", "BB",  "AA",   "AB",     "AA",   "AB",       "BB",      "AB",   "BB",  "AA", "AB", "AA",  "AB",     "BB",    "AB",   "BB")
AF <-      c("AA",  "AA",  "AA", "AA",   "AA",  "AA",  "AA", "AB",   "AB",     "AB",    "AB",      "AB",      "AB",   "AB",  "BB", "BB",  "BB",  "BB",    "BB",     "BB",   "BB")
Formula <- c("1/a","1/b", "1/a", "1/(a+b)",0,  "1/a",   0,  "0.5/a",  "0.5/a", "0.5/a", "1/(a+b)", "0.5 / b", "0.5/a", "0.5/b",0, "1/b",   0,    "1/(a+b)","1/b",   "1/b",  "1/b")
COEFA <-    c(1,    0,      1,     0,     0,     1,     0,   0.5,     0.5,      0.5,      0,        0,         0.5,     0,     0,   0,     0,      0,       0,         0,     0)
COEFAB <-   c(0,    0,      0,     1,     0,     0,     0,    0,       0,        0,       1,        0,         0,       0,     0,   0,     0,       1,      0,         0,     0)
COEFB <-    c(0,    1,      0,     0,     0,     0,     0,    0,       0,        0,       0,        0.5,       0,       0.5,    0,  1,     0,       0,      1,         1,     1)
coefft <- data.frame (mother, child, AF, Formula,COEFA, COEFAB,COEFB)

# create conditions data frame
cond = data.frame(MTR_CLD_AF = paste(mother,child,AF,sep="~"), COEFA, COEFAB,COEFB, stringsAsFactors=FALSE)

# geting coeffients for each row in alldata
matcond=cond[match(with(alldata, paste(mother, child, AF, sep="~")),
                   cond$MTR_CLD_AF),]
alldata$COEFA = matcond$COEFA
alldata$COEFAB = matcond$COEFAB
alldata$COEFB = matcond$COEFB

alldata$PI = with(alldata, COEFA*(1/a) +  COEFB*(1/b) + COEFAB*(1/(a+b)))
out<- summary(alldata$PI == 0)
results <- data.frame (marker = alldata$marker, PI = alldata$PI)
return(results)
 }
