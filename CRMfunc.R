

CRMcalcdwcorr <- function(wordset, type = "token", dw = FALSE, numofwords = 2, gamma = 1, beta = 0.5) {

types <- c("type","token")
gamma <- abs(gamma)
beta <- abs(beta)
if ( gamma > 1 || beta > 1) {
  stop("check parameter values (0<=...<=1)")
}
if (type %ni% types) {
  stop("check CRM type spelling")
}

CRMlist <- vector()

#calculating P(c|w)


lastclmn <- numofwords*2
for (i in 1:numofwords) {
  clmn <- lastclmn + i
 
  wordset[,clmn] <- wordset[,i*2]/sum(na.omit(wordset[,i*2]))
}
wordcolpos <- c(1,3,5,7,9,11,13,15,17,19)
#counting and constructing TP set
for (i in 2:numofwords) {
wordnum <- wordcolpos[i]
TPset <- as.vector(wordset[na.omit(match(wordset$V1, wordset[,wordnum])),wordnum])

#building Precision and Recall components

#type-based
if (type == "type"){ 

#additive
  if (dw == FALSE) {

Prec <- length(TPset)/length(wordset[,1])
Rec <- length(TPset)/length(wordset[,wordnum])
} else {

#d-w
  if (type == "type"){
    minimalist <- vector()
    upper <- for (z in 1:length(match(TPset,wordset[,1]))) {
      o <- match(TPset,wordset[,1])[z]
      p <- match(TPset,wordset[,wordnum])[z]
      minima <- min(wordset[o,lastclmn + 1],wordset[p,lastclmn + i])
      minimalist <- c(minimalist,minima)
    }
    
    Prec <- (length(TPset)*(sum(minimalist)/sum(wordset[match(TPset,wordset[,1]),lastclmn + 1])))/length(wordset[,1])
    Rec <- (length(TPset)*(sum(minimalist)/sum(wordset[match(TPset,wordset[,wordnum]),lastclmn + i])))/length(wordset[,1])
  }
  
}
}



#token-based
if (type == "token") {
  if (dw == FALSE) {
Prec <- sum(wordset[match(TPset,wordset[,1]),lastclmn + 1])
Rec <- sum(wordset[match(TPset,wordset[,wordnum]),lastclmn + i])
} else {

#token-based d-w
if (type == "token") {
  Prec <- min(sum(wordset[match(TPset,wordset[,1]),lastclmn + 1]),sum(wordset[match(TPset,wordset[,wordnum]),lastclmn + i]))
  Rec <-  min(sum(wordset[match(TPset,wordset[,1]),lastclmn + 1]),sum(wordset[match(TPset,wordset[,wordnum]),lastclmn + i]))
}
}
}
#building CRM
CRM <- gamma*((2*Prec*Rec)/(Prec+Rec)) + (1-gamma)*(beta*Prec+(1-beta)*Rec)
CRMlist <- c(CRMlist,CRM)
}
otpt <- list(CRM = CRMlist,Precision = Prec, Recall = Rec)
return(otpt)
}
