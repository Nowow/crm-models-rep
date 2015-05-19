#library(rJava)
#library(wordnet)
#
#initialize the dictionary | initDict(pathData = "C:\\Program Files\\R\\R-3.0.1\\library\\dict")
#
# get a filter getTermFilter(type, word, ignoreCase)
#
#get index terms getIndexTerms(pos, maxLimit, filter)
#pos Part of speech type. Must be either "ADJECTIVE", "ADVERB", "NOUN", or "VERB".
#maxLimit Maximum number of results.
#filter A term filter (see getTermFilter).
#
#get synsets getSynsets(indexterm)
#
#getWord(synset)

#basicwordlist <- list(NULL)
#counter = 0
#for (i in mcewvector) {
#  counter = counter + 1
#  filter <- getTermFilter("ExactMatchFilter", i, ignoreCase = TRUE)
#  indterms <- getIndexTerms("NOUN", maxLimit = "5", filter = filter)
#  try(for (t in 1:10) {
#    syns <- getSynsets(indterms[[1]]) })
#  wdlist <- getWord(syns[[1]])
#  wordlist[[counter]] <- wdlist
#  
#}

tbasicwordlist <- list()
twdlist <- vector()
counter = 0
for (i in bewlistvector) {
  counter = counter + 1
  filter <- getTermFilter("ExactMatchFilter", i, ignoreCase = TRUE)
  indterms <- getIndexTerms("NOUN", maxLimit = "5", filter = filter)
  try(for (k in 1:10) {
    syns <- getSynsets(indterms[[1]]) })
  if (length(syns) < 3) {
    wdlist <- getWord(syns[[1]])
    for (t in wdlist) {
      tfilter <- getTermFilter("ExactMatchFilter", t, ignoreCase = TRUE)
      tindterms <- getIndexTerms("NOUN", maxLimit = "5", filter = tfilter)
      try(for (p in 1:10) {
        tsyns <- getSynsets(tindterms[[1]]) })
      if (length(tsyns) < 2) {
        twdlist <- c(twdlist,t)
      }
    }
    tbasicwordlist[[ i ]] <- twdlist
    twdlist <- vector()
  }
}
