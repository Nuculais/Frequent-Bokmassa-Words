bokTweets<-read.csv("X:/Documents/lab1Dataset.tsv", encoding = "UTF-8",header = TRUE, sep = "\t",quote = "",fill = TRUE)

View(bokTweets)
colnames(bokTweets)
summary(bokTweets)


tweets <- Corpus(VectorSource(bokTweets$text))


for (i in 1:length(tweets)) {content(tweets[[i]]) <- sapply(content(tweets[[i]]),function(row) iconv(row, "latin1", "ASCII", sub=""))}


plotTopWords <- function(tweets, numWordInstances, topWrds){
  
  DTM <- TermDocumentMatrix(tweets, control = list(minWordLength=1))
  matrix <- as.matrix(DTM)
  wFreq <- rowSums(matrix)
  sortedWords <- sort(wFreq, decreasing=TRUE)
  topSortedWords <- subset(sortedWords, sortedWords >= numWordInstances)
  barplot(topSortedWords[1:topWrds], space = 1, cex.names = 0.6, las = 2 )
  return(topSortedWords)
}

topTwtdWrds <- plotTopWords(tweets, 50, 30)


tweetsLwr <- tm_map(tweets,content_transformer(tolower))

topTwtdWrdsLwr <- plotTopWords(tweetsLwr, 50, 30)


removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweetsLwrURL <- tm_map(tweetsLwr, content_transformer(removeURL))
topTwtdWrdsLwrURL <- plotTopWords(tweetsLwrURL, 50, 30)


as.table(cbind(names(topTwtdWrdsLwrURL [1:20]), names(topTwtdWrdsLwr [1:20])))


setdiff(names(topTwtdWrdsLwrURL), names(topTwtdWrdsLwr))


tweetsLwrURLPnct <- tm_map(tweetsLwrURL, removePunctuation)
topTwtdWrdsLwrURLPnct <- plotTopWords(tweetsLwrURLPnct, 50, 30)


tweetsLwrURLPnctNum <- tm_map(tweetsLwrURLPnct, removeNumbers)
topTwtdWrdsLwrURLPnctNum <- plotTopWords(tweetsLwrURLPnctNum, 50, 30)


myStopwords<-c("och", "det", "att", "i", "en", "jag", "hon", "som", "han", "paa", "den", "med", "var", "sig", "foer", "saa", "till", "aer", "men", "ett", "om", "hade", "de", "av", "icke", "mig", "du", "henne", "daa", "sin", "nu", "har", "inte", "hans", "honom", "skulle", "hennes", "daer", "min", "man", "ej", "vid", "kunde", "naagot", "fraan", "ut", "naer", "efter", "upp", "vi", "dem", "vara", "vad", "oever", "aen", "dig", "kan", "sina", "haer", "ha", "mot", "alla", "under", "naagon", "eller", "allt", "mycket", "sedan", "ju", "denna", "sjaelv", "detta", "aat", "utan", "varit", "hur", "ingen", "mitt", "ni", "bli", "blev", "oss", "din", "dessa", "naagra", "deras", "blir", "mina", "samma", "vilken", "er", "saadan", "vaar", "blivit", "dess", "inom", "mellan", "saadant", "varfoer", "varje", "vilka", "ditt", "vem", "vilket", "sitta", "saadana", "vart", "dina", "vars", "vaart", "vaara", "ert", "era", "vilka")


topTwtdWrdsLwrURLPnctNumStop <- tm_map(tweetsLwrURLPnctNum, removeWords, myStopwords )
topTwtdWrdsLwrURLPnctNumStop <- plotTopWords(topTwtdWrdsLwrURLPnctNumStop, 50, 30)


as.table(cbind(names(topTwtdWrds[1:20]), names(topTwtdWrdsLwrURL[1:20]), names(topTwtdWrdsLwrURLPnct[1:20]), names(topTwtdWrdsLwrURLPnctNum[1:20]), names(topTwtdWrdsLwrURLPnctNumStop[1:20])))


myStopwordsApnd <- c(myStopwords, "plats", "maaste", "aarets", "amp", "boerjar", "sista", "vaelkomna", "skriver", "mer", "lyssna", "prata", "snart" , "goer", "aar", "missa", "foersta", "aarets", "saeger" )
topTwtdWrdsLwrURLPnctNumStopApnd <- tm_map(tweetsLwrURLPnctNum, removeWords, myStopwordsApnd)
topTwtdWrdsLwrURLPnctNumStopApnd <- plotTopWords(topTwtdWrdsLwrURLPnctNumStopApnd, 20, 50)


identifiers <- names(topTwtdWrds)
wordcloud(words=identifiers, freq= topTwtdWrds, min.freq=8)


identifiers <- names(topTwtdWrdsLwrURLPnctNumStopApnd)
wordcloud(words=identifiers, freq= topTwtdWrdsLwrURLPnctNumStopApnd, min.freq=100)
