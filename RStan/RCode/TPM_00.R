library(topicmodels)
library(dplyr)
library(tidytext)

data("stop_words")
#========== read in page docs file ========
library(readr)
docs <- read_csv("~/Dropbox/Stan/Data/NLP_Page_Docs.csv", col_names = FALSE, col_types = cols(X1 = col_integer(), X2 = col_character()))
head(docs)
colnames(docs)[2]
#==================== list of list of words =======================
library(stringr)

docs$X3 <- str_replace_all(docs$X2, '[\\[,\\d,\\]]', "") 
docs$X4 <- str_replace_all(docs$X3, "\'", "") 
docs$X4
docs$X5 <- str_split(docs$X4, "\\s+")

dks <- docs$X4 # list of list
class(dks)
unlist(dks[2])
length(dks[740])
dks
length(dks)
txt = tibble(line=1:length(dks), text=dks)
txt

tdy <- txt %>%
  unnest_tokens(word, text) %>% #one-token-per-document-per-row
  anti_join(stop_words) %>%
  mutate(count=frequency(word))

tdy_cnt <- tdy %>%
  count(word, sort=FALSE)


#tdy <- tdy %>%
 # inner_join(tdy_cnt, by=c("word"))

tdy

dtmt<- tdy %>%
  cast_dtm(document=line, term=word, value = count)

dtmt$dimnames

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtmt, k = 10, control = list(seed = 1234))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics = as.data.frame(ap_topics)
ap_topics
library(tidytext)
ap_doc <- tidy(ap_lda, matrix = "gamma")
ap_doc <- as.data.frame(ap_doc)
head(ap_doc, 100)
ap_doc$d <- as.integer(ap_doc$document)
ap_doc$t <- as.integer(ap_doc$topic)
#dind = which(ap_doc$document==1)
#=======================================
library(tidyverse)
doc_loadings <-ap_doc %>% 
  spread(key=topic, value=gamma)

doc_loadings <- doc_loadings[order(as.numeric(doc_loadings$document)),]
class(doc_loadings)

ptp <- as.matrix(doc_loadings[,-1])

ap_doc[ap_doc$t==4&order(ap_doc$gamma, decreasing=TRUE),]

colv = ptp[,1]
colv[colv>0.02]
#===========================================================================================

#============================================================================================
ggplot(data=ap_doc, aes(x=document, y=topic)) + geom_tile(aes(fill=gamma)) + coord_flip() +
  scale_fill_gradient(high="red", low="white")
#============================================================================================