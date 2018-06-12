### Future tasks
# 1. Sentiment analysis
# 2. Assign metadata



#0. Packages needed
##############################
Needed <- c("topicmodels","RTextTools","dplyr","rvest","stringr","tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc", "slam")   

lapply(Needed, require, character.only = TRUE)


####### Function for stem
stemDocumentfix <- function(x)
{
  PlainTextDocument(paste(stemDocument(unlist(strsplit(as.character(x), " "))),collapse=' '))
}

#1. Finding the title and the date for each author
###############################################
b1<-c()
b1[1]<- "http://profusion.com/blog/"
for(i in 2:10){
  b1[i] <-paste0("http://profusion.com/blog/page/",i,"/")  
}

b2 <-list()
title_date <-list()
for(i in 1:10){
  b2[[i]] <- read_html(b1[i])
  title_date[[i]] <- b2[[i]] %>%
    html_nodes(".blog-title a , .date") %>%
    html_text()  
}

title_d <- c(title_date[[1]],title_date[[2]],title_date[[3]],title_date[[4]],title_date[[5]],title_date[[6]],title_date[[7]],title_date[[8]],title_date[[9]],title_date[[10]])

title <-seq(1, length(title_d), by=2)
date <-seq(2, length(title_d), by=2)


df1 <-data.frame(title=title_d[title], timestamp=title_d[date])

# test$year <-str_sub(test$timestamp,start=-4, end=-1)
# test$day <-str_sub(test$timestamp,start=-8, end=-7)
# test$day <- sprintf("%02d", as.numeric(test$day))

df1$ts <-str_sub(df1$timestamp,start=11, end=nchar(as.character(df1$timestamp)))
df1$ts <-strptime(df1$ts, format="%B %d, %Y")
#####################################################################################
Whta 

#2. Finding the url of each blog
###################################################################
title_lower <- tolower(df1$title) #to lower case
title_clean <- str_replace_all(as.character(title_lower), pattern = "[\\( \\‘ \\’ \\?  *\\ !\\ …\\ :\\ –\\ ,\\ \\)]", " ") #remove special characters
title_clean1 <-trimws(title_clean) #trim white spaces
title_clean2 <-str_replace(gsub("\\s+", " ", str_trim(title_clean1)), "B", "b") #trim multiple white spaces

#manual
title_clean2[15] <-"data science can prove prs worth"
title_clean2[20] <-"a statistics walk through"
title_clean2[38] <-"whats in store for wearables data and marketing"
title_clean2[48] <- "the survivors guide to digital marketing internships"      
title_clean2[54]<-"dont fall at the last hurdle the essential rules of copy"

title_clean_web <-str_replace_all(title_clean2, " ", "-")

date_web <-paste0(substr(df1$ts,1,4),"/",substr(df1$ts,6,7), "/",substr(df1$ts,9,10),"/")

url <-c()
for(i in 1:length(date_web)){
  url[i] <-paste0("http://profusion.com/",date_web[i],title_clean_web[i],"/")
}

#3. Finding the content of each blog
###################################################################
c1<-list()

content <-list()
for(i in 1:length(url)){
  c1[[i]] <- read_html(url[i])
  content[[i]] <- c1[[i]] %>%
    html_nodes("h1 , h3, .contentline01 li, .contentline01 p") %>%
    html_text()  
}

blogs <-c()
for(i in 1:length(content)){
  blogs[i] <- paste(content[i])
  blogs[i] <- gsub("[\r\n]", " ",blogs[i] ) #remove carriage returns and new lines
  blogs[i] <- substr(blogs[i], start = 4, stop = nchar(blogs[i])- 20) #remove "c(" and  "Join Profusion\")"
}


#4. Preprocessing
#############################################
 

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(blogs))

# A. Transforming Text
myCorpus <- tm_map(myCorpus, tolower) #to lower case
myCorpus <- tm_map(myCorpus, removePunctuation) ## remove punctuation
myCorpus <- tm_map(myCorpus, removeNumbers) ## remove numbers

# B. Removing “stopwords” (common words) that usually have no analytic value, eg  a, and, also, the, etc.).
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))   

# C.Stemming Words
myCorpus <- tm_map(myCorpus, stemDocumentfix)
#myCorpus <- tm_map(myCorpus, stemDocument,language = "english")

# D. Stripping unnecesary whitespace from your documents
myCorpus <- tm_map(myCorpus, stripWhitespace)

# E. This tells R to treat your preprocessed documents as text documents.
myCorpus <- tm_map(myCorpus, PlainTextDocument) 


#5. Explore your data
#############################################

#Create a DocumentTermMatrix
dtm <- DocumentTermMatrix(myCorpus)

# Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)  

#most and least frequently occurring words.
freq[head(ord)]   
freq[tail(ord)]   

#Check out the frequency of frequencies.
head(table(freq), 20)   


dtms <- removeSparseTerms(dtm, 0.5) # This makes a matrix that is 50% empty space, maximum.   

freqs <- colSums(as.matrix(dtms))   
freqs   

wf <- data.frame(word=names(freqs), freq=freqs)   
head(wf)  

#Plot Word Frequencies
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   



#Relationships Between Terms

findAssocs(dtm, c("data" , "scienc"), corlimit=0.5) # specifying a correlation limit of 0.5   
findAssocs(dtm, "data", corlimit=0.6)
findAssocs(dtm, "science", corlimit=0.6)
findAssocs(dtm, "will", corlimit=0.6)
findAssocs(dtm, "people", corlimit=0.6)
findAssocs(dtm, "work", corlimit=0.6)

#Word Clouds!
  
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)   

set.seed(142)   
wordcloud(names(freq), freq, max.words=100)  

set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=400, rot.per=0.2, colors=dark2)   

#6. Clustering by Term Similarity
#####################################
dtmss <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is only 15% empty space, maximum.   
#inspect(dtmss)   


# A. Hierarchal Clustering
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
fit 

# Dendrogram
plot(fit, hang=-1) 
# Helping to Read a Dendrogram
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red") # draw dendogram with red borders around the 4 clusters   

# B. K-means clustering

d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 4)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   



#7. Topic Modelling
#####################################

# Perform Latent Dirichlet Allocation
lda_AP <- LDA(dtm, 4) 
get_terms(lda_AP, 4)                         # gets 5 keywords for each topic, just for a quick look
get_topics(lda_AP, 4)                        # gets 5 topic numbers per document
# View the Results
#terms(lda_AP)

terms <- posterior(lda_AP)$terms

topics<-list()
for(i in 1:4){
  topics[[i]]<-t(sort(terms[i,], T)[1:10])
}

topics


####################################################
#c(2,5,10, 20, 100)

best.model <- lapply(c(2:100), function(k){LDA(dtm, k)})

best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

Optimal number of topics

