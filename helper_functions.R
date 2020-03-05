
# download book by gutenberg book id and save it to diretory
downloadGutenberg <- function(bookid, dirpath = ""){
        for (id in bookid){
                book <- gutenberg_download(id,  meta_fields = "title")
                booktitle <- gsub(" ","", book$title[1]) %>% replace_symbol() %>% 
                        replace_white() %>% substr(1, 30)
                if (nrow(book) < 10) {
                        book <- gutenberg_download(id)
                        booktitle <- as.character(id)
                        
                }
                path <- paste(dirpath, booktitle, ".txt", sep = "")
                print(path)
                write.csv(book$text, path)
                print("completed")
        }
       "Download Complete"
}



# readData will read text files from given path and clean the data
readData <- function(path, readFiles = FALSE) {
        #read data from directory using package:tm
        if (!readFiles) docs <- VCorpus(DirSource(path))
        #clean the data 
        cleanData(docs)
}


cleanData <- function(docs){
        #data as tibble using package:tidytext
        docs <- tidy(docs)
        
        #clean the data using package:textclean
        docs$text <-  replace_non_ascii(docs$text, remove.nonconverted = FALSE)
        docs$text <- replace_contraction(docs$text)
        docs$text <- gsub('[[:punct:] ]+',' ',docs$text)
        docs$text <- replace_number(docs$text, remove=TRUE)
        docs$text <- replace_ordinal(docs$text, remove = TRUE)
        docs$text <- replace_white(docs$text)
        
        #return
        docs
        
}


# helper function for function word
# vector to dataframe
vecToDF <- function(vec, colname){
        vec <- data.frame(vec)
        colnames(vec) <- colname
        vec[,s] <- as.character(vec[,s])
        vec
}

# find the probability distribution of given_words which is a dataframe
findDistribution <- function(docs, given_words=func.word, colname = "word"){
        # tokenize the words using package:tidytext
        tidydocs <- docs %>% unnest_tokens(word, text) %>%
                count(word, sort = T)
        
        # find the frequency of the given words
        freq <- tidydocs %>% semi_join(given_words, colname)
        
        # determine probability
        freq$n <- freq$n / sum(freq$n)
        
        #return
        freq
}

# Find Hm for different sample. m is the sample size here
sampleDist = function(dist, m, n) { 
        words <- as.character(unlist(dist[,1]))
        prob <- as.numeric(unlist(dist[,2]))
        Hm <- rep(0, n)
        for (i in 1:n){
                # take sample from the dist
                sampleWords <- sample(words, m, replace = T, prob)
                
                #find the different word in that sample
                Hm[i] <- length(table(sampleWords))
        }
        Hm
}




findE <- function(Hm1, Hm2, n, alpha){
        s1 <- sd(Hm1)
        s2 <- sd(Hm2)
        qt(1-alpha/2, n-1)*sqrt((s1**2 + s2**2)/n )
}

findE.new <- function(Hm1, Hm2, n, alpha){
        nu <- ((var(Hm1)/n+var(Hm2)/n)**2)/(((var(Hm1)/n)**2)/(n-1) + 
                                                    ((var(Hm2)/n)**2)/(n-1)) 
        tval <- qt(1-alpha/2, nu)
        tval * sqrt(var(Hm1)/n + var(Hm2)/n)
}


# Function for the main work
confintPlot <- function(dist1, dist2, m = 50, n = 1000, alpha = 0.05, label = ""){
        x=1:m
        dist1.Hm <- rep(0, m)
        dist2.Hm <- rep(0,m)
        E <- rep(0, m)
        
        for (i in x){
                Hm1 <- sampleDist(dist1, i, n)
                Hm2 <- sampleDist(dist2, i, n)
                dist1.Hm[i] <- mean(Hm1)
                dist2.Hm[i] <- mean(Hm2)
                E[i] <- findE(Hm1, Hm2, n, alpha)
        }
        
        # Get CI
        CIu <- dist1.Hm - dist2.Hm + E
        CId <- dist1.Hm - dist2.Hm - E
        
        
        # plot confidence interval using ggplot2 package
        ggplot() + geom_smooth(aes(x=x, y=CIu),color='blue', method = "loess", se=F) + 
                geom_smooth(aes(x=x,y=CId),color='blue', method = "loess", se=F) + 
                geom_smooth(aes(x=x,y=rep(0,m)),color='black', method = "loess", se=F) + 
                labs(title=label, x="m", y="Difference") +
                theme(plot.title = element_text(color="#666666"))
}


#Create plot of two distribution
createPlot <- function(dist1, dist2, m = 50, n = 1000, alpha = 0.05, label = "", 
                       mstart= 1 ){
        x=mstart:m
        dist1.Hm <- rep(0, m)
        dist2.Hm <- rep(0,m)
        E <- rep(0, m)
        
        for (i in x){
                Hm1 <- sampleDist(dist1, i, n)
                Hm2 <- sampleDist(dist2, i, n)
                dist1.Hm[i] <- mean(Hm1)
                dist2.Hm[i] <- mean(Hm2)
                E[i] <- findE(Hm1, Hm2, n, alpha)
        }
        
        # Get CI
        CIu <- dist1.Hm - dist2.Hm + E
        CId <- dist1.Hm - dist2.Hm - E
        
        
        length(x)
        length(CIu)
        
        
        # create plot
        ggplot() + geom_line(aes(x=x, y=CIu),color='red') + 
                geom_line(aes(x=x,y=CId),color='blue') + 
                ylab('Values')+xlab('m')
        # plot(x, CIu, type='l', ylim = c(-1,1))
        # lines(x, CId)
        # lines(x, rep(0,m))
        # title(label)
}
