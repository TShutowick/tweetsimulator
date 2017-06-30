require(twitteR)
setup_twitter_oauth(YOUR_CREDENTIALS_HERE)

#1. setup your twitter
#2. run getTweets for desired user. Usually takes a minute.
#3. run finaltweet. Usually takes a second.


getTweets<-function(user, n=1000)
  {
  #userTimeline returns a list of environments
  #convert each one to a data frame and keep only the first cell
  #now it's a list of tweet character vectors
  
  tweets<-unlist(lapply(userTimeline(user,n), 
          function(x) as.data.frame(x)[1,1]))
  
  #split vectors by words
  tweets<-sapply(tweets,function(x) strsplit(x," "))
  
  #Adding "ENDTWEET" to the end of each tweet lets the tweetgen function know when to stop
  tweets<-sapply(1:length(tweets),function(x) c(tweets[[x]],"ENDTWEET"))

  #collapse list of tweets into single character vector
  words<-unlist(tweets)
  
  #before removing punctuation, separate links so you dont end up with a bunch of messed up urls as words
  links<<-words[grep("http",words,ignore.case=T)]
  
  #tweetgen will replace links at random
  words[grep("http",words,ignore.case=T)]<-"ITSALINK"
  
  #get rid of punctuation so gsub works properly
  #tweetgen will replace them
  words<-gsub("! "," EXPNT ",words)
  words<-gsub("\\.","FULSTOP",words)
  words<-gsub(", "," COMMA ",words)
  words<-gsub("\\n"," ",words)
  words<-gsub(" ","",words)
  

  #remove leftover punctuation, leaving in the # symbol
  words<-gsub("[^a-zA-Z0-9#]","",words)
  
  #vector of unique words
  uniqueWords<-unique(words)

  #locations of unique words in words vector
  locs<-sapply(uniqueWords, function(x) grep(x,words,ignore.case = T))
  
  #for each occurence of each unique word, get the next word
  nextwords<-sapply(locs, function(x) words[x+1])
  
  #move the unique words and next words vectors to the environment
  uniqueWords<<-uniqueWords
  nextwords<<-nextwords
  
}


nextwordgen<-function(current)
{
  #location of current word in unique word vector
  currentloc<-which(uniqueWords==current)
  
  #potential next words
  potnext<-nextwords[[currentloc]]
  
  #random word from potnext
  sample(potnext,1)
}


tweetgen<-function()
{
  #random first word
  first<-nextwordgen("ENDTWEET")
  current<-first
  tweetout<-first
  
  #keep adding new words to tweetout until ENDTWEET is reached
  while(current!="ENDTWEET")
  {
    current<-nextwordgen(current)
    tweetout<-c(tweetout,current)
  }
  
  #collapse into a single character vector
  tweetout<-paste(tweetout,collapse=" ")
  
  #replace punctuaction and links
  tweetout<-gsub("ITSALINK",sample(links,1),tweetout)
  tweetout<-gsub(" ENDTWEET","",tweetout)
  tweetout<-gsub(" EXPNT ","! ",tweetout)
  tweetout<-gsub("FULSTOP","\\.",tweetout)
  tweetout<-gsub(" \\.","\\. ",tweetout)
  tweetout<-gsub(" COMMA ",", ",tweetout)
  tweetout<-gsub("LINEBREAK"," ",tweetout)
  tweetout<-gsub("&amp;","&",tweetout)
  tweetout<-gsub("  "," ",tweetout)


  
  #short tweets are boring, and tweets must be under 140 characters
  if (nchar(tweetout)>140 || nchar(tweetout)<50)
    return(0)
  tweetout
  
}
                    

finaltweet<-function()
{
  tweetout<-0
  #Keep running tweetgen until it works
  while(tweetout==0)
  {
    tweetout<-tweetgen()
  }
  tweetout
}
