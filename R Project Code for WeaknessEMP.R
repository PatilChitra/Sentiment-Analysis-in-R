library(dplyr) # dplyr package is used for data manipulation; it uses pipes
library(tm) # contains the stopwords dictionary
library(textstem) # used for stemming and lemmatization
library(tidyverse) # data manipulation & plotting
library(tidytext) # provides additional text mining functions
library(ggplot2)
library(hunspell)

library(readxl)
GoalsESP <- read_excel("C:/MIS/data/AY 2019-BBE-ESP Free Response.xlsx", 
                                      sheet = "Goals")
View(GoalsESP)
str(GoalsESP)
names(GoalsESP) <- c('ID','Terms','college','MJ', 'Co-OP','Class','Citizenship','coop-n','Goal') 
#renaming rows for convinience - 
head(GoalsESP)
GoalsEsP <- subset(GoalsESP, select = -c(college,MJ)) #removing columns which are not relevant
head(GoalsESP)
view(GoalsESP)

#####transforming to lower case#####

GoalsESP <- GoalsESP %>% mutate(Goal_low = tolower(Goal))
head(GoalsESP)

####remove punctuation####

GoalsESP <- GoalsESP %>% mutate(Goal_nopunc = gsub("[[:punct:]]","",Goal_low))
view(GoalsESP)

####remove stopwords####
stopwords('en')
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
View(stopwords_regex)
GoalsESP <- GoalsESP %>% mutate(Goal_no_stop_word = gsub(stopwords_regex,' ',Goal_nopunc))
head(GoalsESP)

####remove numbers####
GoalsESP <- GoalsESP %>% mutate(Goal_nonumb = gsub('[[:digit:]]',' ',Goal_no_stop_word))
GoalsESP <- GoalsESP %>% mutate(Goal_noword = gsub('students',' ',Goal_nonumb))
GoalsESP <- GoalsESP %>% mutate(Goal_noword = gsub('student',' ',Goal_noword))
GoalsESP <- GoalsESP %>% mutate(Goal_noword = gsub('take',' ',Goal_noword))
Na_regex = paste("na", collapse = '\\b|\\b')
Na_regex
view(GoalsESP)

####remove whitespace####
GoalsESP <- GoalsESP %>% mutate(Goal_WHTSPC = gsub('\\s+',' ',Goal_noword))
View(GoalsESP)

####Stemming#####
GoalsESP <- GoalsESP %>% mutate(Goal_Lem = lemmatize_strings(Goal_WHTSPC))
GoalsESP <- GoalsESP %>% mutate(Goal_Lem2 = gsub('[[:digit:]]',' ',Goal_Lem))
GoalsESP <- GoalsESP %>% mutate(Goal_Lem_Final = gsub('\\s+',' ',Goal_Lem2))

####Lemming####
Text <- GoalsESP %>% select(Goal_Lem_Final)
write.csv(Text,"Goal_final.csv", row.names = FALSE)
write.csv(GoalsESP,"Goal_ALL.csv",row.names=FALSE)
view(Text)
my_text1 <- Text %>% unnest_tokens(word,Goal_Lem_Final)
view(my_text1)
my_text <- Text %>% mutate(linenumber=row_number())
Text_bigrams <- my_text %>% 
  unnest_tokens(bigram, Goal_Lem_Final, token = "ngrams", n = 2)
Text_bigrams
Text_ngrams <- my_text %>% 
  unnest_tokens(bigram, Goal_Lem_Final, token = "ngrams", n = 3)
view(my_text)
Text_ngrams
write.csv(Text_bigrams,"text_bigrams.csv",row.names= FALSE)
write.csv(Text_ngrams,"text_ngarm.csv",row.names=FALSE)





install.packages("sentimentr")
library(sentimentr)
GA <- sentiment(GoalsESP$Goal_Lem_Final)
view(GA)
sum(GA$sentiment)
plot(GA$sentiment, type = "l")
Goal <- sentiment(GoalsESP$Goal)
view(SAS)
sum(SA$sentiment)
plot(SAS$sentiment, type = "l")
sentiment
