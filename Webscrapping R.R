#LOding The rvest Package
library(rvest)
library(dplyr)
# to specify  the website to be scrapped
URL <- ('https://www.airlinequality.com/airline-reviews/british-airways/?sortby=post_date%3ADesc&pagesize=100')

  
#Read code From website
webpage <- read_html(URL)

Flight_number = webpage %>% html_node('.aircraft+ .review-value')%>% html_text()

date = webpage %>% html_node("time")%>%html_text()

text_rating = webpage%>% html_node('.text_header')%>%html_text()

Destination = webpage%>% html_node('.userStatusWrapp')%>%html_text()

seat_type = webpage%>%html_node(".cabin_flown+ .review-value")%>%html_text()
#convert the extracted data into a data frame
flight_df <- data.frame(Flight_number, date, text_rating,Destination,seat_type,
                        stringsAsFactors = FALSE)
write.csv(flight_df,"flight_df.csv")

#load library
library(syuzhet)
library(ggplot2)
library(plotly)
library(plyr)
library(wordcloud)
library(tm)
library(tidyverse)
library(lessR)
library(dplyr)
library(psych)

#read file
library(readxl)
airlinequality_2 <- read_excel("C:/Users/Elite/Downloads/airlinequality 2.xlsx")
View(airlinequality_2)

#restored data in charater format
review1 <-as.character(airlinequality_2$text_review)

#obtain setiment scores
get_nrc_sentiment("happy")
get_nrc_sentiment("abuse")

#store data into new variable

flight1<-get_nrc_sentiment(review1)

#combine text and sentiment column
review_sentiment1 <- cbind(airlinequality_2$text_review)

#barplot for sentiment analysis
barplot(colSums(flight1), col = rainbow(10),ylab ="count", 
        main = 'British Airline Flight Review')


Corpus <- Corpus(VectorSource(airlinequality_2$text_content))
Corpus[[1]] [1]

#covert text to lowercase
Corpus <- tm_map(Corpus,content_transformer(tolower))
#REMOVE NUMBERS
Corpus <- tm_map(Corpus,removeNumbers)
#REMOVE PUTATUATIONS
Corpus <- tm_map(Corpus,removePunctuation)

#ELIMINATE  EXTRA WHITE SPACE
Corpus <- tm_map(Corpus,stripWhitespace)

#REMOVE COMMON ENGLISH STOP WORDS
Corpus <- tm_map(Corpus,removeWords,stopwords('english'))
#creae tdm
tdm <- TermDocumentMatrix(Corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame( word = names(v), freq = v)



wordcloud(d$word,d$freq,
          random.order = TRUE, rot.per = 0.3,
          scale=c(4,.5),max.words = 120,
          colors = brewer.pal(8,"Dark2"))
#to export csv file

write.csv(flight1,file = 'Flight1.csv')
save(flight1,file = "flight1.RData")

#import customer booking
library(readr)
customer_booking <- read_csv("C:/Users/Elite/Downloads/customer_booking.csv")
View(customer_booking)

#read library

library(tidyverse)
library(lessR)
library(dplyr)
library(psych)
library(plyr)



#predictive analysis
attach(customer_booking)
Crust_value_model = lm(formula =  purchase_lead~wants_extra_baggage 
                       + wants_in_flight_meals + wants_preferred_seat , data 
                       = customer_booking)
#get model residual
model_residual = Crust_value_model$residuals
#plot result
hist(model_residual)

#plot residual
qqnorm(model_residual)

#plor Q-Qline
qqline(model_residual)
