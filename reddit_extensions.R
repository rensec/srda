rm(list=ls()) # Start with a clean workspace
library(RedditExtractoR)
library(tidyverse)
library(igraph)


x <- readLines("https://www.reddit.com/r/climate/new.json?limit=1&t=all")

x


threads <- find_thread_urls(subreddit="climate", sort_by="new", period = "all")

thread_contents <- threads$url  # start with the urls from the threads data frame
thread_contents <-  sample(thread_contents, 50) # randomly sample 50
thread_contents <-  get_thread_content(thread_contents)

authors <- thread_contents$threads %>% # Get the "threads" part of the threat_contents object
  select(author, url) # keep only the author and url for each thread

responders <- thread_contents$comments %>% # Get the "comments" part of the threat_contents object
  select(author, url) %>% # keep only the author and url for each thread
  rename("responder" = author) # rename "author" to "responder"

# now match these two together, using the url as the matching variable
reply_net  <- merge(authors, responders, by = "url") %>%
  select(author, responder) %>% # keep only the author and responder vars
  graph_from_data_frame() # turn this into a "network object": something the igraph package for network analysis can work with

# plot the network
plot(reply_net,
     vertex.label=NA,
     vertex.color = "blue",
     vertex.size = 3,
     edge.arrow.size = 0.2,
     edge.color = "black",
     graph.frame = TRUE,
     main = "The reply network of 50 random threads on r/climate"
)

# do a simple community detection on reply_net
communities <- cluster_edge_betweenness(reply_net)

# make an undirected version of the network
reply_net_undir <- as_undirected(reply_net)

# do community detection on the reply network using the Louvain algorithm
communities <- cluster_louvain(reply_net_undir, resolution = .2)

# plot the network with the communities colored
plot(communities, reply_net,
     vertex.label=NA,
     vertex.color = "blue",
     vertex.size = 3,
     edge.arrow.size = 0.2,
     edge.color = "black",
     graph.frame = TRUE,
     main = "The reply network of 50 random threads on r/climate"
)

# do a simple sentiment analysis on the comments in thread_contents
# load the package
library(sentimentr)
# get the sentiment of the comments
# create a data frame of thread_contents$comments
# and then use the sentiment_by function to get the sentiment of each comment
comments <- thread_contents$comments
# create a variable to store the sentiment of each comment
sentiments <- sentiment_by(comments$comment)

comments$sentiment <- sentiments$ave_sentiment


# plot the average sentiment per day
comments %>%
  group_by(date) %>%
  summarise(ave_sentiment = mean(sentiment)) %>%
  ggplot(aes(x = date, y = ave_sentiment, group = 1)) +
  geom_point() +
  geom_line() +
  labs(title = "Average sentiment of comments per day",
       x = "Time",
       y = "Sentiment")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create a data frame with the average sentiment per date
sentiment_per_date <- comments %>%
  group_by(date) %>%
  summarise(ave_sentiment = mean(sentiment))

# plot the average sentiment per day as a line
ggplot(sentiment_per_date, aes(x = date, y = ave_sentiment, group = 1)) +
  geom_point() +
  geom_line() +
  labs(title = "Average sentiment of comments per day",
       x = "Time",
       y = "Sentiment")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
