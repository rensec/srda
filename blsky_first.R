library(bskyr)
library(tidyverse)

# tutorial from https://christophertkenny.com/bskyr/articles/gathering_data.html,
# https://christophertkenny.com/bskyr/


# Functions

# Create a quiet version of bs_get_profile() for convenience, otherwise it will write a lot of useless output to the console
# Uses quietly from the purrr package
# Returns results in bs_get_profile_quiet$result
bs_get_profile_quiet <- quietly(bs_get_profile)

# same for bs_get_followers()
bs_get_followers_quiet <- quietly(bs_get_followers)

# A function to fetch the profile of a user, retrying on failure
get_profile_robust <- function(actor, auth, max_attempts = 5){
  for (k in c(1:max_attempts)) {
    profile <- try(bs_get_profile_quiet(actor = actor, auth = auth)$result)
    if (!(inherits(profile,"try-error"))) {
      #break
      return(profile)
    } else {
      print(paste("error; trying again to fetch profile..."))
    }
  }
  # print error message
  print(paste("error; could not fetch profile, max attempts reached"))

}

p <- get_profile_robust("rensec.bsky.social", auth)

# same for followers

get_followers_robust <- function(actor, auth, limit, max_attempts = 5){
  for (k in c(1:max_attempts)) {
    followers <- try(bs_get_followers(actor = actor, auth = auth, limit = limit))
    if (!(inherits(followers,"try-error"))) {
      #break
      return(followers)
    } else {
      print(paste("error; trying again to fetch followers..."))
    }
  }
  # print error message
  print(paste("error; could not fetch followers, max attempts reached"))

}

# the metropolis hasting algorithm; avoids bias towards high-degree nodes
# see Gjoka et al., Walking in Facebook http://www.minasgjoka.com/papers/unbiasedsampling-infocom2010.pdf
sample_mh <- function(follower_list, auth){
  k_v <- length(follower_list)

  success <- 0
  attempts <- 0
  while(success == FALSE & attempts < k_v){
    #Select node w uniformly at random from neighbors of v.
    w <- sample(follower_list, 1)
    profile_w <- bs_get_profile_quiet(actor = w, auth = auth)$result
    k_w <- as.numeric(profile_w$followers_count)

    #Generate uniformly at random a number 0 ≤ p ≤ 1.
    p <- runif(1) # CHECK

    if (p <= k_v/k_w) {
      success <- TRUE
    } else {
      attempts <- attempts + 1
      message("resampling...")
    }
  }
  if (success == TRUE) {
    return(profile_w)
  } else {
    return(NULL)
  }
}




# authenticate

pwd = readLines("bsky.pwd")

set_bluesky_user("rensec.bsky.social")
set_bluesky_pass(pwd)

# read the password from a file



auth <- bs_auth(user = bs_get_user(), pass = bs_get_pass())

user <- 'rensec.bsky.social'
profile <- bs_get_profile(actor = user, auth = auth)
profile <- bs_get_profile_quiet(actor = user, auth = auth)$result
followers_count <- as.numeric(profile$followers_count)

followers <- bs_get_followers(actor = user, auth = auth, limit = followers_count)


# create a data frame with handle and number of followers
#users_followcount <- cbind.data.frame(user,followers_count)

set.seed(53657)
max_users <- 100
maxattempts <-5


# A random walk

# create an empty data frame with the columns "user" and "followers_count"
users_followcount <- data.frame(user = character(), followers_count = numeric())
next_user <- user

while(nrow(users_followcount) < max_users){

  print(paste("next_user: ", next_user))

  # get the user's profile
  profile <- get_profile_robust(next_user, auth)

  # get the number of followers
  followers_count <- as.numeric(profile$followers_count)
  # print the number of followers
  print(paste("followers_count: ", followers_count))

  # If the number of followers is zero, skip the user
  if (followers_count == 0) {
    next_user <- sample(followers$handle, 1)
    next
  }

  # add the data to the data frame
  users_followcount <- rbind(users_followcount, cbind.data.frame(next_user,followers_count))

  # get the followers of the user
  # try again if it creates an error
  for (k in c(1:maxattempts)) {
    followers <- try(bs_get_followers(actor = next_user, auth = auth, limit = followers_count))
    if (!(inherits(followers,"try-error"))) {
      break
    } else {
      print(paste("error; trying again to fetch followers..."))
    }
  }

  # sample the next user
  next_user <- sample(followers$handle, 1)


  # print the number of rows in the data frame
  print(nrow(users_followcount))
}

hist(users_followcount$followers_count)




posts <- bs_search_posts(query = "bezuinigingen")






# extract all the texts from the posts
text <- sapply(posts$record, function(x) x$text) %>%
  as_tibble()

author_handle <- sapply(posts$author, function(x) x$handle)  %>%
  as_tibble()

mypost <- posts[11]


profile <- bs_get_profile(actor = "rensec.bsky.social", auth = auth)
followers <- get_followers_robust("rensec.bsky.social", auth, limit = as.numeric(profile$followers_count), max_attempts = 2)
pp <- sample_mh(followers$handle, auth)
pp$followers_count
