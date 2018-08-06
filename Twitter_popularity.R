library(tidyverse)
library(lubridate)
library(RSelenium)
library(rvest)

# NOTE: in order for this script to work, you need to have installed Docker on your computer (https://www.docker.com)

# this function scrapes the data and returns a data frame with the mean number of retweets and the mean number of likes
get_popularity_data <- function(username, start, end) {
  # construct the url with the right search query
  url <- paste0("https://twitter.com/search?q=exclude%3Areplies%20from%3A",
                username,
                "%20since%3A",
                start,
                "%20until%3A",
                end,
                sep = "")
  
  # navigate to url
  rd$navigate(url)
  
  # scroll down to be sure to have the full page
  # NOTE: this is a hack and there is probably a cleaner way to do it
  last_page_length <- 0
  page_length <- as.integer(rd$executeScript("return document.body.scrollHeight;"))
  while(page_length != last_page_length) {
    last_page_length <- page_length
    rd$executeScript("window.scroll(0, document.body.scrollHeight);")
    Sys.sleep(1)
    page_length <- as.integer(rd$executeScript("return document.body.scrollHeight;"))
  }
  
  # get the page's source code and read it with rvest
  html <- rd$getPageSource()[[1]] %>% read_html()
  
  # get the mean number of retweets
  mean_retweets <- html %>%
    html_nodes(".ProfileTweet-actionCountList") %>%
    html_nodes(".ProfileTweet-action--retweet") %>%
    html_nodes(".ProfileTweet-actionCount") %>%
    html_attrs() %>%
    map("data-tweet-stat-count") %>%
    as.integer() %>%
    mean()
  
  # get the mean number of likes
  mean_likes <- html %>%
    html_nodes(".ProfileTweet-actionCountList") %>%
    html_nodes(".ProfileTweet-action--favorite") %>%
    html_nodes(".ProfileTweet-actionCount") %>%
    html_attrs() %>%
    map("data-tweet-stat-count") %>%
    as.integer() %>%
    mean()
  
  # create a data frame with the data I just scraped
  tribble(
    ~period, ~mean_retweets, ~mean_likes,
    paste0(start, " to ", end, sep = ""), mean_retweets, mean_likes
  )
}

# run Selenium on Docker (which needs to be already running)
system("docker run -d -p 4445:4444 selenium/standalone-chrome")

# start RSelenium
rd <- remoteDriver(remoteServerAddr = "localhost",
                   port = 4445L,
                   browserName = "chrome")

# open a session
rd$open()

# ask user for the Twitter account whose tweets he wants to analyze
username <- readline(prompt = "Enter Twitter account: ")

# ask user for start and end dates
start <- readline(prompt = "Enter start date in the yyyy-mm-dd format: ")
end <- readline(prompt = "Enter end date in the yyyy-mm-dd format: ")

period_length <- as.integer(readline(prompt = "Enter number of days by period: "))

# generate a sequence of dates corresponding to the first day of each week between start and end
start_dates <- seq(ymd(start), ymd(end), by = paste0(as.character(period_length), " days"))

# construct the list of arguments for pmap_df
# NOTE: I need to add a day to the end date of each period because Twitter's search excludes the value of 'since'
args <- list(username = username,
             start = as.character(start_dates),
             end = ifelse(start_dates + period_length - 1 < end,
                          as.character(start_dates + period_length),
                          as.character(ymd(end) + 1)))

# scrape the data
popularity_data <- pmap_df(args, get_popularity_data)

# popularity_data %>%
#   gather(key = type, value = mean_number, -week) %>%
#   mutate(type = factor(type)) %>%
#   ggplot(mapping = aes(x = week, y = mean_number, group = 1)) +
#   geom_line(color = "blue") +
#   theme_bw() +
#   ggtitle(paste0("Mean number of retweets and likes for ", args$username, " between ", start, " and ", end, sep = "")) +
#   xlab("Week") +
#   ylab("Mean number") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(~type, nrow = 2, scales = "free")

rt_plot <- popularity_data %>%
  ggplot(mapping = aes(x = period, y = mean_retweets, group = 1)) +
  geom_line(color = "blue") +
  theme_bw() +
  ggtitle(paste0("Mean number of retweets for ",
                 username,
                 " between ",
                 start,
                 " and ",
                 end,
                 " (",
                 period_length,
                 "-day periods)",
                 sep = "")) +
  xlab("Period") +
  ylab("Mean number of retweets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  ggsave(paste0(rt_plot$labels$title, ".png"), width = 12, height = 8)

lk_plot <- popularity_data %>%
  ggplot(mapping = aes(x = period, y = mean_likes, group = 1)) +
  geom_line(color = "blue") +
  theme_bw() +
  ggtitle(paste0("Mean number of likes for ",
                 username,
                 " between ",
                 start,
                 " and ",
                 end,
                 " (",
                 period_length,
                 "-day periods)",
                 sep = "")) +
  xlab("Period") +
  ylab("Mean number of likes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  ggsave(paste0(lk_plot$labels$title, ".png"), width = 12, height = 8)

# close the session
rd$close()

# shut down the server on Docker
system("docker stop $(docker ps -q)")
