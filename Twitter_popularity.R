library(tidyverse)
library(lubridate)
library(RSelenium)
library(rvest)

# NOTE: in order for this script to work, you need to have installed Docker on your
# computer (https://www.docker.com) and it has to be running before you run the script

# this function scrapes the data and returns a data frame with the mean number of retweets and the mean number of likes
get_popularity_data <- function(rd, username, start, end) {
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
  median_retweets <- html %>%
    html_nodes(".ProfileTweet-actionCountList") %>%
    html_nodes(".ProfileTweet-action--retweet") %>%
    html_nodes(".ProfileTweet-actionCount") %>%
    html_attrs() %>%
    map("data-tweet-stat-count") %>%
    as.integer() %>%
    median()
  
  # get the mean number of likes
  median_likes <- html %>%
    html_nodes(".ProfileTweet-actionCountList") %>%
    html_nodes(".ProfileTweet-action--favorite") %>%
    html_nodes(".ProfileTweet-actionCount") %>%
    html_attrs() %>%
    map("data-tweet-stat-count") %>%
    as.integer() %>%
    median()
  
  # create a data frame with the data I just scraped
  tribble(
    ~period, ~median_retweets, ~median_likes,
    paste0(start, " to ", end, sep = ""), median_retweets, median_likes
  )
}

# run Chrome with Selenium on Docker, which needs to be already running (I use the option -v /dev/shm:/dev/shm because
# otherwise Chrome often crashes, cf. https://github.com/SeleniumHQ/docker-selenium/issues/79#issuecomment-133083785 for
# the explanation)
system("docker run -v /dev/shm:/dev/shm -d -p 4445:4444 selenium/standalone-chrome")

# start RSelenium
rd <- remoteDriver(remoteServerAddr = "localhost",
                   port = 4445L,
                   browserName = "chrome")

# open a session (for some reason, this often doesn't work the first time, but does after I run the script again)
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
# NOTE: I need to add a day to the end date of each period because Twitter's search excludes the value of 'until'
args <- list(
             rd = c(rd),
             username = username,
             start = as.character(start_dates),
             end = ifelse(start_dates + period_length - 1 < end,
                          as.character(start_dates + period_length),
                          as.character(ymd(end) + 1))
             )

# scrape the data
popularity_data <- pmap_df(args, get_popularity_data)

# plot the evolution of the mean number of retweets between start and end
rt_plot <- popularity_data %>%
  ggplot(mapping = aes(x = period, y = median_retweets, group = 1)) +
  geom_line(size = 1, color = "blue") +
  theme_bw() +
  ggtitle(paste0("Median number of retweets for ",
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
  ylab("Median number of retweets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# plot the evolution of the mean number of likes between start and end
fav_plot <- popularity_data %>%
  ggplot(mapping = aes(x = period, y = median_likes, group = 1)) +
  geom_line(size = 1, color = "blue") +
  theme_bw() +
  ggtitle(paste0("Median number of likes for ",
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
  ylab("Median number of likes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# save the plots
ggsave(paste0(rt_plot$labels$title, ".png"), rt_plot, width = 12, height = 8)
ggsave(paste0(fav_plot$labels$title, ".png"), fav_plot, width = 12, height = 8)

# close the session
rd$close()

# shut down the server on Docker
system("docker stop $(docker ps -q)")
