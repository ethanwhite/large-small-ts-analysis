get_data <- function(){
  data <- read.csv("surveys.csv")
}

get_size_class <- function(weight, threshold){
  if (weight > threshold){
    size_class = "large"
  } else {
    size_class = "small"
  }
  return(size_class)
}

get_size_class_ts_data <- function(df){
  # Convert individual data to time-series data for each of a set of size classes
  # Input: data frame with a year column for time
  #        and a size_class column
  ts_data <-
    df %>% 
    group_by(year, size_class) %>% 
    summarize(counts = n())
  return(ts_data)
}

plot_ts_data <- function(df){
  # Plot time-series data by size class
  # Input: data frame with year, size_class, and counts columns
  ggplot(df, aes(x = year, y = counts, color = size_class)) +
    geom_line()
}
