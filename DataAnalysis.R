
users <- c("T_shan", "B_user2", "B_user3", "B_user4", "W_userP", "W_userK", "W_userC", "M_jens", "M_elvira")

data <- get_data_from_files(users)

data <- remove_duplicate_gets(data)

data <- remove_outliers(data)

compare_swipe_list(data)

clicks_swipe_list(data)

# -------------------------------------- METHODS --------------------------------------

# Load these in (select and run) before running the data analysis above.

# Method to calculate seconds per page
calculate_seconds <- function(data) {
  Seconds = lead(data$Time - lag(data$Time, default = first(data$Time)))
  Seconds <- Seconds / 1000
  # Make the last timestamp of a certain mode NA, since it contains the 
  #time that was needed to switch modes (not relevant)
  for(i in 2:nrow(data)) {
    if ((data[i, 'Mode'] != data[i-1, 'Mode']) ) {
      Seconds[i-1] <- NA
    }
  }
  Seconds
}


# Method to change tinder to swipe view
change_tinder_to_swipe <- function(data) {
  for(i in 1:nrow(data)) {
    if ((data[i, 'Mode'] == "tinder mode") ) {
      data[i, 'Mode'] <- "swipe mode"
    }
  }
  data
}

# Method to remove outliers
remove_outliers <- function(data) {
  remove_rows <- c()
  for(i in 1:nrow(data)) {
    if (!is.na(data[i, "Seconds"])) {
      if (data[i, "Seconds"] > 110) {
        print(data[i, "Seconds"])
        remove_rows <- c(remove_rows, i)
      }
    }
  }
  data <- data[-remove_rows,]
  data
}

# Method to remove duplicate 'get nutritions' and 'get recipe' rows from file
remove_duplicate_gets <- function(data) {
  remove_rows <- c()
  skip_iteration <- FALSE
  for(i in 1:nrow(data)-1) {
    if (skip_iteration == TRUE) {
      skip_iteration <- FALSE
      next
    }
    if (((data[i, 'Operation'] == "get recipe") && (data[i+1, 'Operation'] == "get recipe")) || (data[i, 'Operation'] == "get nutritional values") && (data[i+1, 'Operation'] == "get nutritional values")) {
      remove_rows <- c(remove_rows, i+1)
      skip_iteration <- TRUE
    }
  }
  data <- data[-remove_rows]
  data
}

# Method to retreive and preprocess data from list of user files
get_data_from_files <- function(users) {
  # Create empty dataframe
  userData <- data.frame(UserID=character(),
                         Time=integer(), 
                         Operation=character(), 
                         Mode=character(), 
                         Meal=character(), 
                         Seconds=integer(),
                         stringsAsFactors=FALSE) 
  
  # Load all individual user csv files, preprocess them and merge to one big dataframe
  for (user in users) {
    data <- read.table(paste("Data/", user, ".csv", sep=""), header=FALSE, sep=",", stringsAsFactors=FALSE)
    colnames(data) <- c("Time", "Operation", "Mode", "Meal")
    UserID <- rep(user, nrow(data))
    data <- cbind(UserID, data)
    
    # Change tinder view to swipe view
    data <- change_tinder_to_swipe(data)
    
    # Calculate seconds per page
    Seconds <- calculate_seconds(data)
    data <- cbind(data, Seconds)
    
    # Add processed data of new user to dataFrame
    userData <- rbind(userData, data)
  }
  userData
}

# ---------------------------------- STATISTICAL METHODS ----------------------------------

hist_swipe_list <- function(data) {
  swipes <- c()
  list <- c()
  for(i in 1:nrow(data)) {
    if (data[i, "Mode"] == "swipe mode" && !is.na(data[i, "Seconds"])) {
      swipes <- c(swipes,data[i, "Seconds"])
    }
    else {
      list <- c(list,data[i, "Seconds"])
    }
  }
  
  # Basic box plot
  library(ggplot2)
  ggplot(data, aes(x=Mode, y=Seconds, fill=Mode)) + geom_boxplot()
}

clicks_swipe_list <- function(data) {
  swipeClick <- 0
  swipes <- 0
  listClick <- 0
  list <- 0
  for(i in 1:nrow(data)) {
    if (data[i, "Mode"] == "swipe mode") {
      swipes <- swipes + 1
      if (data[i, "Operation"] == "get nutritional values" || data[i, "Operation"] == "get recipe") {
        swipeClick <- swipeClick + 1
      }
    }
    else {
      list <- list + 1
      if (data[i, "Operation"] == "get nutritional values" || data[i, "Operation"] == "get recipe") {
        listClick <- listClick + 1
      }
    }
  }
  avgS <- swipeClick / swipes
  avgL <- listClick / list
  
  print("Swipe button clicks / Total swipe interactions:")
  print(avgS)
  print("List button clicks / Total list interactions:")
  print(avgL)
}

