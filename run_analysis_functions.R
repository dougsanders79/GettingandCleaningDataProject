# this file contains 3 functions for use with run_analysis.R

# in this function, M1 is the vector of (1-6) values for the activity for each row of the test or training data.  V1 is
# the 2x6 correlation used to assign the activity. 
index_replace <- function(M1, V1){
  X <- vector(mode="numeric", length=0)
  for(i in  1:length(M1)){
    X[i] <- V1[as.numeric(M1[i])]
  }
  X
}

# this function makes more descriptive labels for the variables (columns) for #4 "appropriately labels the data set with descriptive
# variable names.  this probably could have been done easier, but I could not figure it out.   Feel free to suggest! :).

make_labels <- function(M1, SFM, DFS){
  X <- vector(mode="character", length=0)
  A <- vector(mode="character", length=0)
  for(i in  1:length(M1)){ 
    for(j in 1:length(SFM)) {
      X1 <- 0
      X1 <- grep(SFM[j],colnames_tidy[i])
      if(length(X1) != 0)
      {
        A[j] <- DFS[j]
      }
      else
      {
        A[j] <- ""
      } 
    }
    X[i] <- paste(A, collapse = "")
  }
  X
}

# the only way I could figure out how to calculate the mean was to it using the for loop founction below.
#  (I am sure there is a better way.. but I don't have the time right now)  considerable cpu time savings 
# was achieved by using the colMeans function such that the i index loops over the 180 levels of 
# subject_activity (so colMeans function is called 180 times) and the j index simply populates the DF data 
# frame with the appropriate vale from "temp".  Using the mean function 180x66 times was taking forever.


loop_over <- function(DATA_FRAME, FACTOR, cols){
  DF <- data.frame()
  for(i in  1:length(FACTOR)){
    temp <- colMeans(DATA_FRAME[DATA_FRAME$subject_activity== FACTOR[i],][, cols ])
    for( j in 1:2) {
      DF[i, j] <-  DATA_FRAME[DATA_FRAME$subject_activity == FACTOR[i], j][1] 
      #     DF[i, j] <-  0
    }
    for( j in cols) {
      DF[i, j] <-  temp[j-2] 
    }
  }
  row.names(DF) <- FACTOR
  colnames(DF) <- names(DATA_FRAME)[1:68]
  DF
}