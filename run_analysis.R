# run_analysis
# Runs all the data extraction for the coursework
# Use default arguments to run on the data provided with the course
#
# Any additional arguments (...) are passed to the write.table calls that output
# data onto files.
run_analysis <- function(
   data_path="UCI HAR Dataset",      # Path to data
   data_set_names=c("train","test"), # Name of data sets
   features_list_f="features.txt",   # File containing column names
   activity_labels_f="activity_labels.txt",# File with activity labels meaning
   activity_colname="Activity",      # Activity column name
   subject_colname="Subject_ID",     # Subject ID column name
   output="data.txt",                # File to output processed data to
   output_uncollapsed="merged.txt",  # If not "", output data that hasn't been averaged for
                           # all similar activity per subject (i.e. after step 4 of assignment)
                           # to this file
   ...
   ) {
   
   # Go through all 561 feature names (newline-delimited) and 
   # find ones with words 'mean' and 'std' to get the mean and standard deviation.
   # Note down their position/column in the 561-element vector.
   features_list_f = paste(data_path, features_list_f, sep="/") # Get full file path
   features_data = read.table(features_list_f, header=FALSE) # Use default sep, white space
   features_data_row = grep("mean|std", features_data[,2])
   # Get the actual index in column 1 for all matching features.
   features_data_indices = features_data[features_data_row,1]
   
   # Load activity names and mapping.
   activity_labels_f = paste(data_path, activity_labels_f, sep="/")
   activity_labels_data = read.table(activity_labels_f,header=FALSE)
   # First column contains label number, make that the rowname for easy access
   activity_labels <- c()
   for(i in 1:nrow(activity_labels_data)) {
      activity_labels[as.numeric(activity_labels_data[i,1])] <- 
         as.character(activity_labels_data[i,2])
   }
   
   # Go through data sets and load them in
   data <- data.frame()
   for(data_set_name in data_set_names) {
      # Get data set base path
      cur_path <- paste(data_path, data_set_name, sep="/")
      
      # Load subject ID column from subject_<data_name>.txt file
      subject_f <- paste("subject_", data_set_name, ".txt", sep="")
      subject_f <- paste(cur_path, subject_f, sep="/") # Get full path
         # Set first column of cur_data for current data set
      cur_data <- read.table(subject_f, col.names=subject_colname, header=FALSE)
      
      # Load activity column and merge into cur_data
      activity_f <- paste("y_", data_set_name, ".txt", sep="")
      activity_f <- paste(cur_path, activity_f, sep="/") # Get full path
      cur_activity_data <- read.table(activity_f, col.names=activity_colname, header=FALSE)
         # Map activity number to names (definitely not the most efficient way to do it)
      cur_activity_data <- apply(cur_activity_data, 1, function(x) {activity_labels[x]})
         # Merge column into cur_data for current data set
      cur_data <- cbind(cur_data, cur_activity_data)
      # Loses colname on apply, redo here
      colnames(cur_data)[ncol(cur_data)] <- activity_colname

      # Get features data file path
      data_f <- paste("X_", data_set_name, ".txt", sep="")
      data_f <- paste(cur_path, data_f, sep="/") # Get full path
      
      # Load features full data
      cur_feature_data <- read.table(data_f, header=FALSE)
      # Now get only feature columns we want (means and standard deviation)
      cur_feature_data <- cur_feature_data[,features_data_indices]
         # Set colnames and then merge with data
      colnames(cur_feature_data) = as.character(features_data[features_data_indices,2])
      cur_data <- cbind(cur_data, cur_feature_data)
      
      # Merge with main data frame
      data <- rbind(data, cur_data)
   }
   
   # Data up to step 4 of assignment now ready, can be dumped if required
   if(output_uncollapsed!="") {
      write.table(x=data, file=output_uncollapsed, row.names = FALSE, ...)
   }
   
   # Collapse rows sharing same subject and activity combinations by getting mean of all columns
      # Find all unique combinations of subject and activity
   search_cols = c(subject_colname, activity_colname) # Columns we're interested in
   data_unique <- data[!duplicated(data[,search_cols],), search_cols]
      # Get colnames of all data columns (excludes subject ID and activity)
   data_colnames <- colnames(cur_data)
   data_colnames <- data_colnames[data_colnames!=subject_colname & data_colnames!=activity_colname]
      # Create data frame for collapsed data, copy first row of uncollapsed to inherit columns and types
   data_collapsed <- data[1,]
      # Loop through unique combinations and get all columns with data for that combination
      # Then collapse those rows by averaging all columns.
      # Then add the resulting single row to the output data
   for(i in 1:nrow(data_unique)) { 
      # Get similar subject and activity combinations
      cur_data <- data[data[,subject_colname]==data_unique[i,subject_colname] &
                       data[,activity_colname]==data_unique[i,activity_colname],]
      # Calculate the averages for each data column
      cur_data_means_list <- colMeans(cur_data[,data_colnames])
      # Create data frame with the means
         # Create a data frame with enough columns for all features mean
      cur_data_means <- data.frame(matrix(0, ncol=length(cur_data_means_list), nrow=1))
         # Set column names correctly
      colnames(cur_data_means) <- names(cur_data_means_list)
      cur_data_means[1,] <- cur_data_means_list
      # Create single row with subject ID, activity and all means
      cur_data <- cbind(cur_data[1,search_cols], cur_data_means)
      # Set rowname to increment of previous row
      rownames(cur_data) <- i
      
      # Append row to collapsed data frame
      data_collapsed[i,] <- cur_data
   }

   # Write processed data to output file
   write.table(x=data_collapsed, file=output, row.names = FALSE, ...)
   
   # Return processed data
   data_collapsed
}
