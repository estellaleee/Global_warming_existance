library(ngramr)

start_year = 1998
stop_year = 2019

data <- as.data.frame(matrix(ncol=1, nrow=stop_year-start_year+1))

data$V1 <- seq(from=start_year, to=stop_year)

names(data)[names(data)=="V1"] <- "Year"

search_terms <- c("global warming exists", "global warming doesn't exist")

# Create a loop
for(i in 1:length(search_terms)){
  
  # Get each search term and store those in objects
  term <- search_terms[i]
  
  # Search for the term in the English 2012 corpus, starting from the year 1900 to 2008
  # Then house the output in a dataframe
  temp <- ngram(term, corpus = "eng_2012", year_start = start_year, smoothing = 0, 
                count = T, tag = NULL, case_ins = FALSE)
  
  # Merge NYT data with dataframe created step 1, matching by years
  data <- merge(data, temp[,c("Year", "Count")], by ="Year", all.x=TRUE)
  
  # Reaname column by search term
  colname <- paste(term, sep="")
  
  # Rename added column with ID
  names(data)[names(data)=="Count"] <- colname
  
  # Remove temporary dataframe
  rm(temp)
  
}

# Rename the year variable from "term" to "year"
#names(data)[names(data)=="term"] <- "year"

head(data)

data_long <- reshape(data, 
                     varying = search_terms, 
                     v.names = "count",
                     timevar = "search_term", 
                     times = search_terms, 
                     direction = "long")

write.csv(data, 'ngramm_data.csv')

library(ggplot2)

p <- ggplot(data_long, aes(x=Year, y=count, group=search_term))


p +  geom_line(aes(colour = search_term))
