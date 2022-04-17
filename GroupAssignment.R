movieDB <- read.csv("biggieFiltered.csv", header = T)
initialMovieActorMap <- read.csv("initialMovieActorMap.csv", header = T)  
finalMovieActorMap <- read.csv("finalMovieActorMap.csv", header = T)  
library("foreign")

# drop titleType since all are movies
movieDB$titleType <- NULL

# change types
movieDB$primaryTitle <- as.character(movieDB$primaryTitle)
movieDB$originalTitle <- as.character(movieDB$originalTitle)
movieDB$genres <- as.character(movieDB$genres)


# # DO    how many movies in a year?
# s.yearCount <- aggregate(x=movieDB$startYear, by=list(movieDB$startYear),FUN='length')
# # DO    group by directors not null
# t.directorsNotNull<-movieDB[movieDB$directors != '\\N',c("tconst","directors","averageRating")]
# t.directorsNotNull[order(t.directorsNotNull$directors),]
# # DO    how many movies director made?
# s.directorMCount<-aggregate(x=t.directorsNotNull$directors, by=list(t.directorsNotNull$directors),FUN='length')
# # DO    sort descending
# s.directorMCount[order(-s.directorMCount$x),]
# 
# # DO    directors null
# t.directorsNull<-movieDB[movieDB$directors == '\\N',c("tconst","directors","averageRating")]

## Filter Number of Votes > 100
MovieVotesAbv100 <-  movieDB[movieDB$numVotes > 100 ,]

## Filter Number of Votes > 3841 
MovieVotesAbvAvg <-  movieDB[movieDB$numVotes > 3841 ,]

## Filter Past 10 Year Movie 

#create new column to store Year Diff (Year(now) - start year
yeardiff1<- numeric()
yeardiff2<- numeric()

#library used to get Year from data 
library(lubridate)

#Function used to find the difference between the current year and the movie start year
dateDiff <-function(x){
  return (year(Sys.Date()) - x)
}

# Apply to Movies above 100 number of votes.
yeardiff1 <- NULL
for (i in 1:nrow(MovieVotesAbv100)){
  row_start_year <- MovieVotesAbv100$startYear[i]
  computeDateDiff <- dateDiff(row_start_year)
  yeardiff1 <- append(yeardiff1,computeDateDiff)
}


# Apply to Movies above Average number of votes.
yeardiff2 <- NULL
for (i in 1:nrow(MovieVotesAbvAvg)){
  row_start_year <- MovieVotesAbvAvg$startYear[i]
  computeDateDiff <- dateDiff(row_start_year)
  yeardiff2 <- append(yeardiff2,computeDateDiff)
}

# Make initial filtered dataset. Num Votes > 100, Year Diff < 10, Rating >= 7.
initialFilter <- MovieVotesAbv100
initialFilter$YearDiff = yeardiff1
initialFilter<- initialFilter[initialFilter$YearDiff < 10,]
initialFilter<- initialFilter[initialFilter$averageRating >= 7,]

# Make final filtered dataset. Num Votes > Average (3841), Year Diff < 10.
finalFilter <- MovieVotesAbvAvg
finalFilter$YearDiff = yeardiff2
finalFilter<- finalFilter[finalFilter$YearDiff < 10,]

# Get Unique Genres 
uniqueGenres <- unique(unlist(strsplit(as.character(movieDB$genres), ",")))

# Split Genres into Multiple Columns
# Create Data Frame For Movie ID, Genre and Average Rating
# Generate Movie Genre Category Column for Initial Filter Dataset
initialMovieGenre <- data.frame(matrix(ncol = length(uniqueGenres) + 2, nrow = 0))
header <- c("Movie ID",uniqueGenres,"Average Rating")
colnames(initialMovieGenre) <- header

for (i in 1:nrow(initialFilter)){
  initialMovieGenre[nrow(initialMovieGenre) + 1,] <- c("no")
  initialMovieGenre$`Movie ID`[i] <- initialFilter$ï..tconst[i]
  initialMovieGenre$`Average Rating`[i] <- initialFilter$averageRating[i]
  individualMovieGenre <- unlist(strsplit(as.character(initialFilter$genres[i]), ","))
  for (j in individualMovieGenre){
    initialMovieGenre[i,j] = "yes"
  }
}


# Split Genres into Multiple Columns
# Create Data Frame For Movie ID, Genre and Average Rating
# Generate Movie Genre Category Column for Final Filter Dataset
finalMovieGenre <- data.frame(matrix(ncol = length(uniqueGenres) + 2, nrow = 0))
header <- c("Movie ID",uniqueGenres,"Average Rating")
colnames(finalMovieGenre) <- header

for (i in 1:nrow(finalFilter)){
  finalMovieGenre[nrow(finalMovieGenre) + 1,] <- c("no")
  finalMovieGenre$`Movie ID`[i] <- finalFilter$ï..tconst[i]
  finalMovieGenre$`Average Rating`[i] <- finalFilter$averageRating[i]
  individualMovieGenre <- unlist(strsplit(as.character(finalFilter$genres[i]), ","))
  for (j in individualMovieGenre){
    finalMovieGenre[i,j] = "yes"
  }
}

#Convert to respective column to Factor 
initialMovieGenre[2:27] <- lapply(initialMovieGenre[2:27], factor)
initialMovieGenre$`Average Rating` <- as.integer(initialMovieGenre$`Average Rating`)
finalMovieGenre[2:27] <- lapply(finalMovieGenre[2:27], factor)
finalMovieGenre$`Average Rating` <- as.integer(finalMovieGenre$`Average Rating`)

#relationship between Ratings and the Genre
#plot(movieDB$genres,mean(movieDB$averageRating))

#relationship between Ratings and the Year
plot (movieDB$startYear, movieDB$averageRating )

#For Weka (Need to remove the MovieID )
write.arff(initialMovieGenre, "initialMovieGenre.arff")
write.csv(initialMovieGenre, "initialMovieGenre.csv")
write.arff(finalMovieGenre, "finalMovieGenre.arff")
write.csv(finalMovieGenre, "finalMovieGenre.csv")

# Get Unique Actors
initialMovieActorMap$actors <- gsub(" ", "", initialMovieActorMap$actors, fixed = TRUE)
initialMovieActorMap$actors <- gsub("'", "", initialMovieActorMap$actors, fixed = TRUE)
finalMovieActorMap$actors <- gsub(" ", "", finalMovieActorMap$actors, fixed = TRUE)
finalMovieActorMap$actors <- gsub("'", "", finalMovieActorMap$actors, fixed = TRUE)

intialUniqueActors <- unique(unlist(strsplit(as.character(initialMovieActorMap$actors), ",")))
finalUniqueActors <- unique(unlist(strsplit(as.character(finalMovieActorMap$actors), ",")))

# Split Actors into Multiple Columns
# Create Data Frame For Movie ID, Actors and Average Rating
# Perform for Initial
initialMovieActors <- data.frame(matrix(ncol = length(intialUniqueActors) + 2, nrow = 0))
header <- c("Movie ID",intialUniqueActors,"Average Rating")
colnames(initialMovieActors) <- header

# Map relevant movie with actors as factor columns
for (i in 1:nrow(initialMovieActorMap)){
  initialMovieActors[nrow(initialMovieActors) + 1,] <- c("no")
  initialMovieActors$`Movie ID`[i] <- initialMovieActorMap$Movie.ID[i]
  initialMovieActors$`Average Rating`[i] <- initialMovieActorMap$Average.Rating[i]
  individualMovieActors <- unlist(strsplit(as.character(initialMovieActorMap$actors[i]), ","))
  individualMovieActors <- gsub(" ", "", individualMovieActors, fixed = TRUE)
  individualMovieActors <- gsub("'", "", individualMovieActors, fixed = TRUE)
  for (j in individualMovieActors){
    initialMovieActors[i,j] = "yes"
    }
}

# Set Rate Category to be Successful or Not
initialMovieActors$`Average Rating` <- as.numeric(initialMovieActors$`Average Rating`)
initialMovieActors[initialMovieActors$`Average Rating` >= 8, "Rating Category"] <- "Successful"
initialMovieActors[initialMovieActors$`Average Rating` < 8 , "Rating Category"] <- "Unsuccessful"

# Set all categorical variables as factor
initialMovieActors[2:2430] <- lapply(initialMovieActors[2:2430], factor)
initialMovieActors$`Rating Category`<-as.factor(initialMovieActors$`Rating Category`)

# Write to arff and perform analysis
write.arff(initialMovieActors, "initialMovieActors.arff")
write.csv(initialMovieActors, "initialMovieActors.csv")

# Split Actors into Multiple Columns
# Create Data Frame For Movie ID, Actors and Average Rating
# Perform for Final
finalMovieActors <- data.frame(matrix(ncol = length(finalUniqueActors) + 2, nrow = 0))
header <- c("Movie ID",finalUniqueActors,"Average Rating")
colnames(finalMovieActors) <- header

# Map relevant movie with actors as factor columns
for (i in 1:nrow(finalMovieActorMap)){
  finalMovieActors[nrow(finalMovieActors) + 1,] <- c("no")
  finalMovieActors$`Movie ID`[i] <- finalMovieActorMap$Movie.ID[i]
  finalMovieActors$`Average Rating`[i] <- finalMovieActorMap$Average.Rating[i]
  individualMovieActors <- unlist(strsplit(as.character(finalMovieActorMap$actors[i]), ","))
  individualMovieActors <- gsub(" ", "", individualMovieActors, fixed = TRUE)
  individualMovieActors <- gsub("'", "", individualMovieActors, fixed = TRUE)
  for (j in individualMovieActors){
    finalMovieActors[i,j] = "yes"
  }
}

# Set Rate Category to be Successful or Not
finalMovieActors$`Average Rating` <- as.numeric(finalMovieActors$`Average Rating`)
finalMovieActors[finalMovieActors$`Average Rating` >= 7, "Rating Category"] <- "Successful"
finalMovieActors[finalMovieActors$`Average Rating` < 7 , "Rating Category"] <- "Unsuccessful"

# Set all categorical variables as factor
finalMovieActors[2:6676] <- lapply(finalMovieActors[2:6676], factor)
finalMovieActors$`Rating Category`<-as.factor(finalMovieActors$`Rating Category`)

# Write to arff and perform analysis
write.arff(finalMovieActors, "finalMovieActors.arff")
write.csv(finalMovieActors, "finalMovieActors.csv")
