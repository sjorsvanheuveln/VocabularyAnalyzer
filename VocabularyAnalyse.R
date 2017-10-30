#Analysis of Dutch Language Vocabulary
#Sjors van Heuveln 28-10-2017
#Analyze triplet occurrences in the Dutch Language. This script can easily source another vocab file to do new analysis.

#Functions
trimWord <- function(rawWord) {
  return(unlist(strsplit(rawWord,'/'))[1]); #trims off ID
}
wordTriplets <- function(word, size) {
  if (grepl(' ', word)) { return(); } #break if word contains spaces
  if (nchar(word) < size) { return(); }
  for (i in 1:(nchar(word) - (size-1))) {
    triplet <- substr(word, i, i + (size-1));
    write(triplet, outputLocation, append = T);
  }
}
iterateDictionary <- function(dictionary, size, callback) {
  print('This may take a while!');
  print('Please wait ...');
  for (i in 1:length(dictionary)) {
    if (i %% 1000 == 0 ) { print(i); }
    word <- dictionary[i];
    wordTriplets(word, size);
  }
  callback;
}

#Installation: Install the below packages if you haven't already done so. Remove the hashtag and run the lines.
#library(devtools);
#install_github("sjorsvanheuveln/beepr2");
#install.packages('dplyr');
#install.packages('data.table');

#Setup 
library("beepr2");
library(dplyr);
library(data.table)

outputLocation <- '~/Desktop/output.txt';
data <- fread('https://raw.githubusercontent.com/titoBouzout/Dictionaries/master/Dutch.dic')
data <- as.character(data$V1);

# Data Cleanup
dataTrimmed <- sapply(data, trimWord, USE.NAMES = F);
rm(data);gc()

# Main Triplet Creation Process
iterateDictionary(dataTrimmed, 3, beep(23));

#Triplet Loading
rm(dataTrimmed);gc();
tripletData <- as.character(read.table(outputLocation)[,1]);
table <- table(tripletData);

#Triplet Top100 Analysis
wordNames <- names(table);
frequencies <- as.numeric(table);
cat(wordNames[order(frequencies, decreasing=T)][1:100])


