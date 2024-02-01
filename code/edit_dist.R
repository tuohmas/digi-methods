# Tuomas Heikkilä
# 2023-01-30
#
# Script for calculating edit distances between dog names and human names
#
### PREPARATIONS ###############################################################

# Clean the environment
rm(list = ls())

# Load packages
require("stringdist", "dplyr", "tidyr", "strcmp")

# Alternatively
pacman::p_load(dplyr,
               tidyr,
               stringdist)


### CALCULATE EDIT DISTANCES ###################################################

# Mockup names for our dogs
names <- c("Simo","Milo","Murre","Laila")

# for(i in 1:length(names)) {
for(i in seq_along(names)) { # Prefer seq_along() to length() for more neat code

  # Calculate edit distance between consequtive dog names
  dist <- adist(names[i], names[i+1]) %>% as.numeric()
  # print(edit_dist)

  # Add dog information
  paste0("edits from [", names[i], "] to [", names[i+1], "]: ", dist) %>%
    print()
}


# Add data as a matrix
mat_names <- matrix(nrow = length(names), ncol = length(names),
                    dimnames = list(names, names))

# Iterate through names again, now for every cell of the matrix
for(i in seq_along(names)) {

  # Initialize j as (index) 1
  j <- 1

  while(j <= length(names)) {

    # Calculate edit distance between consequtive dog names
    dist <- adist(names[i], names[j]) %>% as.numeric()

    # add edit distance information to proper place in our matrix
    mat_names[i, j] <- dist

    # Add one to j before restarting while loop
    j <- j + 1

  }

  # End loop
}

# Load a data set of the most popular dog names and human names now and in 1900
# Most popular dog names in 2022 (source: Kennelliiton Omakoira as reported by
# Iltalehti, https://www.iltalehti.fi/perheartikkelit/a/b41870fc-7065-438d-8560-ac81df35b707)
# https://dvv.fi/suosituimmat-lasten-nimet (Digi- ja väestötietovirasto)
# Names in 1900 collected from: https://yle.fi/a/3-9418798

# Load data
fname <- ("data/dvv_kenneliliitto_2022_popular_names.csv")
data_names <- read.table(fname, sep = ";", header = TRUE)

# Create empty placeholder vectors
mean_distance_dogs_dogs <- vector("numeric")
mean_distance_dogs_people <- vector("numeric")
mean_distance_dogs_ancients <- vector("numeric")

# Iterate through data
for (i in 1:nrow(data_names)) {

  # Calculate mean edit distances from every dog name to another dog name
  mean_distance_dogs_dogs <-
    stringdistmatrix(data_names[i,1],  # i'th row of the dog name column
                     # compare to other dog names than itself
                     data_names[-i, 1]) %>%
    as.numeric() %>%
    mean() %>%
    append(mean_distance_dogs_dogs, .)

  # Calculate mean edit distances from every dog name to people names in 2022
  mean_distance_dogs_people <-
    stringdistmatrix(data_names[i,1],    # i'th row of the dog name column
                     data_names[,2]) %>% # compare to most popular names in 2022
    as.numeric() %>%
    mean() %>%
    append(mean_distance_dogs_people, .)

  # Calculate mean edit distances from every dog name to people names in 1900
  mean_distance_dogs_ancients <-
    stringdistmatrix(data_names[i,1],    # i'th row of the dog name column
                     data_names[,3]) %>% # compare to most popular names in 1900
    as.numeric() %>%
    mean() %>%
    append(mean_distance_dogs_people, .)

}

# Averages of averages
mean_distance_dogs_dogs %>% mean()
mean_distance_dogs_people %>% mean()
mean_distance_dogs_ancients %>% mean()

# Run t-test to explore whether there is a statistical difference between
# Dog names to recent people names and ancient people names
t.test(mean_distance_dogs_dogs, mean_distance_dogs_people)
t.test(mean_distance_dogs_dogs, mean_distance_dogs_ancients)

t.test(mean_distance_dogs_people, mean_distance_dogs_ancients)

# Null hypothesis stands. Based on t-test results we can't say confidently
# that dog names would be *more* similar to current names than ancient ones
