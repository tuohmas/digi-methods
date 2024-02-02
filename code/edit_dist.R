# Tuomas Heikkilä
# 2023-01-30
#
# Script for calculating edit distances between dog names and human names
#
### PREPARATIONS ###############################################################

# Clean the environment
rm(list = ls())

# Load packages
packages( "dplyr", "tidyr", "stringdist")

# Alternatively, p_load function from pacman library allows installs all missing
# packages (including pacman) and loads them
pacman::p_load(dplyr,
               tidyr,
               stringdist)

### BASIC FUNCTIONS ON VECTORS #################################################

# Lets assign dog names into a "names" variable
names <- c("Simo","Milo","Murre","Laila")

# Names is a character vector:
typeof(names)
is.vector(names)

# Names has the length of 4 elements:
length(names)

# Second element (value in the second index) of the vector is "Milo":
names[2]

# Last element is "Laila":
names[length(names)]

# Which element is "Simo"?
which(names == "Simo")

# To call for the length of the names in the vector, we would use nchar:
nchar(names)

# To sort names alphabetically, we would use sort:
sort(names)

# See how, if we do not assign sorted vector anywhere (names <- sort(names)),
# calling names again will revert to its original order:
print(names)

# Coercing names to numeric type results to a bunch of NA values (missing):
as.numeric(names)

# Whereas coercing names into a dataframe object gives a legitimate dataframe:
as.data.frame(names)

# Now we could build up the data frame by adding new information, like ages
df_dogs <- data.frame(names = names, # Column name, column values
                      ages  = c(2022, 2021, 2018, NA))

# Alternatively, lets use mutate method that allows to edit and add columns
df_dogs <- as.data.frame(names)

# The pipe (%>%) takes what is before it and applies that to the function after
df_dogs <- df_dogs %>% mutate(ages = c(2022, 2021, 2018, NA))

# Is the same as:
df_dogs <- mutate(df_dogs, ages = c(2022, 2021, 2018, NA))

# Glimpse df_dogs:
glimpse(df_dogs)

### CALCULATE EDIT DISTANCES ###################################################

# Lets iterate through every dog name pair, so 1st element against 2nd element,
# 2nd against 3rd and so one, and calculate so called Levenshtein distance
# between the two character strings (the smaller the number, the more similar
# the strings are to one another

# for(i in 1:length(names)) {
for(i in seq_along(names)) { # Prefer seq_along() to length() for more neat code

  # Calculate edit distance between consequtive dog names
  # The pipe (%>%) allows us to perform functions in sequence:
  # 1) Apply adist() function, 2) take the result value an coerce it to numeric

  dist <- adist(names[i], names[i+1]) %>% as.numeric()

  # Error handling: when i is the last element, function will compare it to
  # an non-existing element, resulting to missing (NA). This might cause
  # problems later on, so let's break the loop if the value is NA (with is.na)

  if(is.na(dist)) { break # When dist variable is missing (NA), break loop

    } else {              # When expression is FALSE (dist is not missing value)

  # Add dog information
  paste0("edits from [", names[i], "] to [", names[i+1], "]: ", dist) %>%
    print()

    }
  # End loop
}

### ITERATE THROUGH A LIST, 2 ##################################################

# Instead of simply printing out edit distances one pair at a time, we would
# want to visualize every pair as one matrix

# First, lets initialize an empty matrix of the size of our data set:
# One dog name for every row and one dog name for every column
mat_names <- matrix(nrow = length(names), ncol = length(names),
                    dimnames = list(names, names))

# Iterate through names again, now for every cell of the matrix
for(i in seq_along(names)) {

  # Initialize j as (index) 1
  j <- 1

  # While loop repeats as long as the expression remains TRUE, breaks when FALSE
  while(j <= length(names)) { # "While  j is smaller or equal to vector length"

    # Calculate edit distance between consecutive dog names
    dist <- adist(names[i], names[j]) %>% as.numeric()

    # add edit distance information to its proper place in our matrix:
    # row i, column j
    mat_names[i, j] <- dist

    # Add one to j before restarting the loop
    j <- j + 1

  }

  # End loop
}

# Print out the resulting matrix
print(mat_names)

### ANALYZE SIMILARITY BETWEEEN DOG NAMES TO PEOPLE NAMES ######################

# Load a data set of the most popular dog names and human names now and in 1900
# Most popular dog names in 2022 (source: Kenneliiton Omakoira as reported by
# Iltalehti, https://www.iltalehti.fi/perheartikkelit/a/b41870fc-7065-438d-8560-ac81df35b707)
# https://dvv.fi/suosituimmat-lasten-nimet (Digi- ja väestötietovirasto)
# Names in 1900 collected from: https://yle.fi/a/3-9418798

# Load clean data set
fname <- ("data/dvv_kenneliliitto_2022_popular_names.csv")
data_names <- read.table(fname, sep = ";", header = TRUE)

# Data set is a dataframe with 3 columns for top dog names given in 2022,
# top people names given in 2022, and top people names given in 1900. Columns
# are of type character, and there are 31 rows for each column (no missing)
glimpse(data_names)
View(data_names)

# Initialize empty vectors
mean_distance_dogs_dogs <- vector("numeric")
mean_distance_dogs_people <- vector("numeric")
mean_distance_dogs_ancients <- vector("numeric")

# Iterate through data set

for (i in 1:nrow(data_names)) {

  # Calculate mean edit distances from every dog name to another dog name
  mean_distance_dogs_dogs <-
    stringdistmatrix(data_names[i,1],  # i'th row of the dog name column
                     # compare to other dog names than itself
                     data_names[-i, 1]) %>%
    as.numeric() %>% # Coerce to type numeric
    mean() %>%       # Calculate mean distance to every target string
    # Append average to the result vector. "." is a placeholder for the mean
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
    append(mean_distance_dogs_ancients, .)

}

print(mean_distance_dogs_dogs)
max(mean_distance_dogs_dogs)

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
