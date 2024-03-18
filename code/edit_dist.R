# Tuomas Heikkilä
# 2023-01-30
#
# Introduction to basics of character and text wrangling using the case study
# of dog names and human names
#
### PREPARATIONS ###############################################################

# Clean the environment:
# Removes all variables from the memory

rm(list = ls())

# Set additional options, e.g., suppress scientific notation (9.9E-1 -> 0.99)
options(scipen = 999)

# Load packages dplyr, tidyr and stringdist to the memory
packages("dplyr", "tidyr", "stringdist")

# Alternatively, use p_load function from pacman package to install all missing
# packages (including pacman itself) and load them

pacman::p_load(dplyr,       # Used to manipulate data frames
               tidyr,       # Used to manipulate data frames
               stringdist)  # Used to compute distances between characters

### BASIC FUNCTIONS ON VECTORS #################################################

# Lets assign dog names into a "names" variable
names <- c("Simo", "Milo", "Murre", "Laila")

# Remember to close parenthesis and quotation marks, and follow commas with
# new values/parameters. Compared to many other languages R is very forgiving,
# but not without its own syntax errors.

# We can check that "names" is a character vector:
typeof(names)      # "typeof" should return "character"
class(names)       # Here, "class" function gives the same result

# We might also want a "Yes/No" answer (called boolean or logical):
is.vector(names)   # Should return TRUE
is.numeric(names)  # Whereas "names" is not numeric (FALSE)

# Names has a length of 4 elements:
length(names)

# Second element (value in the second index) of the vector is "Milo":
names[2]

# First to third elements are "Simo", "Milo" and "Murre"
names[1:3] # Print the range of element from 1 to 3
names[-4]  # Is same as excluding the 4th element

# Last element of any vector can be called indirectly by calling its length:
names[length(names)]   # Here, same as name[4] (length(names): 4))
names[length(names)-1] # Similarly, calling second to last element and so fort

# Which element is "Simo"?
which(names == "Simo")

# Check names for a name we are looking for:
"Lulu" %in% names                             # No match this time (FALSE)
c("Lulu", "Murre") %in% names                 # One miss, but also one match

# To call for the length of all the values (dog names), we would use "nchar"
nchar(names)

# To sort names alphabetically, we would use "sort":
sort(names)

# Or, in the reverse (alphabetical) order by combining with "rev":
rev(sort(names))

# See how, if we do not assign sorted vector anywhere (names <- sort(names)),
# calling names again will revert to its original order:
print(names)

names <- sort(names)  # Instead, we need to update the vector

# Usually, we start of by multiple vectors from different data sources and want
# to expand one vector with new values or join two vectors together.

# "append" function that takes two parameters: a) the original
# vector (to which we want to append values) and b) the value(s) to add

names <- c("Simo", "Milo", "Murre")
names <- append(names, "Laila")      # We have just updated the name var

# Variables might also have same values between them, for example "Murre"
names <- c("Simo", "Milo", "Murre")
more_names <- c("Laila", "Murre", "Kamu")

intersect(names, more_names)   # Calling "intersect" returns the shared value(s)
union(names, more_names)       # "union" gives unique values from both
setdiff(names, more_names)     # Hox! "setdiff" returns alle the values that
                               # appear on "names", but not in "more_names"

# To get unique values from a vector that already has duplicate values:
names <- append(names, more_names)   # "Murre" appears twice
unique(names)                        # These elements are unique to "names"

# Alternatively, calling "duplicated" function on a vector checks whether the
# element is repeated further into the vector and returns TRUE for the element
# that is repeated

duplicated(names)           # either FALSE (unique), TRUE (repeated)
which(duplicated(names))    # "which" returns the position of repeated element

names[duplicated(names)]    # Return the duplicate(s) by indexing
names[!duplicated(names)]   # Expression "!duplicated" ("is not duplicate")
                            # returns the vector without duplicated values

# Sometimes we might want to turn ("coerce") the variable into a to numeric type
# ("1" to 1, or "0.5" to 0.5). However, R does not know how to make number from
# words like "Laila", and defaults into a bunch of missing values (NA):

numeric_names <- as.numeric(names)
is.na(numeric_names)                # Checking that numeric_names are missing

any(is.na(numeric_names))  # using "any" function tells us whether there vector
                           # has any element corresponding an expression (is.na)

all(is.na(numeric_names))  # "all" function is similar, but instead of using OR
                           # operators, is uses AND (all elements have match the
                           # the expression "is missing")

# Coercing names into a data frame object gives a legitimate data frame:
as.data.frame(names)

### DATA FRAMES ################################################################

# Now we could build up the data frame by adding new information, like ages
df_dogs <-
  data.frame(names = names,                    # Column name = column values
             born  = c(2022, 2021, 2018, NA))  # Column name = column values

# Glimpse or View the data frame we have created:
glimpse(df_dogs)
str(df_dogs)
View(df_dogs)

# Alternatively, lets use mutate method that allows to edit and add columns
df_dogs <- as.data.frame(names)   # First, coerce names into a data frame

# Now let's add another column by
df_dogs <- mutate(df_dogs, born = c(2022, 2021, 2018, NA))

# In dplyr it is custom to manipulate data frames using a pipe (%>%).
# The pipe takes what is before it and applies that to the proceeding function.
# This reduced nested parantheses and constantly repeating the variable.

df_dogs <- df_dogs %>%
  mutate(born = c(2022, 2021, 2018, NA)) # mutate is still performed on df_dogs

# More ways to add new columns:
birth_place <- c("Lohja", "Kirkkonummi", "Ähtäri", "Posio")  # Define a new var

df_dogs$birth_place <- birth_place     # Create a new column using $[col name]
cbind(df_dogs, birth_place)            # Or bind column to existing data frame

# Adding new cases (as named vectors ~ dictionaries if needed):
new_dog <- c("names" = "Kamu", "born" = 2013, "birth_place" = "Keuruu")

# In base R, adding new cases is a struggle: use "rbind" or "bind_rows" instead
df_dogs[nrow(df_dogs) + 1,] <- new_dog    # Row after the last row
df_dogs <- rbind(df_dogs, new_dog)        # Order of the columns is important
df_dogs <- bind_rows(df_dogs, new_dog)    # Order or the column does not matter

# Drop duplicate values with "unique" or "duplicated"
df_dogs <- df_dogs %>% unique()
df_dogs <- df_dogs[!duplicated(df_dogs),]

# Call the row or column names of data frame:
colnames(df_dogs)
rownames(df_dogs)

# Assign new names
colnames(df_dogs) <- c("name", "born", "origin") # Make sure to have a
                                                 # replacement for every col
rownames(df_dogs) <- tolower(df_dogs$name)       # Dog names, but in lower case

# To call variables (columns) we use df$column or df["row", "column"]:
df_dogs$born
df_dogs[,"born"]   # We will leave rows empty to get all of them

# At some point, "born" has been stringified: coerce back to numeric
df_dogs$born <- as.numeric(df_dogs$born)
df_dogs <- df_dogs %>% mutate(born = as.numeric(born)) # Same thing with dplyr

# To get specific values, we need to specify the row as well:
df_dogs[1, "name"]
df_dogs["simo", "name"]  # Since we added dog names as row names
df_dogs$name[1]

# To search for a specific value, logical operators like "==" and "!=" are used:
df_dogs[df_dogs$name == "Laila",]       # Get all information on "Laila"
df_dogs[df_dogs$name != "Simo", "born"] # Get all ages except for Simo's

# Drop rows with missing values
df_dogs[!is.na(df_dogs$born),]     # Get rows where ages is not missing
df_dogs[complete.cases(df_dogs),] # Get rows where there are no missing values
na.omit(df_dogs)                  # Similar to "complete.cases"

# Filtering (subsetting) cases is easiest using "filter" from dplyr pkg:
df_dogs %>% filter(born > 2020) # Born after 2020
df_dogs %>% filter(between(born, 2018, 2022)) # Born between 2010 and 2020
df_dogs %>% filter(startsWith(origin, "K")) # Born between 2010 and 2020
df_dogs %>% filter(when(character)) # Born between 2010 and 2020

# Selecting columns based on conditions
df_dogs %>% select(origin)             # Select any column by its name
df_dogs %>% select(1:2)                # First two columns by their index
df_dogs %>% select(-1)                 # Exclude first column
df_dogs %>% select(where(is.numeric))  # Select only numeric columns
df_dogs %>% select(starts_with("b"))   # Select columns starting with a letter

keep_cols <- c("name", "born")
df_dogs %>% select(all_of(keep_cols))  # Keep all columns identified by a vector

# Selected columns are still if of the type data frame: "pull" to coerce vectors
df_dogs %>% select(origin) %>% pull()

### JOIN DATA FRAMES ###########################################################

"TBA"

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
