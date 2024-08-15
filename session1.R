##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Loops and Functions  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#.....................Nested loops.....................
# nested loops ----
file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)) {
  for (j in 1:length(file_suffix)){
    print(paste0(file_prefix[i], "_", file_suffix[j]))
  }
} 

#.......................Functions.......................
# bird dog function ----
birddog_sum <- function(bird, dog) {
  pets = bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)

# double it ----
double_it <- function(x) {
  print(2 * x)
}

double_it(1:4) # can also apply to a vector

# exclaim age ----
exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}
exclaim_age(24)

# find max ----
find_max <- function(val1, val2) {
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

5 * find_max(7, 3)

#.............Functions with Conditionals...............

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

half_vec <- vector(mode = "numeric", length = length(quarter_splits))

for (i in seq_along(quarter_splits)) {
  half <- print(quarter_splits[i] + quarter_splits[i+1])
  half_vec[i] <- half
}

half_vec

# animal age ----
animal_age <- function(animal, age) {
  #returns true if NOT a dog or goat
  if (!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be a dog or goat")
  }
  
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number")
  }
  
  if (age <= 0) {
    stop("Age must be positive")
  }
    
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  } 
}

# Try an 8 year old dog.
animal_age(animal = "dog", age = -8)
animal_age(animal = "cow", age = 2) # nothing went wrong, but nothing happened
animal_age(animal = "dog", age = "yellow") #error, non-numeric input

# dog's favorite food ----
dog_choice <- data.frame(dog_name = c("Khorra",
                                      "Teddy",
                                      "Waffle",
                                      "Banjo"),
                         food = c("everything",
                                  "salmon",
                                  "pancakes",
                                  "chicken"))
library(tidyverse)

dog_menu <- function(name) {
  my_sub <- filter(dog_choice, dog_name == name)
  print(paste("My name is", my_sub$dog_name, "and my favorite food is", my_sub$food))
}

dog_menu("Waffle")

# warning messages ----
calc_windpower <- function(rho, radius, windspeed) {
  
  if (windspeed > 130) {
    warning("wow, that's fast! are you sure?")
  }
  
  if (rho > 1.225) {
    warning("that air density is suspicious...")
  }
  
  if (radius < 1) {
    stop("rotor radius must be a positive value")
  }
  print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

calc_windpower(0.4, 50, 40)
