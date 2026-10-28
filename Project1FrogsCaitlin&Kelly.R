# Read in used packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)

# Import the data into an object called frogs
frogs <- read_excel("Desktop/STS3470/Project 1/CalaverasData.xls")

# Update code, given code
#Chunk #2
frogs <-  rename(frogs,
                 day = Day,
                 frog_type = `rent/ind/pro`,
                 distance = `jump distance`,
                 jump_n = `jump #`,
                 distance_rel = `Relative to jump #1`,
                 distance_3 = `3-jump dist`,
                 distance_3_off = `measured 3-jump`,
                 duration = `jump duration`,
                 angle_00 = unknown1,
                 angle_01 = angle,
                 angle_10 = unknown2,
                 velocity_00 = unknown3,
                 velocity_01 = Vel,
                 velocity_10 = unknown4
) %>%
  mutate(
    row = seq_len(nrow(.))
  ) %>%
  select(row, everything())

#Chunk #3
frogs <- frogs %>%
  mutate(jump_n = as.integer(jump_n))

#Chunk #4
## frog_type
frogs <- frogs %>%
  mutate(
    frog_type2 = case_when(
      frog_type == 1 ~ "rental",
      frog_type == 2 ~ "individual",
      frog_type == 3 ~ "pro",
      frog_type < 0 ~ "unknown"
    )
  )


#1: Caitlin
# In chunk #1, we use the library function to call in the tidyverse package, 
# which is a set of packages (including dplyr, stringr, ggplot2, and a few 
# others) that have similar data representations and designs. Then, I added 
# another library function to call in the readxl package, which allows us to 
# read in Excel files through code.

# In chunk #2, to use the starter code, we attach the read_excel function, 
# which allows us to read in our specific frogs data set through our file 
# directory, to an object called “frogs” by using a left arrow, “<-.” The 
# read_excel function helps us avoid manually importing the Excel file each 
# time we load RStudio and want to work with a specific data set. It also tells 
# us that we have 3272 observations of 14 variables in our environment. 
# In the next part of chunk #2, we call the object frogs again and use the 
# rename function, which allows us to rename old variables into new ones. In 
# this case, we call the “frogs” dataset, and we uncapitalize, generalize, 
# shorten, and properly label variable names. A couple of examples of renamed 
# variables include the old name of “unknown3” to “velocity_00” and 
# “rent/ind/pro” to “frog_type.” After the rename function is closed off, we 
# use a pipe operator “%>%” to call another function, the mutate function, but 
# for the same dataset and in consideration of all of the variables we just 
# renamed. The mutate function creates a new variable and column, called “row” 
# that uses the function seq_len, which generates a sequence with a specified 
# length inside of another function, nrow, which returns the number of rows 
# present in the data frame. The overall purpose of this code is to create a 
# new variable called row that essentially counts the number of rows in a 
# specific order (e.g., 1 to 10 in order).
# In the final part of chunk #2, we use the select function, which specifically 
# calls the new variable, row, and uses the everything function to select all 
# of the variables to be shown in the first column, matching the row numbers.

# In chunk #3, we make another object, still called frogs, that calls for the 
# frogs dataset, then uses a pipe operator, which allows the functions used 
# below it to assume that the frogs dataset is being used. Below the pipe 
# operator, we have another mutate function that is creating a second new 
# variable named “jump_n” that uses the old “jump_n” variable as an integer 
# with the as.integer function.
# In chunk #4, we make another frogs object that calls for the frogs dataset, 
# then uses a pipe operator where another mutate function is creating a third 
# new variable named “frog_type2” that uses the case_when function, which 
# allows us to vectorize multiple if-else statements sequentially. The new 
# variable, frog_type2, uses the original variable, frog_type, and assigns 
# names to the numbers that represent each type of frog that is known. Any 
# frogs represented by “-1” and were of an unknown type were labeled as 
# “unknown” through this function. For the frog types 1, 2, and 3, they were 
# labeled as rental, individual, and pro, respectively.




#2a: What is the average distance jumped in the study?
mean(frogs$distance)
# The average jump is 123.7054cm. 

#2b: What is the average distance jumped for rental frogs (frog_type of 1)? 
# Of professional frogs (frog_type of 3)? Use base R

# Rental frogs:
mean(frogs$distance[frogs$frog_type == 1])

# Professional frogs:
mean(frogs$distance[frogs$frog_type == 3])

# The average distance jumped for rental frogs is 79.75309cm, and the 
# average distance jumped for professional frogs is 135.8276cm.

#2c: What is the furthest distance jumped in the study? 
max(frogs$distance)
# The furthest distance jumped in the study is 219.78cm.

#2d: What is the average distance jumped for rental frogs? Of professional 
# frogs? For each question, write a single line of code using dplyr and 
# tidyverse tools. 

# Rental frogs:
frogs |>
  filter(frog_type == 1) |>
  summarise(mean_rental_jump = mean(distance))
# The average distance jumped for rental is 79.8cm. 

# Professional frogs:
frogs |>
  filter(frog_type == 3) |>
  summarise(mean_rental_jump = mean(distance))
# The average distance jumped for professional is 136cm. 

#2e: How many rental frogs are in the study? How many professional frogs 
# are in the study? Write a single line of code that will answer both 
# questions simultaneously. 
frogs |>
  count(frog_type)
# The number of rental frogs is 590 and the number of professional frogs 
# is 2512.


#3: Omit '0' distance values and change cm to meters
frogs_graphs <- frogs |>
  filter(distance != 0)

frogs_graphs <- frogs_graphs |>
  mutate(distance_m = distance/100)


#3a
graphA <- ggplot(frogs_graphs, aes(x = distance_m)) +
  geom_histogram(fill = "darkseagreen", color = "black") +
  labs(x = " ",
       y = " ") +
  theme_classic() + 
  geom_segment(aes(x = 1.3,
                   y = 275,
                   xend = 1.30,
                   yend = 140),
               arrow = arrow(length = unit(0.5, "cm")))

graphA


#3b
# subset so only rental
frogs_graphB <- frogs_graphs |>
  filter(frog_type == 1)


graphB <- ggplot(frogs_graphB, aes(x = distance_m)) +
  geom_histogram(fill = "grey", color = "black") +
  labs(x = " ",
       y = "Number of jumps") +
  theme_classic() +                             
  geom_segment(aes(x = 1.30,
                   y = 55,
                   xend = 1.30,
                   yend = 21),
               arrow = arrow(length = unit(0.5, "cm")))

graphB



#3c
# subset so only professional
frogs_graphC <- frogs_graphs |>
  filter(frog_type == 3)


graphC <- ggplot(frogs_graphC, aes(x = distance_m)) +
  geom_histogram(fill = "darkgreen", color = "black") +
  labs(x = "Jump distance (m)",
       x = " ",
       y = " ") +
  theme_classic() +                              
  geom_segment(aes(x = 1.30,
                   y = 275,
                   xend = 1.30,
                   yend = 118),
               arrow = arrow(length = unit(0.5, "cm")))

graphC


# Combine using patchwork
graphA/graphB/graphC



#4: Caitlin
#PART A
rent_frogs <- filter(frogs, frog_type2=="rental")
rent_frogs
pro_frogs <- filter(frogs, frog_type2=="pro")
pro_frogs

#PART B
set.seed(3470)

myrentsim <- replicate(5000, max(sample(rent_frogs$distance, 50, replace=FALSE)))
myrentsim

mean(myrentsim >= 160.000)

#PART C
myprosim <- replicate(5000, max(sample(pro_frogs$distance, 50, replace=FALSE)))
myprosim

mean(myprosim >= 160.000)


