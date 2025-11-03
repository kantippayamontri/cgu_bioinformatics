#First generate the following a and b:
a <- c(1,5,2,3,6,4)
b <- c(4,1,2,3,6,5)

# Find the numbers in a and b that are equal
a[a==b]
b[b==a]
# Ans: 2 3 6

# what is the maximum product of a and b
max(a*b)
# Ans: 36

# Let’s use the ‘mtcars’ data frame
# What is the number on the 15th row and 4th column?
mtcars[15,4]
# Ans:205

# Is the number on the 11th row and 8th column the same as the 3rd row and
# 11th column?
mtcars[11,8] == mtcars[3,11]
# Ans: TRUE
  # We already know there are 3 types of gears: 3, 4, 5
  # Find out which gear is less use among all these cars
  # Find all information of cars that use the gear you found in the previous
  # question
less_gear <- table(mtcars$gear) # get the frequency
less_gear = less_gear[less_gear == min(less_gear)] # get the min frequency dataframe
less_gear #show the name of less gear and frequency
name_less_gear <- names(less_gear) # gear that use less among all these cars
name_less_gear # show only name of less gear

mtcars[mtcars$gear == name_less_gear, ] # show all information of cars that use the gear
#Ans: % gear number 5 is less use among all these cars

# information the car that has gear 5
# mpg cyl  disp  hp drat    wt qsec vs am gear carb
# Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
# Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
# Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
# Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
# Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8

