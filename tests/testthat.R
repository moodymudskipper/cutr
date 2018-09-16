library(testthat)
library(cutr)

test_check("cutr")

set.seed(1)
numbers  <- round(cumsum(abs(rnorm(20,10,10))))

# from 4 to 262

# maybe additional parameter : crop would be nice, goes well with expand,
# makes breaks and width0 etc more flexible and we can forget about complex variants

# regular breaks



##################
#    breaks      #
##################


# one cut outside left
table(cut3(numbers,-5,"breaks"))
#table(cut3(numbers,-5,"breaks", expand = FALSE)) # should return explicit error

# format is default format function, we can pass the trim parameter to have
# results similar to cut
table(cut3(numbers,-5,"breaks", trim = TRUE))

# or change the formatting function to formatC
table(cut3(numbers,-5,"breaks", format_fun = formatC))

# one cut outside right
table(cut3(numbers,300,"breaks"))

# cuts inside, default expand = TRUE
table(cut3(numbers,c(100,150,200),"breaks"))

# cuts inside, expand = FALSE
table(cut3(numbers,c(100,150,200),"breaks",expand = FALSE))

# cuts outside, default crop = FALSE
table(cut3(numbers,c(0,150,500),"breaks"))

# cuts outside, crop = TRUE
table(cut3(numbers,c(0,150,500),"breaks",crop = TRUE))

# mixing both
table(cut3(numbers,c(100,150,200),"breaks",expand = FALSE, crop = TRUE))


##################
# groups         #
##################

table(cut3(numbers,6,"groups"))

##################
# n_intervals    #
##################

table(cut3(numbers,4,"n_intervals"))
# no surprise with expand and crop
table(cut3(numbers,4,"n_intervals",expand=TRUE))
table(cut3(numbers,4,"n_intervals",crop=TRUE))

##################
# width          #
##################

table(cut3(numbers,50,"width"))
table(cut3(numbers,50,"width",crop = TRUE))

table(cut3(numbers,50,"width_0",oneval = FALSE))

table(cut3(numbers,20,"width_min"))
table(cut3(numbers,20,"width_min", crop = TRUE))
table(cut3(numbers,20,"width_max"))
table(cut3(numbers,20,"width_max", crop = TRUE))

##################
# brackets       #
##################

table(cut3(numbers,50,"width", brackets = c("*","","","*"), sep = " ~ "))

numbers2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
  17, 18, 19, 20)

# table(cut3(numbers2,4,"g",optim_fun = balanced, closed="right"))
# ggplot2::cut_number(numbers2,4)
# Hmisc::cut2(numbers2,4)
