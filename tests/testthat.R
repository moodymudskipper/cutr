library(testthat)
library(cutr)

test_check("cutr")

# set.seed(1)
# numbers  <- round(cumsum(abs(rnorm(20,10,10))))


# has_n_bins(n, call) / nth_label_is(ns, vals) / nth_value_is(ns, vals)



# from 4 to 262

# maybe additional parameter : crop would be nice, goes well with expand,
# makes breaks and width0 etc more flexible and we can forget about complex variants

# regular breaks



##################
#    breaks      #
##################

#
# # one cut outside left
# table(cutf3(numbers,-5,"breaks", format_fun = format))
# #table(cutf3(numbers,-5,"breaks", expand = FALSE)) # should return explicit error
#
# # we can pass the trim parameter to have
# # results similar to cut
# table(cutf3(numbers,-5,"breaks", format_fun = format, trim = TRUE))
#
# # or change the formatting function to formatC
# table(cutf3(numbers,-5,"breaks", format_fun = formatC))
#
# # one cut outside right
# table(cutf3(numbers,300,"breaks"))
#
# # cuts inside, default expand = TRUE
# table(cutf3(numbers,c(100,150,200),"breaks"))
#
# # cuts inside, expand = FALSE
# table(cutf3(numbers,c(100,150,200),"breaks",expand = FALSE))
#
# # cuts outside, default crop = FALSE
# table(cutf3(numbers,c(0,150,500),"breaks"))
#
# # cuts outside, crop = TRUE
# table(cutf3(numbers,c(0,150,500),"breaks",crop = TRUE))
#
# # mixing both
# table(cutf3(numbers,c(100,150,200),"breaks",expand = FALSE, crop = TRUE))


# ##################
# # groups         #
# ##################
#
# table(cutf3(numbers,6,"groups"))
#
# ##################
# # n_intervals    #
# ##################
#
# table(cutf3(numbers,4,"n_intervals"))
# # no surprise with expand and crop
# table(cutf3(numbers,4,"n_intervals",expand=TRUE))
# table(cutf3(numbers,4,"n_intervals",crop=TRUE))
#
# ##################
# # width          #
# ##################
#
# table(cutf3(numbers,50,"width"))
# table(cutf3(numbers,50,"width",crop = TRUE))
#
# table(cutf3(numbers,50,"width_0",oneval = FALSE))
#
# table(cutf3(numbers,20,"width_min"))
# table(cutf3(numbers,20,"width_min", crop = TRUE))
# table(cutf3(numbers,20,"width_max"))
# table(cutf3(numbers,20,"width_max", crop = TRUE))
#
# ##################
# # brackets       #
# ##################
#
# table(cutf3(numbers,50,"width", brackets = c("*","","","*"), sep = " ~ "))
#
# numbers2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
#   2, 2, 2, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
#   17, 18, 19, 20)
#
#
# Hmisc::cut2(x, c(0, 5, 10, 20))
# ##################
# # labels         #
# ##################
#
# debugonce(cutf3)
# debugonce(cut_explicit)
# table(cutf3(numbers,c(0,100,200),"breaks",c("low","high")))
# table(cutf3(numbers,c(0,100,200),"breaks",middle))
#
# ################################
# # crop and interval numbers    #
# ################################
#
# debugonce(cut_explicit)
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks"))
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right"))
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right",simplify=FALSE))
#
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks", crop= TRUE))
# # what I feared
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks", crop=TRUE, closed="right"))
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right",simplify=FALSE))
#
#
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks", crop = TRUE , closed="right", simplify = FALSE))
# table(cutf3(c(1,2,3),c(0,1,2.5,5),"breaks", crop = FALSE, closed="right", simplify = FALSE))
#
#
# # table(cutf3(numbers2,4,"g",optim_fun = balanced, closed="right"))
# # ggplot2::cut_number(numbers2,4)
# # Hmisc::cut2(numbers2,4)
