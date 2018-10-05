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
# table(smart_cut(numbers,-5,"breaks", format_fun = format))
# #table(smart_cut(numbers,-5,"breaks", expand = FALSE)) # should return explicit error
#
# # we can pass the trim parameter to have
# # results similar to cut
# table(smart_cut(numbers,-5,"breaks", format_fun = format, trim = TRUE))
#
# # or change the formatting function to formatC
# table(smart_cut(numbers,-5,"breaks", format_fun = formatC))
#
# # one cut outside right
# table(smart_cut(numbers,300,"breaks"))
#
# # cuts inside, default expand = TRUE
# table(smart_cut(numbers,c(100,150,200),"breaks"))
#
# # cuts inside, expand = FALSE
# table(smart_cut(numbers,c(100,150,200),"breaks",expand = FALSE))
#
# # cuts outside, default crop = FALSE
# table(smart_cut(numbers,c(0,150,500),"breaks"))
#
# # cuts outside, crop = TRUE
# table(smart_cut(numbers,c(0,150,500),"breaks",crop = TRUE))
#
# # mixing both
# table(smart_cut(numbers,c(100,150,200),"breaks",expand = FALSE, crop = TRUE))


# ##################
# # groups         #
# ##################
#
# table(smart_cut(numbers,6,"groups"))
#
# ##################
# # n_intervals    #
# ##################
#
# table(smart_cut(numbers,4,"n_intervals"))
# # no surprise with expand and crop
# table(smart_cut(numbers,4,"n_intervals",expand=TRUE))
# table(smart_cut(numbers,4,"n_intervals",crop=TRUE))
#
# ##################
# # width          #
# ##################
#
# table(smart_cut(numbers,50,"width"))
# table(smart_cut(numbers,50,"width",crop = TRUE))
#
# table(smart_cut(numbers,50,"width_0",oneval = FALSE))
#
# table(smart_cut(numbers,20,"width_min"))
# table(smart_cut(numbers,20,"width_min", crop = TRUE))
# table(smart_cut(numbers,20,"width_max"))
# table(smart_cut(numbers,20,"width_max", crop = TRUE))
#
# ##################
# # brackets       #
# ##################
#
# table(smart_cut(numbers,50,"width", brackets = c("*","","","*"), sep = " ~ "))
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
# debugonce(smart_cut)
# debugonce(cut_explicit)
# table(smart_cut(numbers,c(0,100,200),"breaks",c("low","high")))
# table(smart_cut(numbers,c(0,100,200),"breaks",middle))
#
# ################################
# # crop and interval numbers    #
# ################################
#
# debugonce(cut_explicit)
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks"))
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right"))
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right",simplify=FALSE))
#
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks", crop= TRUE))
# # what I feared
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks", crop=TRUE, closed="right"))
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks",closed="right",simplify=FALSE))
#
#
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks", crop = TRUE , closed="right", simplify = FALSE))
# table(smart_cut(c(1,2,3),c(0,1,2.5,5),"breaks", crop = FALSE, closed="right", simplify = FALSE))
#
#
# # table(smart_cut(numbers2,4,"g",optim_fun = balanced, closed="right"))
# # ggplot2::cut_number(numbers2,4)
# # Hmisc::cut2(numbers2,4)
