context("smart_cut")
library(cutr)

expect_dstr <- function(distr, cuts) {
  expect_equal(distr, tabulate(cuts))
}

expect_brks <- function(brks, cuts) {
  expect_equal(brks, levels(cuts))
}

dp <- function(){
  print(dput(as.double(tabulate(.Last.value))))
  dput(levels(.Last.value))}


x      <- c(rep(1,7),rep(2,5),3:6,17:20)
table(smart_cut(x, 4, "width", closed = "right", open_end = FALSE, expand = FALSE))


test_that("left and right work well with breaks on boundaries or values or empty space", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(13, 3, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "right", open_end = FALSE, expand = FALSE))
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = FALSE, expand = FALSE))
  breaks <- c(1, 6, 17, 20)
  expect_dstr(c(16, 1, 3), smart_cut(x, breaks, "breaks", closed = "right", open_end = FALSE, expand = FALSE))
  expect_dstr(c(15, 1, 4), smart_cut(x, breaks, "breaks", closed = "left", open_end = FALSE, expand = FALSE))
})

test_that("NAs don't break the functions", {
  x      <- c(rep(1,7),NA,rep(2,5),NA,3:6,17:20)
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(13, 3, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "right", open_end = FALSE, expand = FALSE))
})

test_that("open_end works on both sides with both 'closed' values", {
  # we already checked for open_end = FALSE
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(1, 6, 17, 20)
  expect_dstr(c(9, 1, 3), smart_cut(x, breaks, "breaks", closed = "right", open_end = TRUE, expand = FALSE))
  expect_dstr(c(15, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = FALSE))
})

test_that("expand works as intended", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)

  # expand when there's nothing to expand because breaks are outside
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  # maybe should be open on the right, but it's true in any case, so maybe add note that what is expanded is always closed
  expect_brks(c("[0,3)", "[3,8)", "[8,14)", "17", "[17.3,22]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))

  # expand when one side needs to be expanded because we cut on an open boundary
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2, 1), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)", "[8,14)", "17", "[17.3,20)","20"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))

  # expand in most common intended use
  breaks <- c(3,10,15)
  expect_dstr(c(12, 4, 0, 4), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,10)", "[10,15)", "[15,20]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
})


test_that("crop works as intended", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # crop on boundaries with one open and one closed side
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "17", "[17.3,20]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))

  # crop on xmin and xmax with one open and one closed side
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)",  "17", "[17.3,19]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))

  # crop when we have several cuts outside of boundaries
  breaks <- c(-1, 0, 10, 21, 22)
  expect_dstr(c(0, 16, 3, 1), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[-1,1)", "[1,10)", "[10,20)", "[20,22]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE, simplify = FALSE))
  # counter intuitive but 21 is moved to 20 (where else ?), and we can't close on both sides there as we're in the middle (or should we ?)
  # We could have either "[10,20]", "(20,22]" or "[10,20]", "[21,22]", but none of them works with .binCode
  # a check could be done in the end but we lost some info, that the cut has moved, it changes because of opennes of boundary
  # maybe duplicating the cut could be a way to transport that info, and we fix all in the end, see .bincode(1:10,c(1,3,10,10)).
  # but it's complicated and probably an uncommon use case.
  # RE: not so complicated, duplicate cuts and they'll be empty bins, then in the end check if we cropped,
  # if we cropped empty bins should not exist on extremities, so we can do some play on labels and fix them
  # these empty intervals will have open brackets on both sides
  # last R allows duplicate labels but let's not use this feature so early

  # crop for main intended use
  breaks <- c(2,5,10)
  expect_dstr(c(7,2), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[2,5)", "[5,6]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
})


test_that("crop after expand", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # crop on boundaries with one open and one closed side
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "17", "[17.3,20]"), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))

  # crop on xmin and xmax with one open and one closed side
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2, 1), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "[14,17.3)", "[17.3,20)", "[20,20]"),
              smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))
  # same but no open_end
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = FALSE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "[14,17.3)", "[17.3,20]"),
              smart_cut(x, breaks, "breaks", closed = "left", open_end = FALSE, crop = TRUE, expand = TRUE, simplify = FALSE))

  # crop when we have several cuts outside of boundaries
  breaks <- c(-1, 0, 10, 21, 22)
  expect_dstr(c(0, 16, 3, 1), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[-1,1)", "[1,10)", "[10,20)", "[20,22]"),
              smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))

  # crop for main intended use
  breaks <- c(2,5,10)
  expect_dstr(c(7,7,2,4), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,2)","[2,5)", "[5,10)", "[10,20]"),
              smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))
})


# to do : crop after expand edge cases

test_that("squeeze", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, squeeze = TRUE))
  expect_brks(c("[1,2]", "[3,6]", "(8,14)", "17", "[18,20]"),
              smart_cut(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE, squeeze = TRUE))
  expect_brks(c("(-5,0)","(0,0)","[1,6]","(8,12)","[17,20]","(30,40)"),
              smart_cut(x,c(-5,0,1,8,12,30,40),squeeze = TRUE))

})


test_that("what = 'width' works as expected", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # default behavior implicit
  . <- smart_cut(x, 6, "width", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(16, 0, 2, 2), .)
  expect_brks(c("[1,7)", "[7,13)", "[13,19)", "[19,25]"), smart_cut(x, 6, "width", closed = "left", open_end = TRUE, crop = FALSE))

  # default behavior explicit
  expect_dstr(c(16, 0, 2, 2), smart_cut(x, list(6,"left"), "width", closed = "left", open_end = TRUE, crop = FALSE))
  expect_brks(c("[1,7)", "[7,13)", "[13,19)", "[19,25]"), smart_cut(x, list(6,"left"), "width", closed = "left", open_end = TRUE, crop = FALSE))

  # right
  . <- smart_cut(x, list(6,"right"), "width", closed = "left", open_end = TRUE,
             crop = FALSE, simplify = FALSE)
  expect_dstr(c(7, 9, 0, 3, 1), .)
  expect_brks(c("[-4,2)", "[2,8)", "[8,14)", "[14,20)", "[20,20]"), .)

  # centered
  . <- smart_cut(x, list(6,"centered"), "width", closed = "left", open_end = TRUE,
             crop = FALSE, simplify = FALSE)
  expect_dstr(c(14, 2, 0, 4), .)
  expect_brks(c("[-1.5,4.5)", "[4.5,10.5)", "[10.5,16.5)", "[16.5,22.5]"), .)

  # centered0
  . <- smart_cut(x, list(6,"centered0"), "width", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(12, 4, 0, 4), .)
  expect_brks(c("[-3,3)", "[3,9)", "[9,15)", "[15,21]"), .)

  # custom
  . <- smart_cut(x, list(6,~min(.x)-.y/3), "width", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(14, 2, 0, 4), .)
  expect_brks(c("[-1,5)", "[5,11)", "[11,17)", "[17,23]"),.)

  # numeric
  . <- smart_cut(x, list(6,-2), "width", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(13, 3, 0, 4), .)
  expect_brks(c("[-2,4)", "[4,10)", "[10,16)", "[16,22]"),.)
})


test_that("what = 'groups' works as expected", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # default groups
  . <- smart_cut(x, 2, "groups", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(7, 13), .)
  expect_brks(c("1", "[2,20]"), .)

  # balanced
  . <- smart_cut(x, list(2,"balanced"), "groups", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(12, 8), .)
  expect_brks(c("[1,3)", "[3,20]"), .)

  . <- smart_cut(x, list(6,"balanced"), "groups", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(7, 5, 2, 2, 2, 2), .)
  expect_brks(c("1", "2", "[3,5)", "[5,17)", "[17,19)", "[19,20]"), .)

  # biggest small bin
  . <- smart_cut(x, list(2,"biggest_small_bin"), "groups", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(12, 8), .)
  expect_brks(c("[1,3)", "[3,20]"), .)

  # smallest big bin
  . <- smart_cut(x, list(2,"biggest_small_bin"), "groups", closed = "left", open_end = TRUE, crop = FALSE)
  expect_dstr(c(12, 8), .)
  expect_brks(c("[1,3)", "[3,20]"), .)

})


test_that("custom brackets works", {
  x <- c(rep(1,7),rep(2,5),3:6,17:20)
  . <- smart_cut(x, 2, "groups", closed = "left", open_end = TRUE, crop = FALSE,
                 brackets = NULL, sep = "-")
  . <- smart_cut(x, 2, "groups", closed = "left", squeeze = TRUE, open_end = TRUE,
                 crop = FALSE, brackets = NULL,sep = "-")

})


test_that("NAs don't break the package", {
  x <- c(NA,rep(1,7),NA, rep(2,5),3:6,17:20,NA,NA)
  . <- smart_cut(x, 2, "groups", closed = "left", open_end = TRUE, crop = FALSE,
                 brackets = NULL, sep = "-")

})
