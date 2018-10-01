context("cut3")
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
table(cut3(x, 4, "width", closed = "right", open_end = FALSE, expand = FALSE))
test_that("left and right work well with breaks on boundaries or values or empty space", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(13, 3, 0, 1, 3), cut3(x, breaks, "breaks", closed = "right", open_end = FALSE, expand = FALSE))
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = FALSE, expand = FALSE))
  breaks <- c(1, 6, 17, 20)
  expect_dstr(c(16, 1, 3), cut3(x, breaks, "breaks", closed = "right", open_end = FALSE, expand = FALSE))
  expect_dstr(c(15, 1, 4), cut3(x, breaks, "breaks", closed = "left", open_end = FALSE, expand = FALSE))
})

test_that("open_end works on both sides with both 'closed' values", {
  # we already checked for open_end = FALSE
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(1, 6, 17, 20)
  expect_dstr(c(9, 1, 3), cut3(x, breaks, "breaks", closed = "right", open_end = TRUE, expand = FALSE))
  expect_dstr(c(15, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = FALSE))
})

test_that("expand works as intended", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)

  # expand when there's nothing to expand because breaks are outside
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  # maybe should be open on the right, but it's true in any case, so maybe add note that what is expanded is always closed
  expect_brks(c("[0,3)", "[3,8)", "[8,14)", "17", "[17.3,22]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))

  # expand when one side needs to be expanded because we cut on an open boundary
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2, 1), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)", "[8,14)", "17", "[17.3,20)","20"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))

  # expand in most common intended use
  breaks <- c(3,10,15)
  expect_dstr(c(12, 4, 0, 4), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,10)", "[10,15)", "[15,20]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, expand = TRUE))
})


test_that("crop works as intended", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # crop on boundaries with one open and one closed side
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "17", "[17.3,20]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))

  # crop on xmin and xmax with one open and one closed side
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)",  "17", "[17.3,19]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))

  # crop when we have several cuts outside of boundaries
  breaks <- c(-1, 0, 10, 21, 22)
  expect_dstr(c(0, 16, 3, 1), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[-1,1)", "[1,10)", "[10,20)", "[20,22]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE, simplify = FALSE))
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
  expect_dstr(c(7,2), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
  expect_brks(c("[2,5)", "[5,6]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE))
})


test_that("crop after expand", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  # crop on boundaries with one open and one closed side
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "17", "[17.3,20]"), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))

  # crop on xmin and xmax with one open and one closed side
  breaks <- c(1, 3, 8, 14,17.3, 20)
  expect_dstr(c(12, 4, 0, 1, 2, 1), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "[14,17.3)", "[17.3,20)", "[20,20]"),
              cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))
  # same but no open_end
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = FALSE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,3)", "[3,8)","[8,14)", "[14,17.3)", "[17.3,20]"),
              cut3(x, breaks, "breaks", closed = "left", open_end = FALSE, crop = TRUE, expand = TRUE, simplify = FALSE))

  # crop when we have several cuts outside of boundaries
  breaks <- c(-1, 0, 10, 21, 22)
  expect_dstr(c(0, 16, 3, 1), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[-1,1)", "[1,10)", "[10,20)", "[20,22]"),
              cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))

  # crop for main intended use
  breaks <- c(2,5,10)
  expect_dstr(c(7,7,2,4), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE))
  expect_brks(c("[1,2)","[2,5)", "[5,10)", "[10,20]"),
              cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = TRUE, simplify = FALSE))
})


# to do : crop after expand edge cases

test_that("squeeze", {
  x      <- c(rep(1,7),rep(2,5),3:6,17:20)
  breaks <- c(0, 3, 8, 14,17.3, 22)
  expect_dstr(c(12, 4, 0, 1, 3), cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, squeeze = TRUE))
  expect_brks(c("[1,2]", "[3,8]", "(8,14)", "17", "[18,20]"),
              cut3(x, breaks, "breaks", closed = "left", open_end = TRUE, crop = TRUE, expand = FALSE, squeeze = TRUE))
})
