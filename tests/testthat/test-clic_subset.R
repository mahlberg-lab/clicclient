test_that("parsing subset response works", {
  
  response_json <- list(
    list("the", " ", "room", ",", c(0,2)),
    list("'", "God", " ", "grant", " ", "he", " ", "be", " ", "not", " ", "disappointed", "!", " ", "I", " ", "know", " ", "not", " ", "how", " ", "he", " ", "would", " ", "bear", " ", "it", c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27)),
    list(list()),
    c("AgnesG", "7358", "7431"),
    c(1, 9, 31)
  )
  
  expected_df <- data.frame(
    left = "the room,",
    node = "'God grant he be not disappointed! I know not how he would bear it",
    right = "",
    book = "AgnesG"
  )
  
  expect_equal(subset_df(response_json), expected_df)
})

test_that("building subset query works", {
  input_args_1 <- list(corpora = "dickens", subset = "quote")
  expected_query_list_1 <- list(corpora = "dickens", subset = "quote")
  
  expect_equal(subset_query_list(input_args_1), expected_query_list_1)
  
  input_args_2 <- list(corpora = c("dickens", "AgnesG"), subset = "quote")
  expected_query_list_2 <- list(corpora = "dickens", corpora = "AgnesG", subset = "quote")
  
  expect_equal(subset_query_list(input_args_2), expected_query_list_2)
})
