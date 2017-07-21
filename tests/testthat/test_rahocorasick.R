library(stringi)
suppressMessages(library(purrr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

dict <- list(one = 1, ones = 11, two = 2, three = 3)
test_text <- c("one apple",
               "one two-sided coin",
               "apple three",
               "twoonie in my pocket",
               "everything done",
               "the number twone",
               "ones upon a twones",
               "nothing left")

test_expected <-
  frame_data(~index, ~value, ~begin, ~end,
             1,1,1,3,
             2,1,1,3,
             2,2,5,7,
             3,3,7,11,
             4,2,1,3,
             5,1,13,15,
             6,2,12,14,
             6,1,14,16,
             7,1,1,3,
             7,11,1,4,
             7,2,13,15,
             7,1,15,17,
             7,11,15,18) %>%
  split(., .$index) %>%
  map(~ transmute(.,
                  value = as.character(value),
                  begin = as.integer(begin),
                  end = as.integer(end))) %>%
  c(list(data_frame(value = character(0),
                    begin = integer(0),
                    end = integer(0)))) %>%
  setNames(NULL)


test_that("can build trie without values", {
  trie <- ac_build(names(dict))
  expect_s4_class(trie, "jobjRef")
})

test_that("can build trie with values", {
  trie <- ac_build(names(dict), dict)
  expect_s4_class(trie, "jobjRef")
})

test_that("can build with named list", {
  trie <- ac_build_list(dict)
  expect_s4_class(trie, "jobjRef")
})

test_that("cannot build with unnamed list", {
  expect_error(ac_build_list(setNames(dict, NULL)))
})

test_that("can search without values", {
  trie <- ac_build(names(dict))
  returned <- ac_search(test_text, trie)
  pwalk(list(returned,
             expected = test_expected %>% map(~ mutate(., value = NA_character_)),
             test_text),
        function(returned, expected, text) {
          info <- returned %>%
            mutate(returned = T) %>%
            full_join(expected %>%
                        rename(returned_value = value) %>%
                        mutate(expected = T),
                      by = c("begin", "end")) %>%
            filter(is.na(returned) | is.na(expected)) %>%
            mutate(ref = stri_sub(text, begin, end)) %>%
            as.data.frame() %>%
            pander::pander_return() %>%
            discard(stri_isempty) %>%
            paste(collapse = "\n")
          expect_equivalent(returned, expected, info = info)
        })
})

test_that("can search with values", {
  trie <- ac_build(names(dict), dict)
  returned <- ac_search(test_text, trie)
  expect_equal(returned, test_expected)
})

test_that("can search with named list", {
  trie <- ac_build_list(dict)
  returned <- ac_search(test_text, trie)
  expect_equal(returned, test_expected)
})

test_that("can build and search without values", {
  returned <- ac_build_and_search(test_text, names(dict))
  expect_equal(returned, test_expected %>% map(~ mutate(., value = NA_character_)))
})

test_that("can build and search with values", {
  returned <- ac_build_and_search(test_text, names(dict), dict)
  expect_equal(returned, test_expected)
})

test_that("can build and search with named list", {
  returned <- ac_build_and_search_list(test_text, dict)
  expect_equal(returned, test_expected)
})

test_that("can remove overlaps in use", {
  expected <- frame_data(~group, ~begin, ~end, ~overlaps,
             1, 1, 3, T,
             1, 2, 4, F,
             2, 1, 3, F,
             2, 2, 5, T,
             3, 1, 5, T,
             3, 2, 4, F,
             4, 1, 3, T,
             4, 4, 5, T)

  returned <- expected %>%
    with(ac_is_overlapping(begin, end, group))

  expect_equal(returned, expected$overlaps)
})

test_that("can remove overlaps in use", {
  returned <- data_frame(text = test_text[6:7]) %>%
    rowwise %>%
    mutate(references = (ac_build_and_search_list(text,dict))) %>%
    ungroup %>%
    unnest(references) %>%
    mutate(reference = stri_sub(text, begin, end)) %>%
    filter(ac_is_overlapping(begin, end, text))
  expect_equal(returned$text, test_text[c(6, 7, 7)])
})

test_that("can detect word boundaries", {
  text <- "one two-sided coin"
  expected <-
    frame_data(~text, ~begin, ~end, ~is_word,
               text, 1, 3, T,
               text, 1, 4, F,
               text, 5, 7, T,
               text, 1, 7, T,
               text, 1, 13, T,
               text, 2, 3, F,
               text, 1, 2, F,
               text, 5, 6, F,
               text, 1, 18, T,
               text, 15, 18, T,
               text, 14, 18, F,
               text, 16, 18, F)

  returned <- expected %>%
    with(ac_are_boundary_chars(text, begin, end))

  info <- expected %>%
    transmute(row = 1:n(),
              text,
              begin,
              end,
              expected = is_word,
              returned,
              substring = paste0("'", stri_sub(text, begin, end), "'")) %>%
    filter(expected != returned) %>%
    as.data.frame %>%
    pander::pander_return() %>%
    discard(stri_isempty) %>%
    paste(collapse = "\n")
  expect_equal(returned, expected$is_word, info = info)

})
