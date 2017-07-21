#' @import dplyr rJava
NULL

.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
}


#' @export
ac_build_list <- function(dictionary) {
  if(!is.list(dictionary) || is.null(names(dictionary))) {
    stop("dictionary must be a named list")
  }
  ac_build(names(dictionary), dictionary)
}

#' @export
ac_build <- function(keys, values = NULL) {
  keys <- as.character(keys)
  if(is.null(values)) {
    J("ahocorasick.AhoCorasickWrapper")$build(.jarray(keys))
  } else {
    J("ahocorasick.AhoCorasickWrapper")$build(.jarray(keys), .jarray(as.character(values)))
  }
}

#' @export
ac_search <- function(text, trie) {
  text_ided <- data_frame(text) %>%
    mutate(text_index = 1L:length(text) - 1L)

  refs <- J("ahocorasick.AhoCorasickWrapper")$detect(.jarray(text), trie)
  refs_df <- data_frame(text_index = refs$getTextIndex(),
                        value = refs$getValue(),
                        begin = refs$getBegin() + 1L,
                        end = refs$getEnd()) %>%
    group_by(text_index) %>%
    tidyr::nest(value, begin, end, .key = references)

  no_refs_df <- text_ided %>%
    anti_join(refs_df, by = "text_index") %>%
    mutate(references = list(data_frame(value = character(0), begin = integer(0), end = integer(0))))

  text_ided %>%
    inner_join(refs_df, by = "text_index") %>%
    bind_rows(no_refs_df) %>%
    arrange(text_index) %>%
    with(references)
}

#' @export
ac_build_and_search <- function(text, keys, values = NULL) {
  trie <- ac_build(keys, values)
  ac_search(text, trie)
}

#' @export
ac_build_and_search_list <- function(text, dictionary) {
  trie <- ac_build_list(dictionary)
  ac_search(text, trie)
}


#' @export
ac_is_overlapping <- function(begin, end, ..., keep_order = F) {
  dots <- rlang::exprs(...)
  data <- data_frame(begin, end, ...) %>%
    group_by(!!!dots)
  data$gid <- group_indices(data)

  data <- data %>%
    ungroup %>%
    transmute(gid, row_id = row_number(), begin, end)

  if(!keep_order) {
    data <- data %>%
      arrange(gid, desc(end - begin), begin)
  }

  selected <- logical(nrow(data))

  while(nrow(data) > 0) {
    next_bests <- data %>%
      group_by(gid) %>%
      summarise(best_row_id = first(row_id),
                best_begin = first(begin),
                best_end = first(end))
    selected[next_bests$best_row_id] <- T
    data <- data %>%
      inner_join(next_bests, by = "gid") %>%
      filter(begin > best_end | end < best_begin) %>%
      select(gid, row_id, begin, end)
  }

  selected
}

#' @export
ac_are_boundary_chars <- function(text, begin, end) {
  purrr::pmap_lgl(
    list(stringi::stri_locate_all_words(text),
         begin,
         end),
    function(word_boundaries, b, e) {
      b %in% word_boundaries[, "start"] &
        e %in% word_boundaries[, "end"]
    })
}

#' @export
ac_write_trie <- function(trie, file) {
  out <- new(J("java.io.ObjectOutputStream"),
        new(J("java.io.FileOutputStream"),
            file))
  trie$save(out)
  out$close()
  invisible(trie)
}

#' @export
ac_read_trie <- function(file) {
  is <- new(J("java.io.ObjectInputStream"),
             new(J("java.io.FileInputStream"),
                 file))
  trie <- new(J("com.hankcs.algorithm.AhoCorasickDoubleArrayTrie"))
  trie$load(is)
  is$close()
  trie
}

