#' @import dplyr stringi rJava
NULL

.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
}

ac_build <- function(keys, values) {
  if(missing(values)) {
    values <- keys %>% map(~ .jnull())
  }
  map <- new(J("java.util.HashMap"))
  walk2(keys, values, map$put)
  trie <- new(J("com.hankcs.algorithm.AhoCorasickDoubleArrayTrie"))
  trie$build(map)
  trie
}

ac_value <- function(hit) {
  value <- hit$value
  if(is.jnull(value)) {
    NA_character_
  } else {
    value
  }
}

ac_search <- function(trie, text) {
  hit <- trie$parseText(text) %>% as.list
  ref <- hit %>% map_chr(~ stri_sub(text, .$begin + 1, .$end))
  value <- hit  %>% map_chr(ac_value)
  begin <- hit %>% map_dbl(~ .$begin + 1)
  end <- hit %>% map_dbl(~ .$end)
  data_frame(ref, value, begin, end)
}

ac_remove_overlaps <- function(res, keep_order = F) {
  if(!keep_order) {
    res <- res %>% arrange(desc(end - begin), begin)
  }
  res_indices <- res %>% transmute(id = row_number(), begin, end)
  selected <- logical(nrow(res_indices))
  while(nrow(res_indices) > 0) {
    selected[res_indices$id[1]] <- T
    res_indices <- res_indices %>%
      filter(begin >= res_indices$end[1] | end <= res_indices$begin[1])
  }
  res[selected,]
}

ac_save <- function(trie, file) {
  out <- new(J("java.io.ObjectOutputStream"),
        new(J("java.io.FileOutputStream"),
            file))
  trie$save(out)
  out$close()
  invisible(trie)
}

ac_load <- function(file) {
  is <- new(J("java.io.ObjectInputStream"),
             new(J("java.io.FileInputStream"),
                 file))
  trie <- new(J("com.hankcs.algorithm.AhoCorasickDoubleArrayTrie"))
  trie$load(is)
  is$close()
  trie
}

