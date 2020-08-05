`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

load_file <- function(name, dir) {
  ext <- tools::file_ext(name)
  path <- sprintf("%s/%s", dir, name)

  if (ext == "csv") {
    out <- readr::read_csv(path)
  } else if (ext == "feather") {
    out <- feather::read_feather(path)
  } else if (ext == "json") {
    out <- jsonlite::fromJSON(path)
  } else {
    stop(sprintf("Extension %s not recognised", ext))
  }

  return(out)
}

#' @export
sacredr <- function(exp_dir, artifact_names, load_artifacts = TRUE) {

  eids <- list.dirs(exp_dir, full.names = FALSE) %>%
    grep("[0-9]+", x = ., value = TRUE) %>%
    as.integer()

  tb <- lapply(eids, function(eid) {
    load_file("config.json", sprintf("%s/%s", exp_dir, eid)) %>%
      data.frame()
  }) %>%
    dplyr::bind_rows()

  config_cols <- names(tb)

  tb %<>% bind_cols(tibble(eid = eids), .)

  if (!load_artifacts) {
    return(list(tb = tb, config_cols = config_cols))
  }

  artifact_names <- list.files(sprintf("%s/%s", exp_dir, tb$eid[1]))
  for (file_name in c("config.json", "metrics.json", "cout.txt", "run.json")) {
    artifact_names %<>%
      grep(sprintf("^%s$", file_name), x = ., value = TRUE, invert = TRUE)
  }

  tb %<>% dplyr::rowwise()

  for (artifact_name in artifact_names) {
    col_name <- tools::file_path_sans_ext(artifact_name)
    tb %<>%
      dplyr::mutate(
        !!col_name :=
          load_file(artifact_name, sprintf("%s/%s", exp_dir, eid)) %>% list()
      )
  }

  tb %<>% dplyr::ungroup()

  list(
    tb = tb,
    config_cols = config_cols,
    artifact_cols = sapply(artifact_names, tools::file_path_sans_ext)
  )
}
