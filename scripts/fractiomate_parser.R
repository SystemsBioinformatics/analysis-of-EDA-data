# Parsers
# Using mainly functions from the stringi package, meaning that we use ICU 
# regular expressions: http://userguide.icu-project.org/strings/regexp
# See also https://stringi.gagolewski.com/_static/vignette/stringi.pdf

require(dplyr)
require(lubridate)
require(stringi)

rowletters_to_int <- function(x) {
  # Maximally 25*24 = 600 row names
  # single letters (24) followed by double letter codes (24*24) 
  rownames <- c(LETTERS, paste(rep(LETTERS, each=24), LETTERS, sep=""))
  as.integer(factor(x, levels=rownames))
}

patternlist <- list(
  "extensive" = list(
    meta = list(
      expr = "^.+?\\s+:\\s.+",
      example = "<parameter> : <value>"
    ),
    separator = list(
      expr =  "^-+\\s{0,1}$",
      example = "----------------"
    ),
    # Assuming that timing of start and ready times could be MM:SS or HH:MM:SS, 
    # where seconds (SS) have optionally one decimal number i.e. this regular 
    # expression: "(\d{2}:){1,2}\d{2}(\.\d)?"
    timing = list(
      expr = "^\\d+,\\s+\\d+,\\s+(\\d{2}:){1,2}\\d{2}(\\.\\d)?,\\s+(\\d{2}:){1,2}\\d{2}(\\.\\d)?",
      example = "<INT>,  <INT>,  [<HH>:]<MM>:<SS.S>,  [<HH>:]<MM>:<SS.S>"
    ),
    finish = list(
      expr = "^\\*[Rr]un finished\\*\\s+@(\\d{2}:){1,2}\\d{2}(\\.\\d)?",
      example = "*Run finished* @[<HH>:]<MM>:<SS.S>"
    )
  ),
  "simple" = list(
    meta = list(
      expr = "^.+?:\\s+.+",
      example = "Samplename: Sediment"
    ),
    timing = list(
      expr = "^[Ww]ell\\s+\\w+;\\d+\\s+@\\s+\\d+\\.\\d{2}\\s+min",
      example = "Well A;3 @ 0.23 min"
    )
  )
)


parser_selector <- function(lines) {
  
  # There are different fractiomate file types, until now have seen two:
  #   1 extensive one that starts with "Log file : " and shows bot start
  #     and stop times, has many meta parameters
  #   2 simple one that starts with "Samplename:" and only shows a single
  #     time, and no meta parameters, apart from sample name and start date
  #     and time
  # We call these formats here "extensive" and "simple"
  
  index <- lapply(patternlist, function(x) lapply(x, function(p) stri_detect_regex(lines, p$expr)))
  emptylines <- stri_isempty(lines)
  for (i in seq_along(index)) {
    index[[i]][['emptyline']] <- emptylines
  }
  # Number of matches per line
  matches <- lapply(index, function(x) colSums(do.call(rbind, x)))
  # Fraction of *single* matches, excluding empty lines
  fraction_matches <- sapply(matches, function (x) (sum(x[x<=1]) - sum(emptylines))/(length(x) - sum(emptylines)))
  # Minimal required single match fraction, excluding empty lines
  fr_min <- 0.95
  if (any(fraction_matches >= fr_min)) {
    best_match_name <- names(which.max(fraction_matches))
  } else {
    stop("Fractiomate file type not recognized.")
  }
  if (fraction_matches[best_match_name] < 1) {
    # TODO: report parsing problems for the best match
  }
  return(list(type=best_match_name, index=index[[best_match_name]]))
}


parse_fractiomate_simple <- function(lines, index) {
  patterns <- patternlist[['simple']]
  metalist <- lapply(stri_split_fixed(lines[index$meta],": "), stri_trim_both)
  meta <- lapply(metalist, "[", 2)
  names(meta) <- lapply(metalist, "[", 1)
  # interpreted parameters
  # Assuming that "Startdate & time" has the following format:
  # MM/DD/YYYY II:MM:SS AM/PM
  # date_time_re <- "((\\d{2}/){2}\\d{4}\\s+(\\d{2}:){2}\\d{2}\\s+((?i)AM|PM))"
  interpreted <- list('Run_start_time'= meta[['Startdate & time']] %>% 
                       fast_strptime(format="%m/%d/%Y %I:%M:%S %p")
  )
  timing_re <- "(\\w+);(\\d+)\\s+@\\s+(\\d+\\.\\d{2})"
  timing <- do.call(rbind, lapply(stri_match_all_regex(lines[index$timing], timing_re), "[", 2:4))
  colnames(timing) <- c('well_row','well_column','ready_time')
  timing <- timing %>% 
    as_tibble() %>%
    mutate(start_time=ms(start_time)) %>%
    mutate(across(c(well_column), as.integer))
  timing$well_row <- rowletters_to_int(timing$well_row)
  return(list(meta=meta, interpreted=interpreted, timing=timing))
}

## parser for Fractiomate files
parse_fractiomate_extensive <- function(lines, index) {
  patterns <- patternlist[['extensive']]
  if (!any(index$separator)) {
    message('No separator lines found but expected two lines like "', patterns$separator$example, '". Parameters will not be separated')
  } else {
    index$meta1 <- index$meta & seq_along(index$meta) < which(index$separator)[1]
    index$meta2 <- index$meta & seq_along(index$meta) > which(index$separator)[1]
    metalist1 <- lapply(stri_split_fixed(lines[index$meta1]," : "), stri_trim_both)
    metalist2 <- lapply(stri_split_fixed(lines[index$meta2]," : "), stri_trim_both)
    meta1 <- lapply(metalist1, "[", 2)
    names(meta1) <- lapply(metalist1, "[", 1)
    meta2 <- lapply(metalist2, "[", 2)
    names(meta2) <- lapply(metalist2, "[", 1)
  }
  # interpreted parameters
  # Assuming that "Run Start Time" has the following format:
  # MM/DD/YYYY II:MM:SS AM/PM
  date_time_re <- "((\\d{2}/){2}\\d{4}\\s+(\\d{2}:){2}\\d{2}\\s+((?i)AM|PM))"
  interpreted <- list('Run_start_time'= meta1['Run Start Time'] %>% 
                        stri_extract(regex=date_time_re) %>%
                        fast_strptime(format="%m/%d/%Y %I:%M:%S %p")
                      )
  if ('Start Delay' %in% names(meta1)) {
    interpreted[['Start_delay']] <-
      meta1[['Start Delay']] %>%
      hms()
  } else {
    warning('"Start delay" not found. Assuming 0 seconds')
    interpreted[['Start_delay']] <- ms("00:00")
  }
  if ('Temperature' %in% names(meta2)) {
    interpreted[['Measured_temperature']] <-
      meta2[['Temperature']] %>%
      stri_extract_first_regex("\\d+(\\.\\d+)?") %>%
      as.numeric()
  } else {
    warning("Measured temperature not found")
    interpreted[['Measured_temperature']] <- NA
  }
  time_re <- "(\\d{2}:){1,2}\\d{2}(\\.\\d)?"
  if (sum(index$finish)==0) {
    message('No finish line, but expected something like "', patterns$finish$example, '". Parameter "Finish_time" can not be retrieved')
  } else {
    if (sum(index$finish) > 1) {
      message('Multiple finish lines matching something like "', patterns$finish$example, '". Using only the first match.')
    }
    interpreted[['Finish_time']] <- 
      lines[min(which(index$finish))] %>%
      stri_extract(regex=time_re) %>%
      ms()
  }
  # parameters['Run_Finish_Time']
  # timing of fractions
  timing <- do.call(rbind, lapply(strsplit(lines[index$timing],","),trimws))
  colnames(timing) <- c('well_row','well_column','start_time','ready_time')
  timing <- timing %>%
    as_tibble() %>%
    mutate(start_time=ms(start_time), ready_time=ms(ready_time)) %>%
    mutate(across(c(well_row, well_column),as.integer))
  return((list(meta1=meta1, meta2=meta2, 
               interpreted=interpreted, timing=timing)))
}

parse_fractiomate <- function(file) {
  lines <- readLines(file)
  choice <- parser_selector(lines)
  if (choice$type == "extensive") {
    parse_fractiomate_extensive(lines, choice$index)
  } else {
    if (choice$type == "simple") {
      parse_fractiomate_simple(lines, choice$index)
    }
  }
}
