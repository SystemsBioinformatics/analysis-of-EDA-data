# Parsers
# Using mainly functions from the stringi package, meaning that we use ICU 
# regular expressions: http://userguide.icu-project.org/strings/regexp
# See also https://stringi.gagolewski.com/_static/vignette/stringi.pdf

require(dplyr)
require(lubridate)
require(stringi)

## parser for Fractiomate files
parse_fractiomate <- function(file) {
  # Assuming that timing of start and ready times could be MM:SS or HH:MM:SS, 
  # where seconds (SS) have optionally one decimal number i.e. this regular 
  # expression: "(\d{2}:){1,2}\d{2}(\.\d)?"
  patterns <- list(
    meta = list(
      expr = "^.+?\\s+:\\s.+",
      example = "<parameter> : <value>"
    ),
    separator = list(
      expr =  "^-+\\s{0,1}$",
      example = "----------------"
    ),
    timing = list(
      expr = "^\\d+,\\s+\\d+,\\s+(\\d{2}:){1,2}\\d{2}(\\.\\d)?,\\s+(\\d{2}:){1,2}\\d{2}(\\.\\d)?",
      example = "<INT>,  <INT>,  [<HH>:]<MM>:<SS.S>,  [<HH>:]<MM>:<SS.S>"
    ),
    finish = list(
      expr = "^\\*[Rr]un finished\\*\\s+@(\\d{2}:){1,2}\\d{2}(\\.\\d)?",
      example = "*Run finished* @[<HH>:]<MM>:<SS.S>"
    )
  )
  all_lines <- readLines(file)
  index <- lapply(patterns, function(x) stri_detect_regex(all_lines, x$expr))
  # Empty lines
  index[['emptyline']] <- stri_isempty(all_lines)
  # Does any line have multiple interpretations?
  nrofinterp <- colSums(do.call(rbind,index))
  if (any(nrofinterp > 1)) {
    stop("Line numbers are multi-interpretable: ", paste(which(nrofinterp > 1), collapse = ","))
  }
  # TODO checking indices and generating messages if needed
  if (!any(index$separator)) {
    message('No separator lines found but expected two lines like "', patterns$separator$example, '". Parameters will not be separated')
  } else {
    index$meta1 <- index$meta & seq_along(index$meta) < which(index$separator)[1]
    index$meta2 <- index$meta & seq_along(index$meta) > which(index$separator)[1]
    metalist1 <- lapply(stri_split_fixed(all_lines[index$meta1]," : "), stri_trim_both)
    metalist2 <- lapply(stri_split_fixed(all_lines[index$meta2]," : "), stri_trim_both)
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
      all_lines[min(which(index$finish))] %>%
      stri_extract(regex=time_re) %>%
      ms()
  }
  # parameters['Run_Finish_Time']
  # timing of fractions
  timing <- do.call(rbind, lapply(strsplit(all_lines[index$timing],","),trimws))
  colnames(timing) <- c('well_row','well_column','start_time','ready_time')
  timing <- timing %>%
    as_tibble() %>%
    mutate(start_time=ms(start_time), ready_time=ms(ready_time)) %>%
    mutate(across(c(well_row, well_column),as.integer))
  return((list(meta1=meta1, meta2=meta2, 
               interpreted=interpreted, timing=timing)))
}
