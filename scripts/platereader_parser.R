# Parsers
# Using mainly functions from the stringi package, meaning that we use ICU 
# regular expressions: http://userguide.icu-project.org/strings/regexp
# See also https://stringi.gagolewski.com/_static/vignette/stringi.pdf

require(dplyr)
require(lubridate)
require(stringi)

## Utility functions
# Creates a list of start and end demarcations of sections of a type, plate or 
# iteration index
sections <- function(idx) {
  N <- length(idx)
  start <- which(idx)
  end <- start + diff(c(start, N+1)) - 1
  return(lapply(seq_along(start), function(i) {list('start'=start[i], 'end'=end[i])}))
}

# Creates a nested list of type, plate and iteration sections
nested_sections <- function(idx) {
  type <- sections(idx$type)
  plate <- sections(idx$plate)
  iter <- sections(idx$iteration)
  sections <- list('types'=list())
  for (ity in seq_along(type)) {
    sections$types[[ity]] <- type[[ity]]
    sections$types[[ity]]$plates <- 
      plate[sapply(plate, function(x) {
        x$start > sections$types[[ity]]$start & 
          x$end <= sections$types[[ity]]$end
      })]
    for (ipl in seq_along(sections$types[[ity]]$plates)) {
      sections$types[[ity]]$plates[[ipl]]$iter <- list()
      if (length(iter) > 0) {
        iter_within <- sapply(iter, function(x) {
          x$start > sections$types[[ity]]$plates[[ipl]]$start & 
            x$end <= sections$types[[ity]]$plates[[ipl]]$end
          })
        if (length(iter_within) > 0) {
          sections$types[[ity]]$plates[[ipl]]$iter <- iter[iter_within]
        }
      }
    }
  }
  return(sections)
}

## Parser for raw platereader files
parse_platereader <- function(file) {
  # Assuming the following hierarchical ordering of these files:
  # [Measurement type [Plate nr & wavelengths[Iteration nr](1:m)](1:n)](1:p)
  # If the number of iterations (m) equals 1 then the iteration number is not
  # explicitly given in the file.
  # Below these hierarchies are represented as nested sections
  patterns <- list(
    type = list(
      expr = "^[Rr]esults of (\\w+)",
      example = "Results of <type>"
    ),
    plate = list(
      expr = "^\\s+[Pp]late\\s?:\\s?(\\d+)\\s+\\-\\s+[Ww]avelength\\s?:\\s?(\\S+)",
      example = "Plate:<nr> - Wavelength:<wl>"
    ),
    iteration = list(
      expr = "^\\s?[Ii]teration\\s?:\\s?(\\d+)",
      example = "Iteration: <nr>"
    ),
    read = list(
      expr = "^(\\s\\d+(?:\\.\\d+)?)+",
      example = "<nr> <nr> <nr> ..."
    )
  )
  all_lines <- readLines(file)
  index <- lapply(patterns, function(x) stri_detect_regex(all_lines, x$expr))
  index[['emptyline']] <- stri_isempty(all_lines)
  # Does any line have multiple interpretations?
  nrofinterp <- colSums(do.call(rbind,index))
  if (any(nrofinterp > 1)) {
    stop("Line numbers are multi-interpretable: ", paste(which(nrofinterp > 1), collapse = ","))
  }
  readlist <- list()
  read_nr <- 1
  sections <- nested_sections(index)
  for (ty in sections$types) {
    m_type <- all_lines[ty$start] %>%
      stri_match_first_regex(patterns$type$expr) %>%
      `[`(1,2)
    for (pl in ty$plates) {
      plate_info <- all_lines[pl$start] %>%
        stri_match_first_regex(patterns$plate$expr)
      m_plate <- plate_info[1,2]
      m_wavel <- plate_info[1,3]
      if (length(pl$iter) == 0) {
        # implicit iteration demarcations
        iterlist <- list(list('start'=pl$start + 1, 'end'=pl$end))
      } else {
        iterlist <- pl$iter
      }
      for (it in iterlist) {
        if (length(pl$iter) > 0) {
          m_iter <- all_lines[it$start] %>%
            stri_match_first_regex(patterns$iteration$expr) %>%
            `[`(1,2)
        } else {
          # implicit iteration number 1
          m_iter <- '1'
        }
        cur_read <- index$read & 
          seq_along(index$read) > it$start &
          seq_along(index$read) <= it$end
        m_value <- all_lines[cur_read] %>%
          stri_trim_both() %>%
          stri_split_regex("\\s+")
        m_row <- 
          unlist(lapply(seq_along(m_value), function(x) {rep(x,length(m_value[[x]]))}))
        m_col <-
          unlist(lapply(seq_along(m_value), function(x) {seq_along(m_value[[x]])}))
        m_value <- as.numeric(unlist(m_value))
        readlist[[read_nr]] <- tibble(m_type, m_plate, m_wavel, m_iter, m_row, m_col, m_value)
        read_nr <- read_nr+1
      }
    }
  }
  return(readlist %>% bind_rows())
}
