#' Get researcher's citation history
#'
#' @description A fucntion to fetch citation history of a researcher from Google Scholar.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fGetResCitHist <- function(inResID) {
  locDat = as.data.table(scholar::get_citation_history(inResID))
  locDat[,
         cites_cumul := cumsum(cites)]
  return(locDat)
}

# Get researcher's publication list
fGetResPubList <- function(inResID) {
  return(scholar::get_publications(inResID))
}

# Get citation history of an article
fGetArtCitHist <- function(inResID, inPubID) {
  # Use local implementation instead of:
  # scholar::get_article_cite_history(inResID, inPubID)
  # As per: https://github.com/jkeirstead/scholar/issues/102

  library(magrittr)

  site <- getOption("scholar_site")
  inResID <- scholar::tidy_id(inResID)
  url_base <- paste0(site, "/citations?",
                     "view_op=view_citation&hl=en&citation_for_view=")
  url_tail <- paste(inResID, inPubID, sep=":")
  url <- paste0(url_base, url_tail)

  res <- scholar::get_scholar_resp(url)
  httr::stop_for_status(res, "get user id / article information")
  doc <- rvest::read_html(res)

  ## Inspect the bar chart to retrieve the citation values and years
  years <- doc %>%
    rvest::html_nodes(".gsc_oci_g_a") %>%
    rvest::html_attr('href') %>%
    stringr::str_match("as_ylo=(\\d{4})&") %>%
    "["(,2) %>%
    as.numeric()

  vals <- doc %>%
    rvest::html_nodes(".gsc_oci_g_al") %>%
    rvest::html_text() %>%
    as.numeric()

  # MD_ADD: Get the title of the paper
  title <- doc %>%
    rvest::html_nodes(xpath = '//meta[@property="og:title"]') %>%
    rvest::html_attr('content') %>%
    stringr::word(1, 5)

  if (length(years) < 1)
    df <- data.frame(year = NaN, cites = 0, title = title)
  else
    df <- data.frame(year = years, cites = vals, title = title)

  if(nrow(df)>0) {
    ## There may be undefined years in the sequence so fill in these gaps
    # dftmp <- data.frame(year=min(years):max(years))
    #
    # df <- merge(dftmp,
    #             df, all.x=TRUE)
    #
    # df[is.na(df)] <- 0

    df$pubID <- inPubID
  } else {
    # complete the 0 row data.frame to be consistent with normal results
    df$pubID <- vector(mode = mode(inPubID))
  }

  # cat("\n\ndf\n")
  # print(df)

  return(df)
}

# Get researcher's name for a GoogleScholar DI
fGetResName <- function(inResID) {
  return(scholar::get_profile(inResID)$name)
}

# Prepare top N citations for Researcher ID and a publication list
fPrepareTopNcitHist <- function(inResID, inPubList, inN) {
  cat('fPrepareTopNcitHist\n')

  locACH <- lapply(1:inN,
                   function(x) fGetArtCitHist(inResID, inPubList$pubid[x]))

  locDACH <- data.table::rbindlist(locACH)

  locDACH[,
          `:=`(cites_cumul = cumsum(cites),
               year = year - first(year)),
          by = pubID]

  return(locDACH)
}

# Link to Google Docs Spreadsheet with a list of group's publications
pubCollPertzLab = 'https://docs.google.com/spreadsheets/d/1Wld0tqo4v3rRVyseMmr9Unv_46iXvu-hN7z83l4-La8/pub?output=csv'

# Get data from Google Docs Spreadsheet
fGetDataFromGD <- function(inURL) {
  googlesheets4::gs4_deauth()
  locDat <- as.data.table(googlesheets4::read_sheet(inURL, sheet = 1))

  locDat[,
         `:=`(resID = gsub('.*citation_for_view=(.*):(.*)$', '\\1', GoogleScholarLink),
              pubID = gsub('.*citation_for_view=(.*):(.*)$', '\\2', GoogleScholarLink))]

  locDat[,
         GoogleScholarLink := NULL]
}

fGetArtCollCitHist <- function(inDat) {
  locTmp = inDat[,
                 fGetArtCitHist(resID, pubID),
                 by = seq_len(nrow(inDat))]

  locTmp = merge(locTmp,
                 inDat,
                 by = "pubID")

  return(locTmp)
}
