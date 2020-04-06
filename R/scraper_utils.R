#' Extracts text from html object given css selector
#'
#'
#' @param webpage xml2 object read_html
#' @param css_sel css selector or xpath pointing to text that supposed to
#' be downloaded
#'
#' @return  A character.
content_text <- function(webpage, css_sel){
  node <- rvest::html_node(webpage, css_sel)
  text <- rvest::html_text(node)
  return(text)
}

#' Determines whether object is empty, if so assigns NA to it.
#'
#' @param obj atomic vector, list, matrix or data frame
#'
#' @return passed object or NA if object is empty
if_empty <- function(obj){
  if(rlang::is_empty(obj)){
    obj <- NA
  }
  return(obj)
}


#' Extracts id from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return integer
id <- function(webpage){
  text <- content_text(webpage,
                         '.offer-titlebox__details > em:nth-child(2) > small:nth-child(1)')
  result <- readr::parse_number(text)
  if(is_empty(result)){
    text <- content_text(webpage,
                         '.offer-titlebox__details > em:nth-child(2) > small:nth-child(2)')
    result <- readr::parse_number(text)
  }
  if(is_empty(result)){
    text <- content_text(webpage,
                         '.offer-titlebox__details > em:nth-child(2) > small:nth-child(3)')
    result <- readr::parse_number(text)
  }
  result <- if_empty(result)
  return(result)
}

#' Extracts creation date from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return date
creation_date <- function(webpage){
  text <- content_text(webpage, '.offer-titlebox__details > em:nth-child(2)')
  text <- stringr::str_extract(text, '[:digit:]{1,2}\\s[:alpha:]+\\s[:digit:]{4}')
  result <- readr::parse_date(text, '%d %B %Y', locale = readr::locale('pl'))
  result <- if_empty(result)
  return(result)
}


#' Extracts name of oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return character
name <- function(webpage){
  text <- content_text(webpage, '.offer-titlebox > h1:nth-child(1)')
  text <- stringr::str_replace_all(text, '[:space:]', ' ')
  result <- stringr::str_trim(text)
  result <- if_empty(result)
  return(result)
}


#' Extracts price from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return double
price <- function(webpage){
  text <- content_text(webpage, '.price-label > strong:nth-child(1)')
  text <- stringr::str_replace(text, '[:space:]', '')
  text <- stringr::str_extract(text, '[:digit:]+')
  result <- as.numeric(text)
  result <- if_empty(result)
  return(result)
}


#' Extracts name of district from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return character
district <- function(webpage){
  text <- content_text(webpage, 'a.show-map-link:nth-child(1) > strong:nth-child(1)')
  text <- stringr::str_split(text, ',')
  result <- stringr::str_trim(text[[1]][3])
  result <- if_empty(result)
  return(result)
}


#' Extracts discription from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return character
discription <- function(webpage){
  text <- content_text(webpage, '#textContent')
  text <- stringr::str_replace_all(text, '[:space:]', ' ')
  result <- stringr::str_trim(text)
  result <- if_empty(result)
  return(result)
}


#' Extracts user name from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return character
user <- function(webpage){
  text <-content_text(webpage, '.offer-user__details > h4:nth-child(1)')
  text <- stringr::str_replace_all(text, '[:space:]', ' ')
  result <- stringr::str_trim(text)
  result <- if_empty(result)
  return(result)
}


#' Extracts number of views from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return integer
views <- function(webpage){
  text <- content_text(webpage,
                       'div.pdingtop10:nth-child(3) > strong:nth-child(1)')
  result <- as.integer(text)
  result <- if_empty(result)
  return(result)
}


#' Extracts table with detail from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return tibble
extract_table <- function(webpage){
  tab <- rvest::html_nodes(webpage, '.details')
  tab <- rvest::html_table(tab, header= F, fill = T)[[1]]
  tab <- tibble::as_tibble(tab)
  tab <- dplyr::select(tab, X1, X2)
  tab <- dplyr::filter(tab, !(stringr::str_detect(X1, '\\n\t') |
                           stringr::str_length(X1) == 0))
  tab <- tidyr::spread(tab, X1, X2)
  tab <- janitor::clean_names(tab)

  return(tab)

}


#' Parses value of rent extracted from table with deatils
#'
#' @param value_PLN character
#'
#' @return double
h_rent <- function(value_PLN){
  rent <- stringr::str_extract(value_PLN,
                               '[:digit:]+,[:digit:]+|[:digit:]+')
  rent <- readr::parse_double(rent, locale = readr::locale(decimal_mark = ','))
  return(rent)
}


#' Parses size of flat extracted from table with deatils
#'
#' @param size_m character
#'
#' @return double
h_size <- function(size_m){
  size <- stringr::str_extract(size_m, '[:digit:]+,[:digit:]+|[:digit:]+')
  size <- readr::parse_double(size, locale = readr::locale(decimal_mark = ','))
  return(size)
}

#' Extracts and cleans table with detail from oxl flat rental offer
#'
#' @param webpage xml2 object read_html
#'
#' @return tibble
olx_tab <- function(webpage){
  tab <- extract_table(webpage)
  tab <- dplyr::mutate(tab,
                       czynsz_dodatkowo = h_rent(czynsz_dodatkowo),
                       powierzchnia = h_size(powierzchnia),
                       liczba_pokoi = stringr::str_replace(liczba_pokoi, 'Kawalerka', '1'),
                       liczba_pokoi = readr::parse_number(liczba_pokoi))


  result <- tibble::tibble('czynsz' = if_empty(tab$czynsz_dodatkowo),
                   'liczba_pokoi' = if_empty(tab$liczba_pokoi),
                   'oferta_od' = if_empty(tab$oferta_od),
                   'powierzchnia' = if_empty(tab$powierzchnia),
                   'poziom' = if_empty(tab$poziom),
                   'zabudowa' = if_empty(tab$rodzaj_zabudowy),
                   'meble' = if_empty(tab$umeblowane))

  return(result)
}


#' Determins how many subpages with offers are there
#'
#' @param webpage xml2 object read_html
#'
#' @return integer
num_of_offer_sites <- function(webpage){
  text <- content_text(webpage, '.pager')
  found_numbers <- unlist(stringr::str_extract_all(text, '[:space:][:digit:]+[:space:]') )
  max_num <- max(as.integer(found_numbers))
  return(max_num)
}

# Extracts links to olx offers from all subpages
extract_olx_links <- function(){
  url_start = 'https://www.olx.pl/nieruchomosci/mieszkania/wynajem/wroclaw'
  webpage_start <- xml2::read_html(url_start)
  # determine number of subsites with offers
  max_site_number <- num_of_offer_sites(webpage_start)
  # empty data frame for results
  links <- character()
  cat('Extracting links to offers:\n')
  p <- dplyr::progress_estimated(n = max_site_number)
  for( site_number in 1:max_site_number){
    tmp_url <- paste0(url_start, '/?page=', as.character(site_number))
    tmp_webpage <- xml2::read_html(tmp_url)
    tmp_nodes <- rvest::html_nodes(tmp_webpage,
                                    'tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)')
    tmp_links <- rvest::html_attr(tmp_nodes, 'href')
    tmp_links <- tmp_links[stringr::str_detect(tmp_links,
                                               'https://www.olx.pl/oferta/')]
    links <- c(links, tmp_links)
    p$tick()$print()
  }
  links <- unique(links)
  return(links)
}


