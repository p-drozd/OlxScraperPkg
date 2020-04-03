library(magrittr)


#' Function downloads text from given css selector
#'
#'
#' @param webpage - rvest object read_html
#' @param css_sel - css selector or xpath pointing to text that supposed to
#' be downloaded
#'
#' @return - A character.
#'
#' @examples
content_text <- function(webpage, css_sel){
  node <- rvest::html_node(webpage, css_sel)
  text <- rvest::html_text(node)
  return(text)
}

#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
if_empty <- function(obj){
  if(rlang::is_empty(obj)){
    obj <- NA
  }
  return(obj)
}


#' Title
#'
#' @param webpage
#'
#' @return
#' @export
#'
#' @examples
id <- function(webpage){
  result <- webpage %>%
    content_text('.offer-titlebox__details > em:nth-child(2) > small:nth-child(1)') %>%
    readr::parse_number()
  if(is_empty(result)){
    result <- webpage %>%
      content_text('.offer-titlebox__details > em:nth-child(2) > small:nth-child(2)') %>%
      readr::parse_number()
  }
  result <- if_empty(result)
  return(result)
}

creation_date <- function(webpage){
  result <- webpage %>%
    content_text('.offer-titlebox__details > em:nth-child(2)') %>%
    stringr::str_extract('[:digit:]{1,2}\\s[:alpha:]+\\s[:digit:]{4}') %>%
    readr::parse_date('%d %B %Y', locale = locale('pl'))
  if(is_empty(result)){result <- NA}
  return(result)
}

name <- function(webpage){
  result <- webpage %>%
    content_text('.offer-titlebox > h1:nth-child(1)') %>%
    stringr::str_replace_all('[:space:]', ' ') %>%
    stringr::str_trim()
  if(is_empty(result)){result <- NA}
  return(result)
}

price <- function(webpage){
  result <- webpage %>%
    content_text('.price-label > strong:nth-child(1)') %>%
    stringr::str_replace('[:space:]', '') %>%
    stringr::str_extract('[:digit:]+') %>%
    as.numeric()
  if(is_empty(result)){result <- NA}
  return(result)
}

district <- function(webpage){
  result <- webpage %>%
    content_text('a.show-map-link:nth-child(1) > strong:nth-child(1)') %>%
    stringr::str_split(',') %>%
    { .[[1]][3] } %>%
    stringr::str_trim()
  if(is_empty(result)){result <- NA}
  return(result)
}

discription <- function(webpage){
  result <- webpage %>%
    content_text('#textContent') %>%
    stringr::str_replace_all('[:space:]', ' ') %>%
    stringr::str_trim()
  if(is_empty(result)){result <- NA}
  return(result)
}

user <- function(webpage){
  result <- webpage %>%
    content_text('.offer-user__details > h4:nth-child(1)') %>%
    stringr::str_replace_all('[:space:]', ' ') %>%
    stringr::str_trim()
  if(is_empty(result)){result <- NA}
  return(result)
}

views <- function(webpage){
  result <- webpage %>%
    content_text('div.pdingtop10:nth-child(3) > strong:nth-child(1)') %>%
    as.integer()
  if(is_empty(result)){result <- NA}
  return(result)
}

extract_table <- function(webpage){
  tab <- webpage %>%
    rvest::html_nodes('.details') %>%
    rvest::html_table(header= F, fill = T) %>%
    .[[1]] %>%
    tibble::as_tibble() %>%
    dplyr::select(X1, X2) %>%
    dplyr::filter(!(stringr::str_detect(X1, '\\n\t') |
                      stringr::str_length(X1) == 0)) %>%
    dplyr::spread(X1, X2) %>%
    janitor::clean_names()

  return(tab)

}

h_rent <- function(value_PLN){
  value_PLN %>%
    stringr::str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>%
    readr::parse_double(locale = locale(decimal_mark = ','))
}

h_size <- function(size_m){
  size_m %>%
    stringr::str_extract('[:digit:]+,[:digit:]+|[:digit:]+') %>%
    readr::parse_double(locale = locale(decimal_mark = ','))
}


olx_tab <- function(webpage){
  tab <- extract_table(webpage) %>%
    mutate(czynsz_dodatkowo = h_rent(czynsz_dodatkowo),
           powierzchnia = h_size(powierzchnia),
           liczba_pokoi = parse_number(liczba_pokoi))

  result <- tibble('czynsz' = if_empty(tab$czynsz_dodatkowo),
                   'liczba_pokoi' = if_empty(tab$liczba_pokoi),
                   'oferta_od' = if_empty(tab$oferta_od),
                   'powierzchnia' = if_empty(tab$powierzchnia),
                   'poziom' = if_empty(tab$poziom),
                   'zabudowa' = if_empty(tab$rodzaj_zabudowy),
                   'meble' = if_empty(tab$umeblowane))

  return(result)
}


#' function allows to determin how many pages with offers are ther
#'
#' @param webpage
#'
#' @return
#'
#' @examples
num_of_offer_sites <- function(webpage){
  text <- content_text(webpage, '.pager')
  found_numbers <- unlist(stringr::str_extract_all(text, '[:space:][:digit:]+[:space:]') )
  max_num <- max(as.integer(found_numbers))
  return(max_num)
}

extract_olx_link <- function(url){
  webpage <- xml2::read_html(url)
  nodes <- rvest::html_nodes(webpage,
                             'tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)')
  links <- rvest::html_attr(nodes, 'href')
  olx_links <- links[stringr::str_detect(links, 'https://www.olx.pl/oferta/')]
  return(olx_links)
}

get_link_to_offers <- function(url_start = 'https://www.olx.pl/nieruchomosci/mieszkania/wynajem/wroclaw'){
  # read webpage
  webpage_start <- xml2::read_html(url_start)
  # determine number of subsites with offers
  max_site_number <- num_of_offer_sites(webpage_start)
  # empty data frame for results
  offers <- tibble()
  print(str_c('Number of subsites with offers: ', as.character(max_site_number)))

  for( site_number in 1:max_site_number){
    # url for subsite with offers
    url <- str_c(url_start, '/?page=', as.character(site_number))
    # read given url and scrap links to offers
    webpage <- url %>%
      read_html()
    links <- webpage %>%
      html_nodes('tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > div:nth-child(1) > h3:nth-child(1) > a:nth-child(1)') %>%
      html_attr('href')
    links <- links[links %>% str_detect('https://www.olx.pl/oferta/')]
    promoted <- c(promoted, links %>% str_detect('promoted'))
    # scrap offers from obtained links
    for(link in links){
      tmp <- scrap_offer(link)
      offers <- offers %>% rbind(tmp)
    }
    # update progress bar
    setTxtProgressBar(pb, site_number)
  }
  offers$wyrozniona = promoted
  # close progress bar
  close(pb)
  return(offers)
}
