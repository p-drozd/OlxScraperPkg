
#' Scrapes olx flat rental offer.
#'
#' @description Scrapes olx flat rental offer and returns tibble containing alee
#'   information about offer. If some data is missing NA is imputed. Works for
#'   polish version of olx.
#'
#' @param url url to olx offer
#'
#' @return tibble with data about specific offer
#' @export
scrap_offer <- function(url){
  webpage <- xml2::read_html(url)

  id <- id(webpage)

  data_utworzenia <- creation_date(webpage)

  nazwa <- name(webpage)

  cena <- price(webpage)

  dzielnica <- district(webpage)

  opis <- discription(webpage)

  uzytkownik <- user(webpage)

  wyswietlenia <- views(webpage)

  result <- tibble::tibble(id ,
                   data_utworzenia ,
                   nazwa ,
                   cena ,
                   dzielnica ,
                   opis ,
                   uzytkownik ,
                   wyswietlenia ,
                   promowana = stringr::str_detect(url, 'promoted'))

  tmp <- olx_tab(webpage)
  result <- cbind(result, tmp)

  return(result)
}




#' Scraps flat renting offers posted on OLX.pl available at given moment.
#'
#' @description  If links = NA then function scraps links to offers and than by
#'   crawling from link to link downloads date about offers. Some links points
#'   to offers on sites other than OLX. Those links are ommited.
#'
#' @param links character vector contatining links to offers
#'
#'
#' @return A tibble containing data about offers
#' @export
get_offers <- function(links = NA){
  if(is.na(links)){links <- extract_olx_links()}
  offers <- tibble::tibble()
  cat('\nExtracting offers:\n')
  p <- dplyr::progress_estimated(n = length(links))
  for( link in links){
    tmp_offer <- scrap_offer(link)
    offers <- dplyr::bind_rows(offers, tmp_offer)
    p$tick()$print()
  }
  return(offers)
}






