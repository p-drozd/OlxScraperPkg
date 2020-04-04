
#' Title
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
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




#' get_offers scraps flat renting offers posted on OLX.pl
#'
#' It starts at given url, determines how many sites with links to offers are
#' ther, scraps links to offers and than by crawling from link to link
#' downloads date about offers.
#'
#' Some links points to offers on sites other than OLX. Those links are ommited.
#'
#' @param url_start starting url, default one is for site with offers from
#' Wrocław
#'
#' @return A tibble containing data about offers
#' @export
#'
#' @examples
get_offers <- function(links = NA){
  if(is.na(links)){links <- extract_olx_links()}
  offers <- tibble::tibble()
  message('Extracting offers:/n')
  p <- dplyr::progress_estimated(n = length(links))
  for( link in links){
    tmp_offer <- scrap_offer(link)
    offers <- dplyr::bind_rows(offers, tmp_offer)
    p$tick()$print()
  }
  return(offers)
}






