
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
get_offers <- function(url_start = 'https://www.olx.pl/nieruchomosci/mieszkania/wynajem/wroclaw'){
  # read webpage
  webpage_start <- url_start %>%
    read_html()
  # determine number of subsites with offers
  max_site_number <- num_of_offer_sites(webpage_start)
  # empty data frame for results
  offers <- tibble()
  print(str_c('Number of subsites with offers: ', as.character(max_site_number)))
  pb <- txtProgressBar(min = 0, max = max_site_number, style = 3)
  promoted <- logical()
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






