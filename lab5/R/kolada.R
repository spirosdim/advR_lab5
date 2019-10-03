#' A class to collect data from kolada
#' 
#' @import httr
#' @import jsonlite
#' @export kolada
#' @exportClass kolada
kolada <- setRefClass("kolada",
                      field = list(),
                      methods = list(
                        get_id = function(municipal){
                          'Get the id of a municipality'
                          path_mu = "http://api.kolada.se/v2/municipality?"
                          api_raw_ret <- httr::GET(url = path_mu,
                                                   query = list(title=municipal))
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          id_json <- jsonlite::fromJSON(api_text,flatten = TRUE)
                          id <- id_json$values$id
                          return(id)
                        },
                        get_skola = function(municipal){
                          'Get the school list in a municipality'
                          path_sk <- "http://api.kolada.se/v2/ou?"
                          id <- get_id(municipal)
                          api_raw_ret <- httr::GET(url = path_sk,
                                                   query = list(municipality=id,title="skola"))
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          skola_json <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                          sk_list <- skola_json['values.title']
                          colnames(sk_list) <- c('school')
                          return(sk_list)
                        },
                        get_municipality_list = function(){
                          'Get the complete municipality list'
                          path_get_mu <- "http://api.kolada.se/v2/municipality"
                          api_raw_ret <- httr::GET(url = path_get_mu)
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          municipality_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                          municipality_list <- municipality_list_raw[,c(2,3)]
                          colnames(municipality_list) = c('id','municipality')
                          return(municipality_list)
                        }
                      )
)