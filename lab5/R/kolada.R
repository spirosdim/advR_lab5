kolada <- setRefClass("kolada",
                      field = list(municipal="character"),
                      methods = list(
                        initialize = function(municipal){
                          municipal <<- municipal
                        },
                        get_id = function(){
                          path_mu = "http://api.kolada.se/v2/municipality?"
                          api_raw_ret <- httr::GET(url = path_mu,
                                                   query = list(title=municipal))
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          id_json <- ef <- jsonlite::fromJSON(api_text,flatten = TRUE)
                          id <- id_json$values$id
                          return(id)
                        }
                      )
)