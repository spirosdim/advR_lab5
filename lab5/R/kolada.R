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
                          id_json <- jsonlite::fromJSON(api_text,flatten = TRUE)
                          id <- id_json$values$id
                          return(id)
                        },
                        get_school = function(){
                          path_sk <- "http://api.kolada.se/v2/ou?"
                          id <- get_id()
                          api_raw_ret <- httr::GET(url = path_sk,
                                                   query = list(municipality=id,title="skola"))
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          skola_json <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                          sk_list <- skola_json['values.title']
                          colnames(sk_list) <- c('school')
                          return(sk_list)
                        }
                      )
)