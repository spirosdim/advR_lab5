#' A class to collect data from kolada
#' 
#' @import httr
#' @import jsonlite
#' @import methods
#' @export kolada
#' @exportClass kolada
kolada <- setRefClass("kolada",
                      field = list(),
                      methods = list(
                        get_municipality_list = function(){
                          'Get the complete municipality list'
                          path_get_mu <- "http://api.kolada.se/v2/municipality"
                          api_raw_ret <- httr::GET(url = path_get_mu)
                          api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                          municipality_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                          municipality_list <- municipality_list_raw[,c(2,3)]
                          colnames(municipality_list) = c('id','municipality')
                          return(municipality_list)
                        },
                        get_id = function(Mname){
                          'Get the id of a municipality'
                          mulist <- get_municipality_list()$municipality
                          if (!(Mname %in% mulist)) stop("municipality name is wrong.")
                          mlist <- get_municipality_list()
                          id <- mlist$id[mlist$municipality==Mname]
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
                        get_numb = function(Mname, op){
                          'Get the statistics'
                          #1 dead
                          #2 moving net
                          #3 born / 1000
                          #4 born numb
                          mulist <- get_municipality_list()$municipality
                          if (!(op %in% c(1:4))) stop("not one of the provided options")
                          if (!(Mname %in% mulist)) stop("municipality name is wrong.")
                          
                          start_path <- "http://api.kolada.se/v2/data/kpi"
                          if (op==1){
                            n<-"N01805"
                            uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                            api_raw_ret <- httr::GET(url = uurl)
                            api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                            polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                            
                            vvec <- rep(0, length(polulation_list_raw$values.values))
                            for (i in 1:length(polulation_list_raw$values.values)){
                              vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                            }
                            
                            df <- data.frame("period"=polulation_list_raw$values.period)
                            df$values <- vvec 
                            
                          }else if (op==2){
                            n<-"N01800"
                            uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                            api_raw_ret <- httr::GET(url = uurl)
                            api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                            polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                            
                            vvec <- rep(0, length(polulation_list_raw$values.values))
                            for (i in 1:length(polulation_list_raw$values.values)){
                              vvec[i] <- polulation_list_raw$values.values[[i]][[4]][[3]]
                            }
                            df <- data.frame("period"=polulation_list_raw$values.period)
                            df$values <- vvec 
                            
                            
                          }else if (op==3){
                            n<-"N02951"
                            uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                            api_raw_ret <- httr::GET(url = uurl)
                            api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                            polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                            
                            vvec <- rep(0, length(polulation_list_raw$values.values))
                            for (i in 1:length(polulation_list_raw$values.values)){
                              vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                            }
                            df <- data.frame("period"=polulation_list_raw$values.period)
                            df$values <- vvec 
                            
                          }else {
                            n<-"N01804"
                            uurl <- paste(start_path, "/", n, "/municipality/", get_id(Mname), sep = "")
                            api_raw_ret <- httr::GET(url = uurl)
                            api_text <- httr::content(api_raw_ret, as = "text", encoding = "UTF-8")
                            polulation_list_raw <- data.frame(jsonlite::fromJSON(api_text,flatten = TRUE))
                            
                            vvec <- rep(0, length(polulation_list_raw$values.values))
                            for (i in 1:length(polulation_list_raw$values.values)){
                              vvec[i] <- polulation_list_raw$values.values[[i]][[4]]
                            }
                            df <- data.frame("period"=polulation_list_raw$values.period)
                            df$values <- vvec 
                          }
                          
                          return(df)
                        }
                        
                        
                      )
)
