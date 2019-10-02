kolada <- setRefClass("kolada",
                      filed = list(municipal="character"),
                      methods = list(
                        initialize = function(municipal){
                          municipal <<- municipal
                        },
                        get_id = function(){
                          
                        }
                        )
                      ))