#' parses JSON for  history request
#' 
parse_history = function(JSON_req, UTC=TRUE, summary=FALSE) {
  parse_entry = function(entry) {
    if(UTC) date = entry[[2]]
    else date = entry[[1]]
    entry = entry[-c(1:2)]
    
    df = data.frame(entry)
    df$date = date$pretty
    
    return(df)
  }
  
  
  lists = lapply(JSON_req, parse_entry)
  do.call(rbind, lists)
}