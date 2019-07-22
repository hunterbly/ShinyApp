get_data <- function(){

  ## Get sample data when no db conection

  df = data.table::fread("./data/stock.csv")

  return(df)
}
