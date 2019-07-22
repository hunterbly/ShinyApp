sql_connection <- function(){

  ## Return connection for remote database
  ##
  ## Arg
  ##
  ## Return
  ##  conn (PostgreSQLConnection): PSQL connection
  ##
  ## Example
  ##  conn = sql_connection
  ##  DBI::dbDisconnect(conn)


  conn <-  tryCatch(
    {
      # local
      DBI::dbConnect(drv    = RPostgreSQL::PostgreSQL(),
                     dbname = "stock",
                     host   = "localhost",
                     port   = 4004,
                     user   = "db_user",
                     password = 'P@ssw0rDB')
    },
    error = function(e){
      # Remote
      DBI::dbConnect(drv    = RPostgreSQL::PostgreSQL(),
                     dbname = "stock",
                     host   = "206.189.149.240",
                     port   = 4004,
                     user   = "db_user",
                     password = 'P@ssw0rDB')

    }
  )

  return(conn)
}


sql_query <- function(sql){

  ## Get data from the provided sql
  ##
  ## Arg
  ##  sql (str): SQL statement
  ##
  ## Return
  ##  res (Dataframe): Dataframe of the result of the query statement
  ##
  ## Example
  ##  df <- sql_query("select * from stock where date >= '2019-06-01' order by date desc")
  ##

  conn <- sql_connection()

  res <- as.data.frame(DBI::dbGetQuery(conn, sql))

  DBI::dbDisconnect(conn)

  return(res)

}

sql_write <- function(df, table, key){
  ## Write dataframe to table
  ##
  ## Arg
  ##  df (Dataframe): Dataframe to be written to table
  ##  table (str): Name of the table name
  ##  key (List[str]): List of columns to be check for duplicate records
  ##
  ## Return
  ##  res:
  ##
  ## Example
  ##
  ##

  conn <- sql_connection()

  DBI::dbWriteTable(conn,
                    table,
                    df,
                    append = TRUE,
                    row.names = FALSE)

  DBI::dbDisconnect(conn)

  return(NULL)
}


####
# Function interfaces
####



####
# Not implemented
####


get_pool <- function(){

  ## Try out

  # pool <- pool::dbPool(
  #
  #   drv = RPostgreSQL::PostgreSQL(),
  #   dbname = "stock",
  #   host = "206.189.149.240",
  #   user = "db_user",
  #   port = 4004,
  #   password = "P@ssw0rDB",
  #   maxSize = 10,
  #   idleTimeout = 120
  #
  # )
  # pool

}

