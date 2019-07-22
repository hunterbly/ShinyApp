
f_bull_stick <- function(open.lag0, close.lag0, upper.limit = 0.05, lower.limit = 0.04)
{
  boolean <- 	ifelse(
    ((close.lag0 - open.lag0) / open.lag0) > lower.limit &
      ((close.lag0 - open.lag0) / open.lag0) < upper.limit,
    1, 0
  )

  return(boolean)
}

f_bear_stick <- function(open.lag0, close.lag0, upper.limit = -0.05, lower.limit = -0.04)
{
  boolean <- 	ifelse(
    ((close.lag0 - open.lag0) / open.lag0) < lower.limit &
      ((close.lag0 - open.lag0) / open.lag0) > upper.limit, 1, 0
  )

  return(boolean)
}

f_bull_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{

  boolean <- 	ifelse(
    ((close.lag0 > open.lag0) &
       (open.lag1 > close.lag1) &
       (close.lag0 > open.lag1) &
       (close.lag1 > open.lag0))
    , 1, 0
  )

  return(boolean)
}

f_bear_engulf <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{

  boolean <- 	ifelse(
    (close.lag0 < open.lag0) &
      (close.lag1 > open.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag1 < open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_bull_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      (close.lag1 < open.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag1 < open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_bear_harami<- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 < open.lag0) &
      (close.lag1 > open.lag1) &
      (close.lag0 > open.lag1) &
      (close.lag1 > open.lag0)
    , 1, 0
  )

  return(boolean)
}

f_2day_reverse_good<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_up_threshold = 0.03, ytd_down_threshold = -0.03)
{
  boolean <- 	ifelse(
    (((close.lag0 - close.lag1) / close.lag1) > today_up_threshold) &
      (((close.lag1 - close.lag2) / close.lag2) < ytd_down_threshold)
    , 1, 0
  )

  return(boolean)
}

f_2day_reverse_bad<- function(open.lag0, close.lag0, open.lag1, close.lag1, open.lag2, close.lag2, today_down_threshold = -0.03, ytd_up_threshold = 0.03)
{
  boolean <- 	ifelse(
    (((close.lag0 - close.lag1) / close.lag1) < today_down_threshold) &
      (((close.lag1 - close.lag2) / close.lag2) > ytd_up_threshold)
    , 1, 0
  )

  return(boolean)
}

f_bull_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      (close.lag1 < open.lag1) &
      (open.lag0 < close.lag1) &
      (close.lag0 < open.lag1) &
      (close.lag0 >((open.lag1 + close.lag1)/2))
    , 1, 0
  )

  return(boolean)
}

f_bear_pierce <- function(open.lag0, close.lag0, open.lag1, close.lag1)
{
  boolean <- 	ifelse(
    (close.lag1 > open.lag1) &
      (open.lag0 > close.lag0) &
      (close.lag0 > open.lag1) &
      (close.lag0 < ((open.lag1 + close.lag1)/2)) &
      (open.lag0 > close.lag1)
    , 1, 0
  )

  return(boolean)
}

f_hammer <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
  boolean <- 	ifelse(
    (close.lag0 > open.lag0) &
      ((open.lag0 - low.lag0) >= (tail_multiplier * (close.lag0 - open.lag0))) &
      (((close.lag0 - open.lag0) / open.lag0) > least_body_length) &
      ((high.lag0 - close.lag0) <= (close.lag0 - open.lag0))
    , 1, 0
  )

  return(boolean)
}

f_shooting_star <- function(open.lag0, close.lag0, open.lag1, close.lag1, low.lag0, high.lag0, tail_multiplier = 2, least_body_length = 0.005)
{
  boolean <- 	ifelse(
    (open.lag0 > close.lag0) &
      ((high.lag0 - open.lag0) >= (tail_multiplier * (open.lag0 - close.lag0))) &
      (((close.lag0 - low.lag0) <= (open.lag0 - close.lag0))) &
      (((open.lag0 - close.lag0) / open.lag0) > least_body_length)
    , 1, 0
  )

  return(boolean)

}


high_return <- function(df, n) {
  df %>% arrange(date)
  varname <- paste("high.return.lead", n , sep=".")
  varval <- lazyeval::interp(~(lead(high, n) - close) / close, n=n)
  mutate_(df, .dots= setNames(list(varval), varname))
}

low_return <- function(df, n) {
  df %>% arrange(date)
  varname <- paste("low.return.lead", n , sep=".")
  varval <- lazyeval::interp(~(lead(low, n) - close) / close, n=n)
  mutate_(df, .dots= setNames(list(varval), varname))
}

eval_signal <- function(signal.list, df){

  df.return <- plyr::ldply(signal.list, cal_signal_strength, df)
  df.return <- t(df.return)
  colnames(df.return) <- signal.list
  rownames(df.return) <- NULL

  data.frame(df.return)


}


cal_signal <- function(df){

  ## Calculate whether the pre-defined signal is being hit with the corresponding return in the following days
  ##
  ## Arg
  ##  df (Dataframe): Dataframe of the stock data
  ##
  ## Return
  ##  df.singal (Dataframe): Dataframe of the stock data with flag of signal hit and the corresponding return in the following days
  ##
  ## Example
  ##  df.signal = cal_singal(df)

  if('code' %in% colnames(df)){
    data <- df %>% filter(volume != 0) %>% arrange(code, date)
  }
  else{
    data <- df %>% filter(volume != 0) %>% arrange(date)
  }

  data <- data %>% mutate(open.lag1 = lag(open, n = 1),
                          open.lag2 = lag(open, n = 2),
                          close.lag1 = lag(close, n = 1),
                          close.lag2 = lag(close, n = 2))

  data <- data %>% mutate(s_bull_stick = f_bull_stick(open, close),
                          s_bear_stick = f_bear_stick(open, close),
                          s_bull_engulf = f_bull_engulf(open, close, open.lag1, close.lag1),
                          s_bear_engulf = f_bear_engulf(open, close, open.lag1, close.lag1),
                          s_bull_harami = f_bull_harami(open, close, open.lag1, close.lag1),
                          s_bear_harami = f_bear_harami(open, close, open.lag1, close.lag1),
                          s_2day_reverse_good = f_2day_reverse_good(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
                          s_2day_reverse_bad = f_2day_reverse_bad(open, close, open.lag1, close.lag1, open.lag2, close.lag2),
                          s_bull_pierce = f_bull_pierce(open, close, open.lag1, close.lag1),
                          s_bear_pierce = f_bear_pierce(open, close, open.lag1, close.lag1),
                          s_hammer = f_hammer(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005),
                          s_shooting_star = f_shooting_star(open, close, open.lag1, close.lag1, low, high, tail_multiplier = 2, least_body_length = 0.005))


  for(i in 1:5) {
    data <- high_return(df = data, n=i)
    data <- low_return(df = data, n=i)
  }

  data %>% arrange(desc(date))

  return(data)
}


cal_signal_strength <- function(signalName, df.grouped, threshold = 0.03){

  ## Calculate whether the pre-defined signal is being hit with the corresponding return in the following days
  ##
  ## Arg
  ##  SingalName (str): Name of the pre-defined Signal name, e.g. s_bull_stick
  ##  df.grouped (Dataframe):
  ##  threshold (num): Percentage of the threshold
  ##
  ## Return
  ##  out (Dataframe):
  ##
  ## Example
  ##

  df <- df.grouped

  out <- tryCatch({
    filter_criteria <- interp(~which_column == 1, which_column = as.name(signalName))
    df.filtered <- df %>% filter_(filter_criteria)
    signal_count <- nrow(df.filtered)

    # Calculate positive return, median & count
    df.filtered.high <- df.filtered %>% select(matches('high.return'))
    df.filtered.high.threshold <- sapply(df.filtered.high, function(x){ (y <- ifelse(x >= threshold, x, NA))})

    df.filtered.high.threshold.median <- apply(df.filtered.high.threshold,2,median,na.rm=TRUE)
    df.filtered.high.threshold.count  <- apply(df.filtered.high.threshold, 2, function(c){sum(c!=0, na.rm = TRUE)/signal_count})
    df.filtered.high.threshold.return <- rbind(df.filtered.high.threshold.median, df.filtered.high.threshold.count)
    df.filtered.high.threshold.return[] <- vapply(df.filtered.high.threshold.return, function(x){ifelse(is.na(x), 0, x)}, numeric(1))

    # Calculate negative return, median & count
    df.filtered.low <- df.filtered %>% select(matches('low.return'))
    df.filtered.low.threshold <- sapply(df.filtered.low, function(x){ (y <- ifelse(x <= -1 * threshold, x, NA))})

    df.filtered.low.threshold.median <- apply(df.filtered.low.threshold,2,median,na.rm=TRUE)
    df.filtered.low.threshold.count  <- apply(df.filtered.low.threshold, 2, function(c){sum(c!=0, na.rm = TRUE)/signal_count})
    df.filtered.low.threshold.return <- rbind(df.filtered.low.threshold.median, df.filtered.low.threshold.count)
    df.filtered.low.threshold.return[] <- vapply(df.filtered.low.threshold.return, function(x){ifelse(is.na(x), 0, x)}, numeric(1))


    #Calculate Signal Strength
    number_days	= ncol(df.filtered.high.threshold.return)
    index.strength = (abs(sum(matrixStats::colProds(df.filtered.high.threshold.return))) - abs(sum(matrixStats::colProds(df.filtered.low.threshold.return)))) * 10000 / number_days

    index.strength
  },error = function(e){
    # message('Error')
    return(0)
  }
  )
  return(out)



}

get_signal_strength <- function(df){

  signal.list <- c('s_bull_stick',
                   's_bear_stick',
                   's_bull_engulf',
                   's_bear_engulf',
                   's_bull_harami',
                   's_bear_harami',
                   's_2day_reverse_good',
                   's_2day_reverse_bad',
                   's_bull_pierce',
                   's_bear_pierce',
                   's_hammer',
                   's_shooting_star')

  data.signal <- df %>% nest(-code) %>% mutate(signal = purrr::map(data, ~cal_signal(.)))

  data.signal.eval <- data.signal %>% mutate(signal.eval = purrr::map(signal, ~eval_signal(signal.list, .)))

  # get evaluated signal
  signal.result <- data.signal.eval %>% select(code, signal.eval) %>% unnest(signal.eval)
}


get_hit_signal <- function(ref.date, format = 'wide'){

  ## Get the signal for the reference date with long or wide format
  ##
  ## Args:
  ##  ref.date (str): Date in YYYY-MM-DD format, e.g. 2018-01-01
  ##  format (str): Wide or Long format of the output, e.g. c('wide', 'long')
  ##
  ## Returns:
  ##  df.signal (Dataframe): Stock price dataframe with calculated signal in the input date only
  ##
  ## Example:
  ##   get_hit_signal(ref.date = '2019-06-26')

  date.input    = lubridate::ymd(ref.date)
  date.earliest  = date.input - 20

  query = sprintf("SELECT * FROM STOCK WHERE DATE >= '%s' AND DATE <= '%s'", date.earliest, date.input)
  df.raw = sql_query(query)

  # Calculate the signal and append to the original data
  df.signal.all = cal_signal(df.raw)

  # Filter by the input date and select related column only
  df.signal.filtered = df.signal.all %>%
                        select(c('date', 'code'), starts_with('s_')) %>%    # select signal column only
                        filter(date == date.input)

  if(format == 'wide'){

    # Return wide format
    df.signal = df.signal.filtered

  }else if(format == 'long'){

    # Return wide format

    df.signal = reshape2::melt(df.signal.filtered, id.vars=c("date", "code"), na.rm = TRUE)
    colnames(df.signal) <- c('date', 'code', 'signal', 'hit')

  }else{
    stop_quietly(sprintf("Data format - %s is no supported ", format))
  }

  return(df.signal)
}

save_hit_signal <- function(df.signal){

  # Filter out non zero hit and add id column at the front being insert
  df.signal.nz = df.signal %>% filter(hit != 0)

  sql_write(df    = df.signal.nz,
            table = 'signal_history',
            key   = NULL)

  return(NULL)
}
