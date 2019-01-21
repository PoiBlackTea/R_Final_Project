# install.packages("rlist")
myfun <- function(){
  # ----------------------------------
  Log_file <<- vector(mode = "list", length = 3)
  names(Log_file) <<- c("日期", "stat", "爬蟲次數")
  # ----------------------------------
  URL <- scan(file = "URL.txt", what = "", n = -1, sep = "", quiet = TRUE)
  url_date <- URL[1]
  url_stock <- URL[2]
  # ---------------------------------------
  # 最早只能20120507 ,2015前格式不同 transaction_date <- sprintf("2015%s01", "01")
  # install.packages("rlist"),  ,  "2018"
  transaction_date <- list()
  for(i in c("2015", "2016", "2017")){
    for(j in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")){
      transaction_date <- list.append(transaction_date, sprintf("%s%s01", i, j))
    }
  }
  stock <<- ldply(transaction_date, .fun = reptile, url_stock, url_date, .id = NULL)
  save.image("2330_2015-2017Raw_data.RData")
}

reptile <- function(transaction_date, url_stock, url_date){
  url_stock <- gsub("20181207", transaction_date, url_stock)
  
  # ---------------------------------------
  html_data <- fromJSON(url_stock) %>% .[['data']] %>% as.data.frame
  # "fields":["日期","成交股數","成交金額","開盤價","最高價","最低價","收盤價","漲跌價差","成交筆數"]
  
  # ---------------------------------------
  # 2017/12/15
  stock_three <- adply(html_data[,1, drop = FALSE], .margins = 1, .fun = catch, url_date, .id = NULL)
  colnames(stock_three) <- c("V1", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19")
  # colnames(stock_three) <- c("日期", "證券代號", "證券名稱", "外陸資買進股數(不含外資自營商)", "外陸資賣出股數(不含外資自營商)", "外陸資買賣超股數(不含外資自營商)", "外資自營商買進股數", "外資自營商賣出股數", "外資自營商買賣超股數", "投信買進股數", "投信賣出股數", "投信買賣超股數", "自營商買賣超股數", "自營商買進股數(自行買賣)", "自營商賣出股數(自行買賣)", "自營商買賣超股數(自行買賣)", "自營商買進股數(避險)", "自營商賣出股數(避險)", "自營商買賣超股數(避險)", "三大法人買賣超股數")
  # --------------------------------------
  clear <- 0 - as.numeric(gsub(",", "", as.character(stock_three$X19)))
  return(data.frame("日期" = stock_three$V1, "證券代號" = stock_three$X1, "證券名稱" = stock_three$X2, "外陸資買進股數(不含外資自營商)" = stock_three$X3, "外陸資賣出股數(不含外資自營商)" = stock_three$X4, "外陸資買賣超股數(不含外資自營商)" = stock_three$X5, "外資自營商買進股數" = stock_three$X6, "外資自營商賣出股數" = stock_three$X7, "外資自營商買賣超股數" = stock_three$X8, "投信買進股數" = stock_three$X9, "投信賣出股數" = stock_three$X10, "投信買賣超股數" = stock_three$X11, "自營商買賣超股數" = stock_three$X12, "自營商買進股數(自行買賣)" = stock_three$X13, "自營商賣出股數(自行買賣)" = stock_three$X14, "自營商買賣超股數(自行買賣)" = stock_three$X15, "自營商買進股數(避險)" = stock_three$X16, "自營商賣出股數(避險)" = stock_three$X17, "自營商買賣超股數(避險)" = stock_three$X18, "三大法人買賣超股數" = stock_three$X19, "自然人買賣超" = clear[1], "成交股數" = html_data$V2, "開盤價" = html_data$V4, "最高價" = html_data$V5, "最低價" = html_data$V6,"收盤價" = html_data$V7,"成交筆數" = html_data$V9))
}
catch <- function(transaction_date, url_date){
  count_limit <- 1
  transaction_date <- transaction_date[1,1] %>% (function(x) paste(as.character(1911 + as.numeric(substring(x, 1, 3))), substring(x, 5, 6), substring(x, 8, 9), sep = ""))
  url_date <- gsub("20181207", transaction_date, url_date)
  Log_file$'日期' <<- list.append(Log_file$'日期', transaction_date)
  while (TRUE){
    Sys.sleep(7)
    out <- tryCatch(
      {
        html_data_date <- url_date %>% fromJSON
        if (length(html_data_date) == 1){
          print(transaction_date)
          if(html_data_date$'stat' == "很抱歉，沒有符合條件的資料!" || count_limit > 9){
            tmp <- NA
            Log_file$'stat' <<- list.append(Log_file$'stat', "很抱歉，沒有符合條件的資料!")
            Log_file$'爬蟲次數' <<- list.append(Log_file$'爬蟲次數', count_limit)
            break
          }
          count_limit <- count_limit + 1
          print(html_data_date)
          cat("-------------\n")
          next
        }
        stock_date <- html_data_date %$% data %>% data.frame(.)
        tmp <- stock_date %>% filter(X1 == '2330')
        if(ncol(tmp) == 16){
          tmp[,c(9:19)] <- tmp[,c(6:16)]
          tmp[,c(6:8)] <- "0"
        }
        colnames(tmp) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19")
        Log_file$'stat' <<- list.append(Log_file$'stat', "accept!")
        Log_file$'爬蟲次數' <<- list.append(Log_file$'爬蟲次數', count_limit)
        break
        },
      error = function(con){
        if(count_limit > 9){
          tmp <- NA
          Log_file$'stat' <<- list.append(Log_file$'stat', con)
          Log_file$'爬蟲次數' <<- list.append(Log_file$'爬蟲次數', count_limit)
          break
        }
        print(transaction_date)
        count_limit <- count_limit + 1
        print(con)
        cat("------------error\n")
      }
    )
  }
  return (data.frame(tmp))
}
