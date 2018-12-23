# install.packages("cowplot");install.packages("jsonlite");install.packages("purrr");install.packages("dplyr");install.packages("curl")
myfun <- function(){
  # 讀檔，裡面是URL，不讓人看到多餘資訊
  URL <- scan(file = "URL.txt", what = "", n = -1, sep = "", quiet = TRUE)
  # 輸入交易日期並取代網址日期部分
  cat("請輸入日期，eg:20181220\n")
  transaction_date <- scan("", what = "", n = 1, sep = "", quiet = TRUE)
  URL <- gsub("20181207", transaction_date, URL)
  url_date <- URL[1]
  url_stock<- URL[2]
  
  # 爬網路資料
  html_data <- url_date %>% fromJSON
  
  # 把想要的資料放進data.frame,並命名
  stock_date <- html_data %$% data %>% data.frame(.)
  # colnames(stock_date) <- c("證券代號", "證券名稱", "外陸資買進股數(不含外資自營商)", "外陸資賣出股數(不含外資自營商)", "外陸資買賣超股數(不含外資自營商)", "外資自營商買進股數", "外資自營商賣出股數", "外資自營商買賣超股數", "投信買進股數", "投信賣出股數", "投信買賣超股數", "自營商買賣超股數", "自營商買進股數(自行買賣)", "自營商賣出股數(自行買賣)", "自營商買賣超股數(自行買賣)", "自營商買進股數(避險)", "自營商賣出股數(避險)", "自營商買賣超股數(避險)", "三大法人買賣超股數")
  rownames(stock_date) <- NULL
  
  # 單日三大法人綜合買超前十
  stock_date_top10 <- stock_date %>% .[order(-as.numeric(gsub(",", "", .$X19))), ] %>% .[1:10, , drop = TRUE]
  
  # 拿這十個股票名字去爬資料(成交量)回來
  URL_vector <- stock_date_top10[, "X1"] %>% map_chr(., function(x) gsub("2330", x, url_stock))
  stock_volume <- aaply(URL_vector, .margins = 1, .fun = get_stock_volume, transaction_date)
  
  # 結合成一個新的data.frame
  stock_date_top10 <- data.frame(stock_date_top10, X20 = stock_volume)
  rownames(stock_date_top10) <- NULL
  
  # 畫圖輸出成PDF
  NULL_collect <- stock_date_top10 %>% map(as.vector) %>% pmap(show_pdf, transaction_date)
}

show_pdf <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, transaction_date){
  # 去除,後建成vector,買量(張數)
  stock_buy <- c(as.numeric(gsub(",", "", X3)) + as.numeric(gsub(",", "", X6)), as.numeric(gsub(",", "", X9)), as.numeric(gsub(",", "", X13)) + as.numeric(gsub(",", "", X16)))
  stock_buy <- c(stock_buy, as.numeric(gsub(",", "", X20)) - sum(stock_buy)) %>% as.character %>% map_int(., stock_count)
  stock_buy_percent <- round(stock_buy / sum(stock_buy) *100, 1)
  # 去除,後建成vector,賣量(張數)
  stock_sell <- c(as.numeric(gsub(",", "", X4)) + as.numeric(gsub(",", "", X7)), as.numeric(gsub(",", "", X10)), as.numeric(gsub(",", "", X14)) + as.numeric(gsub(",", "", X17)))
  stock_sell <- c(stock_sell, as.numeric(gsub(",", "", X20)) - sum(stock_sell)) %>% as.character %>% map_int(., stock_count)
  stock_sell_percent <- round(stock_sell / sum(stock_sell) *100, 1)
  
  # 外資,投信,內資，散戶
  stock_distribution <- c("Foreign Investment Institution", "Domestic Institution", "Dealer", "retail investors")
  stock_distribution_buy <- paste(stock_distribution, stock_buy_percent) %>% paste(., "%", sep = "")
  stock_distribution_sell <- paste(stock_distribution, stock_sell_percent) %>% paste(., "%", sep = "")
  Pie_chart_df_buy <- data.frame(People = stock_distribution_buy, stock = stock_buy)
  Pie_chart_df_sell <- data.frame(People = stock_distribution_sell, stock = stock_sell)
  
  # 去空白結合成檔名
  file_name <- paste(transaction_date, "_", gsub(" ", "", X1), gsub(" ", "", X2), ".pdf", sep = "")
  
  # theme_void()隱藏坐標軸
  P1 <- ggplot(data = Pie_chart_df_buy) + geom_bar(aes(x = factor(1), y = stock, fill = People), stat = ("identity")) + coord_polar("y", start = 0) + theme_void() + ggtitle('Buy')
  P2 <- ggplot(data = Pie_chart_df_sell) + geom_bar(aes(x = factor(1), y = stock, fill = People), stat = ("identity")) + coord_polar("y", start = 0) + theme_void() + ggtitle('Sell')
  
  #install cowplot為了多圖ggplot放一張輸出
  plot_grid(P1, P2)
  
  # ggsave是ggplot2內建的檔案輸出
  ggsave(file = file_name, width = 10, height = 10)
}
# 我只要張不要股，ex: XXXX,XXX(只要前四位)
stock_count <- function(stock_element){
  stock_buy_nchar <- nchar(stock_element)
  if (stock_buy_nchar > 3){
    stock_num <- substring(stock_element, 1, stock_buy_nchar-3)
  }
  else{
    stock_num <- "0"
  }
  return(as.integer(stock_num))
}

get_stock_volume <- function(URL_stock, transaction_date){
  
  # 封包一次丟太多，每次都被證交所ban，所以延遲10秒，怕.jpg
  Sys.sleep(10)
  html_data <- fromJSON(URL_stock)
  # "fields":["日期","成交股數","成交金額","開盤價","最高價","最低價","收盤價","漲跌價差","成交筆數"]
  transaction_date <- paste("107/", substring(transaction_date, 5, 6), "/", substring(transaction_date, 7, 8), sep = "")
  # 抓出這筆資料
  stock_volume <- html_data[["data"]] %>% grep(transaction_date, .) %>% html_data[["data"]][., ] %>% .[2] 
  return(c(stock_volume))
}
