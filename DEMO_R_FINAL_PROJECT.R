# install.packages("cowplot");install.packages("jsonlite");install.packages("purrr");install.packages("dplyr");install.packages("curl")
myfun <- function(){
  # Ū�ɡA�̭��OURL�A�����H�ݨ�h�l��T
  URL <- scan(file = "URL.txt", what = "", n = -1, sep = "", quiet = TRUE)
  # ��J�������è��N���}�������
  cat("�п�J����Aeg:20181220\n")
  transaction_date <- scan("", what = "", n = 1, sep = "", quiet = TRUE)
  URL <- gsub("20181207", transaction_date, URL)
  url_date <- URL[1]
  url_stock<- URL[2]
  
  # ���������
  html_data <- url_date %>% fromJSON
  
  # ��Q�n����Ʃ�idata.frame,�éR�W
  stock_date <- html_data %$% data %>% data.frame(.)
  # colnames(stock_date) <- c("�Ҩ�N��", "�Ҩ�W��", "�~����R�i�Ѽ�(���t�~������)", "�~�����X�Ѽ�(���t�~������)", "�~����R��W�Ѽ�(���t�~������)", "�~�����ӶR�i�Ѽ�", "�~�����ӽ�X�Ѽ�", "�~�����ӶR��W�Ѽ�", "��H�R�i�Ѽ�", "��H��X�Ѽ�", "��H�R��W�Ѽ�", "����ӶR��W�Ѽ�", "����ӶR�i�Ѽ�(�ۦ�R��)", "����ӽ�X�Ѽ�(�ۦ�R��)", "����ӶR��W�Ѽ�(�ۦ�R��)", "����ӶR�i�Ѽ�(���I)", "����ӽ�X�Ѽ�(���I)", "����ӶR��W�Ѽ�(���I)", "�T�j�k�H�R��W�Ѽ�")
  rownames(stock_date) <- NULL
  
  # ���T�j�k�H��X�R�W�e�Q
  stock_date_top10 <- stock_date %>% .[order(-as.numeric(gsub(",", "", .$X19))), ] %>% .[1:10, , drop = TRUE]
  
  # ���o�Q�ӪѲ��W�r�h�����(����q)�^��
  URL_vector <- stock_date_top10[, "X1"] %>% map_chr(., function(x) gsub("2330", x, url_stock))
  stock_volume <- aaply(URL_vector, .margins = 1, .fun = get_stock_volume, transaction_date)
  
  # ���X���@�ӷs��data.frame
  stock_date_top10 <- data.frame(stock_date_top10, X20 = stock_volume)
  rownames(stock_date_top10) <- NULL
  
  # �e�Ͽ�X��PDF
  NULL_collect <- stock_date_top10 %>% map(as.vector) %>% pmap(show_pdf, transaction_date)
}

show_pdf <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, transaction_date){
  # �h��,��ئ�vector,�R�q(�i��)
  stock_buy <- c(as.numeric(gsub(",", "", X3)) + as.numeric(gsub(",", "", X6)), as.numeric(gsub(",", "", X9)), as.numeric(gsub(",", "", X13)) + as.numeric(gsub(",", "", X16)))
  stock_buy <- c(stock_buy, as.numeric(gsub(",", "", X20)) - sum(stock_buy)) %>% as.character %>% map_int(., stock_count)
  stock_buy_percent <- round(stock_buy / sum(stock_buy) *100, 1)
  # �h��,��ئ�vector,��q(�i��)
  stock_sell <- c(as.numeric(gsub(",", "", X4)) + as.numeric(gsub(",", "", X7)), as.numeric(gsub(",", "", X10)), as.numeric(gsub(",", "", X14)) + as.numeric(gsub(",", "", X17)))
  stock_sell <- c(stock_sell, as.numeric(gsub(",", "", X20)) - sum(stock_sell)) %>% as.character %>% map_int(., stock_count)
  stock_sell_percent <- round(stock_sell / sum(stock_sell) *100, 1)
  
  # �~��,��H,����A����
  stock_distribution <- c("Foreign Investment Institution", "Domestic Institution", "Dealer", "retail investors")
  stock_distribution_buy <- paste(stock_distribution, stock_buy_percent) %>% paste(., "%", sep = "")
  stock_distribution_sell <- paste(stock_distribution, stock_sell_percent) %>% paste(., "%", sep = "")
  Pie_chart_df_buy <- data.frame(People = stock_distribution_buy, stock = stock_buy)
  Pie_chart_df_sell <- data.frame(People = stock_distribution_sell, stock = stock_sell)
  
  # �h�ťյ��X���ɦW
  file_name <- paste(transaction_date, "_", gsub(" ", "", X1), gsub(" ", "", X2), ".pdf", sep = "")
  
  # theme_void()���ç��жb
  P1 <- ggplot(data = Pie_chart_df_buy) + geom_bar(aes(x = factor(1), y = stock, fill = People), stat = ("identity")) + coord_polar("y", start = 0) + theme_void() + ggtitle('Buy')
  P2 <- ggplot(data = Pie_chart_df_sell) + geom_bar(aes(x = factor(1), y = stock, fill = People), stat = ("identity")) + coord_polar("y", start = 0) + theme_void() + ggtitle('Sell')
  
  #install cowplot���F�h��ggplot��@�i��X
  plot_grid(P1, P2)
  
  # ggsave�Oggplot2���ت��ɮ׿�X
  ggsave(file = file_name, width = 10, height = 10)
}
# �ڥu�n�i���n�ѡAex: XXXX,XXX(�u�n�e�|��)
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
  
  # �ʥ]�@����Ӧh�A�C�����Q�ҥ��ban�A�ҥH����10���A��.jpg
  Sys.sleep(10)
  html_data <- fromJSON(URL_stock)
  # "fields":["���","����Ѽ�","������B","�}�L��","�̰���","�̧C��","���L��","���^���t","���浧��"]
  transaction_date <- paste("107/", substring(transaction_date, 5, 6), "/", substring(transaction_date, 7, 8), sep = "")
  # ��X�o�����
  stock_volume <- html_data[["data"]] %>% grep(transaction_date, .) %>% html_data[["data"]][., ] %>% .[2] 
  return(c(stock_volume))
}