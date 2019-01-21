# install.packages("rlist")
myfun <- function(){
  # ----------------------------------
  Log_file <<- vector(mode = "list", length = 3)
  names(Log_file) <<- c("���", "stat", "���Φ���")
  # ----------------------------------
  URL <- scan(file = "URL.txt", what = "", n = -1, sep = "", quiet = TRUE)
  url_date <- URL[1]
  url_stock <- URL[2]
  # ---------------------------------------
  # �̦��u��20120507 ,2015�e�榡���P transaction_date <- sprintf("2015%s01", "01")
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
  # "fields":["���","����Ѽ�","������B","�}�L��","�̰���","�̧C��","���L��","���^���t","���浧��"]
  
  # ---------------------------------------
  # 2017/12/15
  stock_three <- adply(html_data[,1, drop = FALSE], .margins = 1, .fun = catch, url_date, .id = NULL)
  colnames(stock_three) <- c("V1", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19")
  # colnames(stock_three) <- c("���", "�Ҩ�N��", "�Ҩ�W��", "�~����R�i�Ѽ�(���t�~������)", "�~�����X�Ѽ�(���t�~������)", "�~����R��W�Ѽ�(���t�~������)", "�~�����ӶR�i�Ѽ�", "�~�����ӽ�X�Ѽ�", "�~�����ӶR��W�Ѽ�", "��H�R�i�Ѽ�", "��H��X�Ѽ�", "��H�R��W�Ѽ�", "����ӶR��W�Ѽ�", "����ӶR�i�Ѽ�(�ۦ�R��)", "����ӽ�X�Ѽ�(�ۦ�R��)", "����ӶR��W�Ѽ�(�ۦ�R��)", "����ӶR�i�Ѽ�(���I)", "����ӽ�X�Ѽ�(���I)", "����ӶR��W�Ѽ�(���I)", "�T�j�k�H�R��W�Ѽ�")
  # --------------------------------------
  clear <- 0 - as.numeric(gsub(",", "", as.character(stock_three$X19)))
  return(data.frame("���" = stock_three$V1, "�Ҩ�N��" = stock_three$X1, "�Ҩ�W��" = stock_three$X2, "�~����R�i�Ѽ�(���t�~������)" = stock_three$X3, "�~�����X�Ѽ�(���t�~������)" = stock_three$X4, "�~����R��W�Ѽ�(���t�~������)" = stock_three$X5, "�~�����ӶR�i�Ѽ�" = stock_three$X6, "�~�����ӽ�X�Ѽ�" = stock_three$X7, "�~�����ӶR��W�Ѽ�" = stock_three$X8, "��H�R�i�Ѽ�" = stock_three$X9, "��H��X�Ѽ�" = stock_three$X10, "��H�R��W�Ѽ�" = stock_three$X11, "����ӶR��W�Ѽ�" = stock_three$X12, "����ӶR�i�Ѽ�(�ۦ�R��)" = stock_three$X13, "����ӽ�X�Ѽ�(�ۦ�R��)" = stock_three$X14, "����ӶR��W�Ѽ�(�ۦ�R��)" = stock_three$X15, "����ӶR�i�Ѽ�(���I)" = stock_three$X16, "����ӽ�X�Ѽ�(���I)" = stock_three$X17, "����ӶR��W�Ѽ�(���I)" = stock_three$X18, "�T�j�k�H�R��W�Ѽ�" = stock_three$X19, "�۵M�H�R��W" = clear[1], "����Ѽ�" = html_data$V2, "�}�L��" = html_data$V4, "�̰���" = html_data$V5, "�̧C��" = html_data$V6,"���L��" = html_data$V7,"���浧��" = html_data$V9))
}
catch <- function(transaction_date, url_date){
  count_limit <- 1
  transaction_date <- transaction_date[1,1] %>% (function(x) paste(as.character(1911 + as.numeric(substring(x, 1, 3))), substring(x, 5, 6), substring(x, 8, 9), sep = ""))
  url_date <- gsub("20181207", transaction_date, url_date)
  Log_file$'���' <<- list.append(Log_file$'���', transaction_date)
  while (TRUE){
    Sys.sleep(7)
    out <- tryCatch(
      {
        html_data_date <- url_date %>% fromJSON
        if (length(html_data_date) == 1){
          print(transaction_date)
          if(html_data_date$'stat' == "�ܩ�p�A�S���ŦX���󪺸��!" || count_limit > 9){
            tmp <- NA
            Log_file$'stat' <<- list.append(Log_file$'stat', "�ܩ�p�A�S���ŦX���󪺸��!")
            Log_file$'���Φ���' <<- list.append(Log_file$'���Φ���', count_limit)
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
        Log_file$'���Φ���' <<- list.append(Log_file$'���Φ���', count_limit)
        break
        },
      error = function(con){
        if(count_limit > 9){
          tmp <- NA
          Log_file$'stat' <<- list.append(Log_file$'stat', con)
          Log_file$'���Φ���' <<- list.append(Log_file$'���Φ���', count_limit)
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