# install.packages("reshape2");install.packages("coefplot");
myfun <- function(){
  
  RData <- dir(pattern = "Raw_data.RData")
  year15_18 <- RData %>% map_df(stock_bind)
  #去NA
  year15_18 <- year15_18[complete.cases(year15_18),] 
  year15_18 %<>% map_df((function(x)x %>% as.character %>% gsub(",", "", .))) %>% as.data.frame(stringsAsFactors = FALSE)
  year15_18[,4:22]  %<>% map_df((function(x) x %>% as.numeric/1000))
  year15_18$x28 <- c(year15_18[-1,23], "226.50")
  year15_18[,23:28] %<>% map_df((function(x) x %>% as.numeric)) %>% as.data.frame
  year15_18 %<>% mutate(target = round((x28 - x23)/x23 *100, 2))
  year15_18 <<- year15_18
  # 中文太多會保智Rstudio指令不會動
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  md1 <<- glm(target ~ x6 * x9 * x12 * x16 * x19 * x20 * x21 * x22 * x27, data = year15_18)
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # ---------------------------------------------------------------------------------------------------------
  # md1 << md2 << md3 << md4
  md2 <<- glm(target ~ x6:x19 + x19:x20 + x12:x21 + x19:x27 + x6:x12:x19 + x12:x16:x19 + x12:x19:x20 + x6:x20:x21 + x6:x19:x22+ x19:x20:x22 + x12:x21:x27 +
                x6:x16:x20:x21 + x12:x16:x19:x22, data = year15_18)
  md3 <<- glm(target ~ x12:x21 + x19:x27 + x6:x20:x21, data = year15_18)
  md4 <<- glm(target ~ x12:x21 + x19:x27, data = year15_18)
  # ---------------------------------------------------------------------------------------------------------
  # md1 << md5 << md6 << md7 << md8
  md5 <<- glm(target ~ x6 * x19 + x19:x20 + x19:x27 + x6:x12:x19 + x12:x16:x19 + x12:x19:x20 + x6:x20:x21 + x16:x20:x21 + x6:x19:x22 + x19:x20:x22 + x12:x21:x27 + x6:x16:x20:x21
              + x6:x12:x19:x22 + x12:x16:x19:x22 + x12:x19:x20:x22 + x6:x16:x21:x22 + x6:x19:x21:x22 + x6:x20:x21:x22 + x16:x20:x21:x22 + x19:x20:x21:x22, data = year15_18)
  md6 <<- glm(target ~ x6 * x19 + x19:x20 + x6:x12:x19 + x27:x12:x21 + x6:x19:x12:x22 + x12:x16:x19:x22 + x12:x19:x20:x22 + x6:x20:x21:x22, data = year15_18)
  md7 <<- glm(target ~ x6 * x19 + x19:x20 + x6:x12:x19 + x27:x12:x21 + x6:x19:x12:x22 + x12:x16:x19:x22, data = year15_18)
  md8 <<- glm(target ~ x6 + x16:x19 + x19:x20 + x6:x12:x19 + x27:x12:x21 + x6:x19:x12:x22 + x12:x16:x19:x22, data = year15_18)


  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  md9 <<- glm(target ~ x4 * x5 * x22 + x10 * x11 * x22 + x14 * x15 * x22 + x17 * x18 * x22 + x20 * x22 + x21 * x22, data = year15_18)
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # md9 << md10 << md11 << md12 << md13
  md10 <<- glm(target ~ x4 * x5 * x22 + x10:x11 + x17 * x18 * x22, data = year15_18)
  md11 <<- glm(target ~ x4 * x5 * x22 + x10:x11,data = year15_18)
  md12 <<- glm(target ~ x4 * x5 + x5:x22 + x10:x11 + x4:x5:x22, data = year15_18)
  md13 <<- glm(target ~ x4 * x5 + x5:x22 + x4:x5:x22, data = year15_18)
  # --------------------------------------------------------------------
  # md14時間爆掉，solution: eval(parse(text =  ))
  # md14 <<- glm(target ~x4*x5*x6*x7*x8*x9*x10*x11*x12*x13*x14*x15*x16*x17*x18*x19*x20*x21*x22*x23*x24*x25*x26*x27*x28, data = year15_18)
  # for(i in range(5)){
  #   tmp <- names(coef(md14)) %>%  .[summary(md14)$coef[,4]  <=0.4 & !is.na(summary(md14)$coef[,4]) & .!= "(Intercept)"]
  #   tmp2 <- sprintf("glm(target ~ %s , data = year15_18)", paste(names(tmp), collapse = "+"))
  #   md14 <<- eval(parse(text = tmp2)) 
  # }
  # ---------------------------------------------------------------------------------------------------------
  # 交叉驗證,k=5
  model4CV <- cv.glm(year15_18, md4, K = 5)
  model8CV <- cv.glm(year15_18, md8, K = 5)
  model13CV <- cv.glm(year15_18, md13, K = 5)
  cvResults <- as.data.frame(rbind(model4CV$delta, model8CV$delta, model13CV$delta))
  names(cvResults) <- c("Error", "Adjusted.Error")
  cvResults$Model <- sprintf("model%s", c(4,8,13))
  # 模型殘差平方和
  cvANOVA <- anova(md4, md8, md13)
  cvResults$ANOVA <- cvANOVA$`Resid. Dev`
  # AIC
  cvResults$AIC <- AIC(md4, md8, md13)$AIC
  cvMelt <- melt(cvResults, id.vars = "Model", variable.name = "Measure", value.name = "Value")
  ggplot(cvMelt, aes(x = Model, y = Value)) + geom_line(aes(group = Measure, color = Measure)) + facet_wrap(~Measure, scales = "free_y") + theme(axis.text.x = element_text(angle = 90, vjust = .5)) + guides(color = FALSE)


  # ---------------------------------------------------------------------------------------------------------
  # dim_random <<- replicate(5,list(sort(sample(4:28, 5)))) 為了統一這邊
  # md_list <<- vector(mode = "list")
  # no <- dim_random %>% map(md_random, year15_18)
  # which(coef(md_list[[1]]) <= 0.2)
  # 
}
stock_bind <- function(tmp){
  load(tmp)
  colnames(stock) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21","x22","x23","x24","x25","x26","x27")
  return(stock)
}
# md_random <- function(x, year15_18){
# 
#   md_list <<- list.append(md_list, glm(year15_18$target ~ year15_18[,x[[1]]]*year15_18[,x[[2]]]*year15_18[,x[[3]]]*year15_18[,x[[4]]]*year15_18[,x[[5]]]))
# }