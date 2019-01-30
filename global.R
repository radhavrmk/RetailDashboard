library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(dplyr)
library(plotly)
library(treemap)
library(RColorBrewer)

# Global Variables ####
exclude_categories = c("44000", "4400C","44X72", "44Z72", "44W72", "441", "441X", "4411", "44111", "44112", "4413")
master_categories = c("4400A", "44Y72")
exclude_types = c("RATIO", "PCT")
remove_strings = c("[a-zA-Z0-9:]+") 
date_frmts = c("%b-%y", "%y-%b", "%m/%d/%Y")

# setwd("/Users/RV/Documents/Radha/OneDrive - Cognizant/Personal/Learn/DataScience/NYC/classes/handson/Projects/RetailTrendsShiney/RetailSales")

cleanDescription = function(original, remove_strings){
  for (i in remove_strings) {
    clean = str_remove(original, i)
  }
  return(trimws(clean))
}

find_last_full_quarter = function(df){
  max_date = max(df$date)
  last_full_Q_date = max_date
  month_to_check = month(max_date)
  if (month_to_check %in% c(1, 4, 7, 10)) {
    last_full_Q_date = max_date %m+% months(-1)
  } else if (month_to_check %in% c(2, 5, 8, 11)) {
    last_full_Q_date = max_date %m+% months(-1)
  } else
    last_full_Q_date = max_date
  return(last_full_Q_date)
}

# Load the retail data from multiple CSV file and pre-process ####
loadPreprocessRetailData = function(){
  data_df = read.csv("./data/raw/MRTS-mf-data.csv", stringsAsFactors = FALSE)
  category_df = read.csv("./data/raw/MRTS-mf-data-category.csv", stringsAsFactors = FALSE)
  time_df = read.csv("./data/raw/MRTS-mf-data-time-periods.csv",
                     stringsAsFactors = FALSE)
  etype_df = read.csv("./data/raw/MRTS-mf-error-type.csv", stringsAsFactors = FALSE)
  dtype_df = read.csv("./data/raw/MRTS-mf-data-type.csv", stringsAsFactors = FALSE)
  
  df = data_df %>%
    left_join(category_df, by = "cat_idx") %>%
    left_join(time_df, by = "per_idx") %>%
    left_join(dtype_df, by = "dt_idx") %>%
    left_join(etype_df, by = "et_idx") %>%
    mutate(value = as.numeric(val), date = as_date(parse_date_time(per_name, orders =
                                                                     date_frmts))) %>%
    mutate(
      year = year(date),
      month = month(date),
      quarter = quarter(date)
    ) %>%
    mutate(temp = paste0(cat_code, ": ")) %>%
    mutate(cat_desc2 = trimws(str_remove(cat_desc, remove_strings))) %>%
    select(-temp)
  
  last_full_Q_date = find_last_full_quarter(df)
  df = filter(df, date <= last_full_Q_date)

  return(df)
}


#Load and process eCommerce data ####
loadEcommerceData = function(){
  df = read.csv("./data/raw/FRED_ecomm.csv", stringsAsFactors = FALSE)
  df = df %>%
    mutate(date = as_date(parse_date_time(df$date, orders=date_frmts))) %>%
    mutate(cat_desc = "E-commerce Retail Sales")
  return(df)
}

processEcommerceData = function(ecomm_raw_df, retail_df){
  
  
  ecomm_df = retail_df %>%
    filter(dt_code == "SM", cat_code == "44000", is_adj == 1) %>%
    select(date, cat_desc = cat_desc2, value) %>%
    mutate(date = floor_date(date, "quarter")) %>%
    group_by(date, cat_desc) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    filter(date %in% ecomm_raw_df$date) %>%
    rbind(select(ecomm_raw_df, date, cat_desc, value)) %>%
    spread(key = cat_desc, value = value) %>%
    mutate(`ex E-com Sales` = `Retail Trade` - `E-commerce Retail Sales`) %>%
    # mutate(`pct E-Comm sales` = `E-commerce Retail Sales` / `Retail Trade`) %>%
    # select(-`pct E-Comm sales`) %>%
    gather(
      key = cat_desc,
      value = value,
      `Retail Trade`,
      `E-commerce Retail Sales`,
      `ex E-com Sales`
    )
  
  ecomm_df = ecomm_df %>% group_by(cat_desc) %>%
    # arrange(cat_desc, date) %>%
    mutate(pct_diff_inc = ((value - lag(value)) / value) * 100) %>%
    ungroup()
  
  ecomm_df = ecomm_df %>%
    mutate(quarter = quarter(date)) %>%
    group_by(quarter, cat_desc) %>%
    mutate(pct_diff = ((value - lag(value)) / value) * 100) %>%
    left_join(ecomm_raw_df[, c("date", "pct_of_retail")], by = "date")
  
  return(ecomm_df)
}

cleanRetailData = function(df){
  df = df %>% 
    filter(et_idx == 0, !is.na(value)) %>%
    filter(! cat_code %in% exclude_categories) %>%
    filter(! dt_unit %in% exclude_types) %>%
    select(-ends_with("_idx")) %>% 
    arrange(date, cat_desc2)
  return(df)
}

createSubSectorList = function(df){
  subsec_df = df %>% filter(is_adj == 1) %>%
    filter(!cat_code %in% master_categories) %>%
    select(cat_code, cat_desc2) %>%
    arrange(cat_desc2) %>%
    distinct()
  
  return(subsec_df$cat_desc2)
}

# function to apply multiple filters on the dataframe ####
applyFilters = function(df, min_date=NULL, max_date=NULL, SA=TRUE, data_type = "SM", master_cats = TRUE, sector = NULL){
  
  df = filter(df, is_adj == SA)
  if(master_cats) df = filter(df, cat_code %in% master_categories) 
    else df = filter(df, !cat_code %in% master_categories)
  if(data_type != "ALL") df = filter(df, dt_code==data_type)
  if (!is.null(min_date)) df = filter(df, year>=min_date)
  if (!is.null(max_date)) df = filter(df, year<=max_date)
  if (!is.null(sector)) df = filter(df, cat_desc2 == sector)
  return(df)
}

   
# Function to apply roll-ups ####
# sub_sec flag is for future use where current subsectors will be grouped 
#futher to broader categories
applyRollups = function(df, sub_sec = FALSE, period = "month", YoY = TRUE, func=sum) {
  
  group_cols = c("is_adj")
  
  if (sub_sec)
    group_cols = append(group_cols, "sub_sec")
  else
    group_cols = append(group_cols, "cat_desc2")
  
  if (period == "year")
    group_cols = append(group_cols, period)
  else if (period == "quarter") {
    if (YoY)
      group_cols = append(group_cols, c("quarter", "year"))
    else
      group_cols = append(group_cols, c("year", "quarter"))
  }
  else if (period == "month") {
    if (YoY)
      group_cols = append(group_cols, c("month", "year"))
    else
      group_cols = append(group_cols, c("year", "month"))
  }
  message("grouping by : ", group_cols)
  
  df = df %>%
    group_by(.dots = group_cols) %>%
    summarise(date = min(date), value = func(value))
  
  if (!YoY)
    df = ungroup(df)
  
  df = mutate(df, pct_diff = ((value - lag(value)) / lag(value)) * 100)
  return(df)
}

calculate12MonthSales = function(df){
  df2 = applyFilters(df, min_date = global_max_year -3, max_date = global_max_year, master_cats = FALSE)
  df2 = applyRollups(df2, period = "month", YoY = TRUE)
  df2 = df2 %>% group_by(cat_desc2) %>%
    top_n(12, wt=date) %>%
    summarise(total_value = sum(value), avg_growth = mean(pct_diff))  
  return(df2)
}

calculate12MonthInventory = function(df){
  df2 = applyFilters(df, min_date = global_max_year -3, max_date = global_max_year, master_cats = FALSE,data_type = "IM")
  df2 = applyRollups(df2, period = "month", YoY = TRUE)
  df2 = df2 %>% group_by(cat_desc2) %>%
    top_n(12, wt=date) %>%
    summarise(avg_value = mean(value), avg_growth = mean(pct_diff))
  return(df2)
}

get_top_n = function(df, number_needed=5, data_type = "value"){
  if (data_type == "value"){
    top_five = df %>%
      group_by(date) %>%
      top_n(n=number_needed, wt = value) %>%
      ungroup()
  } else{
    top_five = df %>%
      group_by(date) %>%
      top_n(n=number_needed, wt = pct_diff) %>%
      ungroup()
  }  
  return(top_five)
}

# Loading ####
df = loadPreprocessRetailData()
ecomm_raw_df = loadEcommerceData()
ecomm_df = processEcommerceData(ecomm_raw_df, df)
df3 = cleanRetailData(df) ## to be done after loading retail data 
subsec_list = createSubSectorList(df3)

# Set further global variables ####
# Hardcoding for 2017 below need to be removed and get last full year from data
global_min_year = min(df$year)
global_max_year = max(df$year)
data_type = "SA"
current_year= 2017
current_quarter = 4
years_disp = unique(df$year)
quarters_disp = 1:4
months_disp = 1:12
period_disp = c("Yearly"="year", "Quarterly"="quarter", "Monthly" = "month")

last_12mth_ret_sales = calculate12MonthSales(df3)
last_12mth_ret_inv = calculate12MonthInventory(df3)
ecomm_stats = ecomm_df %>% group_by(cat_desc) %>%
  top_n(4, wt=date) %>%
  summarise(total_value = sum(value), avg_growth = mean(pct_diff))


build_primary_plot = function(df_upd, type_of_data, xlabel = "", ylabel="", title=""){
  
  df_upd = na.omit(df_upd) 
  if(type_of_data == "value") df_upd$value =  df_upd$value/1e3
  else if (type_of_data == "pct_diff") df_upd$pct_diff =  df_upd$pct_diff/1e2
  
  g = df_upd  %>%
    ggplot(aes(x=date, y=df_upd[[type_of_data]], color=cat_desc2)) +
    geom_line(na.rm = TRUE) +
    scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=xlabel, y =ylabel, title = title ) +
    theme(legend.text = element_text(colour="blue", size=7), legend.position="bottom") 
  
  if (type_of_data == "value") g = g + scale_y_continuous(labels = comma) 
  else if (type_of_data == "pct_diff") g = g + scale_y_continuous(labels = percent) 
  #   labs(x = "", y = "Quarterly Sales(in $bln)")   
  return(g)
}

build_top5_plot = function(top_five, type_of_data="value", xlabel = "", ylabel="", title="" ){
  
  top_five = na.omit(top_five) 
  if(type_of_data == "value") top_five$value =  top_five$value/1e3
  else if (type_of_data == "pct_diff") top_five$pct_diff =  top_five$pct_diff/1e2
  
  g = top_five %>% ggplot(aes(x=date, y=top_five[[type_of_data]], fill=cat_desc2)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x=xlabel, y =ylabel, title = title ) +
    theme(legend.text = element_text(colour="blue", size=7), legend.position="none") 
  
  if (type_of_data == "value") g = g + scale_y_continuous(labels = comma) 
  else if (type_of_data == "pct_diff") g = g + scale_y_continuous(labels = percent) 
  
  return(g)
}

# eComm plots ####
geteComVsRetailSales = function(){
  ecomm_df %>% ggplot(aes(x=date, y = value/1e3, color = cat_desc)) +
    geom_line() +  
    theme(legend.text = element_text(colour="blue", size=7), legend.position="bottom") +
    theme(axis.text.y = element_text(angle = 90, hjust = 1))+
    scale_y_continuous(labels = comma) + 
    labs(x = "", y = "Quarterly Sales(in $bln)") 
}

getEcommPctSales = function(){
  ecomm_raw_df %>% ggplot(aes(x = date, y = pct_of_retail/100)) +
    geom_bar(stat = "identity", fill = "tomato") +
    theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = "Percent of Retail")
}

getEcomYOYGrowth = function(){
  ecomm_df %>% ggplot(aes(x = date, y = pct_diff/100, color = cat_desc)) +
    geom_line() + geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, color = "black") +
    theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = "Percent Growth - YOY")
}




# xx = df3 %>% filter(cat_desc2 == "Food and Beverage Stores", is_adj==0, dt_code=="SM") %>%
#     select(date, value) %>% 
#     mutate(xval = (year(date)-1992)*12 + month(date))
# 
# linearModel = loess(value~xval,na.omit(xx))           
# 
# summary(linearModel)
# coeffx = linearModel$coefficients
# res = linearModel$residuals
# res2 = residuals(linearModel)
# 
# all.equal(res,res2)
# 
# xx %>% select(date, value) 
# 
# xx%>%    ggplot(aes(x= xval, y = value)) + geom_point(color="red") +
#     geom_smooth(method = "loess", color = "blue")
# 
# yy = cbind(xx,res=res)
# yy %>% 
#   ggplot(aes(x= xval, y = value)) + geom_point(color="red") + 
#   geom_smooth(method = "loess", color = "blue") +
#   geom_point(aes(x=xval, y =value+res ), color = "green")



# 
# df6 = applyRollups(df4, period = "month", YoY=TRUE)
# df7 = applyRollups(df4, period = "year", YoY=TRUE)
# df8 = applyRollups(df4, period = "year", YoY=TRUE)
# 
# df5 = na.omit(df5)


# upd_df = applyFilters(df3,min_date = global_min_year, max_date = global_max_year,  master_cats = FALSE)
# 
# upd_df = applyRollups(upd_df, period = "year", YoY = TRUE)
# 
# upd_df = upd_df %>% filter(year(date) == current_year) %>%
#           mutate(value = value/1e3, pct_diff = pct_diff/1e2)
# upd_df = na.omit(upd_df)
# 
# median_value = median(upd_df$value)
# median_growth = median(upd_df$pct_diff)
# 
# g = ggplot(upd_df, aes(x=value, y = pct_diff, label=cat_desc2)) + geom_point(size=2, color = "tomato") + 
#   # geom_text(aes(label=ifelse((pct_diff>median_growth & value > median_value),cat_desc2,""),hjust=0,vjust=0, size = 2, nudge_y = 0.5 ))+
#   # geom_label()+  
#   geom_hline(yintercept = median_growth, color="orange")+
#   geom_vline(xintercept =  median_value, color="orange")+
#   theme(legend.position = "null") +
#   scale_y_continuous(labels = percent) + 
#   scale_x_continuous(labels = comma) 
# g
# ggplotly(g)


## Get Top n categories







# xx = df8 %>% group_by(date) %>% top_n(5, wt = value) %>% ungroup()
# yy = df8 %>% group_by(date) %>% top_n(-5, wt = value) %>% ungroup()
# 
# 
# xx2 = get_top_n(df8, number_needed = 5, data_type = "value")
# 
# yy = get_top_n(df8, -5, data_type = "value")
# 
# all.equal(xx,yy)


# df4 = applyFilters(df3, min_date = 1993, max_date = 2018, data_type = "SM", master_cats = FALSE)
# 
# df8 = applyRollups(df4, period = "month", YoY=TRUE)
# 
# 
# xx = df8 %>% filter(cat_desc2 == "All Other Gen. Merchandise Stores" )
# g1 = build_primary_plot(xx,type_of_data = "value")
# g1
# 
# xx = df8 %>% filter(cat_desc2 == "All Other Gen. Merchandise Stores")
# mean_val = mean(xx$pct_diff, na.rm = TRUE)
# print(mean_val)
# 
# g1 = build_primary_plot(xx,type_of_data = "pct_diff") + 
#     geom_hline(yintercept = 0, color = "black")  +
#     geom_hline(yintercept = mean_val, color = "blue") + 
#     geom_smooth( method = lm, se=FALSE, color="purple")
# 
# g1
# 
# xx %>% select(date, pct_diff) %>%
#   ggplot()
# 
# df4 = applyFilters(df3, min_date = global_min_year, max_date = global_max_year, data_type = "SM", master_cats = FALSE)
# df8 = applyRollups(df4, period = "month", YoY=TRUE)
# 
# last_12mon_sales_YOY = df8 %>% 
#         arrange(desc(date)) %>% 
#         group_by(cat_desc2) %>%
#         top_n(12, wt=date) %>%
#         summarise(mean_sales = mean(value), mean_growth = mean(pct_diff))
# 
# df8 = applyRollups(df4, period = "month", YoY=FALSE)
# 
# last_12mon_sales = df8 %>% 
#   arrange(desc(date)) %>% 
#   group_by(cat_desc2) %>%
#   top_n(12, wt=date) %>%
#   summarise(mean_sales = mean(value), mean_growth = mean(pct_diff))








# ecomm_df %>% ggplot(aes(x=date, y = value, color = cat_desc)) +
#   geom_line() + scale_y_log10()
# 
# ecomm_df %>% ggplot(aes(x=date, y = pct_diff, fill = cat_desc)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_smooth(se=FALSE, aes(color =  cat_desc)) +
#   # geom_smooth(data = xy, se=FALSE, method = lm, formula = y~x, aes( color = "green" )) +
#   geom_hline(yintercept = 0, color="black") +
#   theme_bw()
# ####

# 
# build_tree = function(df){
#   g = treemap(df,index = "cat_desc2", vSize = "value")
#   return(g)
# }
# 
# g3 = build_tree(xx)

# g4 = bottom_five %>% ggplot(aes(x=period, y=value, fill=cat_desc2)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_minimal()


# xx = df7 %>% filter(year(date) == 2016)
# 
# upd_df = applyFilters(df3,min_date = 1992, max_date = 2018 )
# print(unique(upd_df$dt_code))
# 
# upd_df = applyRollups(upd_df, period = "year",YoY = TRUE)
# 
# upd_df = filter(na.omit(upd_df), year(date) == current_year)  
# median_value = median(upd_df$value)
# median_growth = median(upd_df$pct_diff)
# 
# 
# g = ggplot(upd_df, aes(x=value, y = pct_diff)) + geom_point(size=3, color = "tomato") + 
#   # geom_label_repel(aes(label = ifelse(pct_diff>=5,cat_desc2,"")),
#   #                  box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50') +
#   geom_text(aes(label=ifelse(pct_diff>median_growth,cat_desc2,'')),hjust=0,vjust=0)+
#   
#   geom_abline(slope = 0, intercept = median_growth, color="orange")+
#   geom_abline(slope = 90, intercept = median_value, color="orange")+
#   theme(legend.position = "null") 
# 
# ggplotly(g)
# 
# 
# ggplot(xx, aes(x=value, y = pct_diff)) + geom_point(size=3) + 
#   geom_label_repel(aes(label = ifelse(pct_diff>=5,cat_desc2,"")),
#                    box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50') +
#   geom_abline(slope = 0, intercept = 5, color="orange")+
#   theme(legend.position = "null") 
# 
# 
# yy = parse_date("2018-12-03")
# 
# quarter(yy)
# 
# xx = data.frame(y=c(1,4,9,16,25),row.names = c("a","b","c","d","e"))
# xx = data.frame(y=c(1,4,9,16,25),x = c("a","b","c","d","e"))
# 
# mosaicplot(xx)
# treemap(xx,index = "x", vSize = "y")

# dfxx = df3 #%>% filter(year == 2010)
# dfxx2 = dfxx %>% 
#         select(cat_desc2, is_adj, dt_code, value, date)%>% 
#         filter (is_adj == 1, dt_code == "SM") %>%
#         select(-is_adj, -dt_code) %>%
#         spread(key=cat_desc2,value=value)
#       

# dfxx3 = spread(dfxx2, key=dt_code, value = value)
# 
# rownames(dfxx2) = dfxx2$date  
# 
# dfxx2 = select(dfxx2, -date)
# is.ts(tsx)
# tsx = ts(dfxx2)
# 
# 
# plot.ts(tsx)

# 
# expenses <- data.frame(
#   date=seq(as.Date("2016-03-01"), as.Date("2016-12-31"), by=1),
#   amount=rgamma(length(date), shape = 2, scale = 20))
# expenses$dt2 = floor_date(expenses$date,"year")
# expenses$dt3 = floor_date(expenses$date,"quarter")
# 
# xx = expenses %>% mutate(date = floor_date(date,"month"))

# xx = month(df3$date)
# st1 = "month"
# st2 = paste0(st1,"(df3$date)")
# # st2 = list("df3$date")
# st3 = parse(text=st2)
# st3
# xx = eval(st3)
# 
# ?parse
# 
# year()



# df5 %>%
#   filter(.,year == 1994,quarter == 3) %>%
#   arrange(.,desc(value))
# 
# 
# cols_g = c("cat_desc", "year")
# 
# dfx = group_by(df4, .dots=cols_g)
# 
# dfx = df4 %>% group_by(cat_desc) %>% group_by("year")

# xx  = applyFilters(df3,min_date = 2003)

# build_primary_plot = function(min_year){
#   # print(min_year)
#   g1 = applyFilters(df3,min_date = min_year)  %>%
#     ggplot(aes(x=date, y=value, color=cat_desc2)) +
#     geom_line() +
#     scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   return(g1)
# }


# ggplotly(g)

# g2 = df32 %>% ggplot(aes(x=date, y=value, color=cat_desc2)) +
#   geom_line() +
#   scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplotly(g)


# bottom_five = df5 %>%
#   mutate(period = paste0(as.character(year), "-" , as.character(quarter))) %>%
#   group_by(period,quarter) %>%
#   top_n(-5,wt = value) 