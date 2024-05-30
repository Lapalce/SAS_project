###表格读取
customers=read.csv("C:/Users/35842/Desktop/OlistData/olist_customers_dataset.csv")
geolocation=read.csv("C:/Users/35842/Desktop/OlistData/olist_geolocation_dataset.csv")
order_items=read.csv("C:/Users/35842/Desktop/OlistData/olist_order_items_dataset.csv")
order_payments=read.csv("C:/Users/35842/Desktop/OlistData/olist_order_payments_dataset.csv")
order_reviews=read.csv("C:/Users/35842/Desktop/OlistData/olist_order_reviews_dataset.csv")
orders=read.csv("C:/Users/35842/Desktop/OlistData/olist_orders_dataset.csv")
products=read.csv("C:/Users/35842/Desktop/OlistData/olist_products_dataset.csv")
sellers=read.csv("C:/Users/35842/Desktop/OlistData/olist_sellers_dataset.csv")
translation=read.csv("C:/Users/35842/Desktop/OlistData/product_category_name_translation.csv")

order=merge(order_items,orders,by="order_id")
order=merge(order,customers,by="customer_id")
order=merge(order,order_payments,by="order_id")
order=merge(order,order_reviews,by="order_id")
order=merge(order,products,by="product_id")
order=merge(order,translation,by="product_category_name")
all_data=merge(order,sellers,by="seller_id")

write.csv(all_data,file="C:/Users/35842/Desktop/OlistData/data1.csv",row.names = FALSE)


###预处理
data1=read.csv("C:/Users/35842/Desktop/OlistData/data1.csv")

library(lubridate)

data1$order_purchase_timestamp=ymd_hm(data1$order_purchase_timestamp)
latest_order_time = max(data1$order_purchase_timestamp)
data2 = data1[!duplicated(data1$order_id), ]

##（Recency）

data2$recency = difftime(latest_order_time,data2$order_purchase_timestamp,units = "day")
data2$recency = (as.integer(data2$recency))
recency=data2[,c(5,41)]

##（Frequency）
unique_customer_ids = unique(data1$customer_id)
# 计算每个用户ID对应的order_id数量
customer_order_counts = table(data1$customer_id)
# 查看结果
customer_order_counts
# 如果需要将这个频率表保存为一个数据框
frequency <- as.data.frame(customer_order_counts)
colnames(frequency) <- c("customer_id", "frequency")

#（Monetary）
monetary = aggregate(payment_value ~ customer_id, data = data2, FUN = sum)
names(monetary) = c("customer_id", "monetary")


data3=merge(recency,frequency,by="customer_id")
data3=merge(data3,monetary,by="customer_id")

write.csv(data3,file = "C:/Users/35842/Desktop/data3.csv",row.names = FALSE)
################################################################################

data3=read.csv("C:/Users/35842/Desktop/data3.csv")
# RFM图
par(mfrow=c(1,3))
hist(data3$recency)
hist(data3$frequency)
hist(data3$monetary)
# 按中位数划分
median_recency = median(data3[,2])
median_frequency = median(data3[,3])
median_monetary = median(data3[,4])
# 按CI上限划分
ci_upper_recency = unname(quantile(data3[,2],0.75) + 1.5 * (quantile(data3[,2],0.75) - quantile(data3[,2],0.25)))
ci_upper_frequency = unname(quantile(data3[,3],0.75) + 1.5 * (quantile(data3[,3],0.75) - quantile(data3[,3],0.25)))
ci_upper_monetary = unname(quantile(data3[,4],0.75) + 1.5 * (quantile(data3[,4],0.75) - quantile(data3[,4],0.25)))
# 按四分位点划分
q1_recency = unname(quantile(data3[,2],0.25))
q1_frequency = unname(quantile(data3[,3],0.25))
q1_monetary = unname(quantile(data3[,4],0.25))
# 按平均数划分
mean_recency = mean(data3[,2])
mean_frequency = mean(data3[,3])
mean_monetary = mean(data3[,4])


# 计算 R、F、M 值的评分
data3$R_score_by_mean = ifelse(data3[,2] <= mean_recency,1,0) 
data3$F_score_by_mean = ifelse(data3[,3] > mean_frequency,1,0) 
data3$M_score_by_mean = ifelse(data3[,4] > mean_monetary,1,0) 

data3$R_score_by_CI = ifelse(data3[,2] <= ci_upper_recency,1,0) 
data3$F_score_by_CI = ifelse(data3[,3] > ci_upper_frequency,1,0) 
data3$M_score_by_CI = ifelse(data3[,4] > ci_upper_monetary,1,0) 

data3$R_score_by_mid = ifelse(data3[,2] <= median_recency,1,0) 
data3$F_score_by_mid = ifelse(data3[,3] > median_frequency,1,0) 
data3$M_score_by_mid = ifelse(data3[,4] > median_monetary,1,0)

data3$R_score_by_q1 = ifelse(data3[,2] <= q1_recency,1,0) 
data3$F_score_by_q1 = ifelse(data3[,3] > q1_frequency,1,0) 
data3$M_score_by_q1 = ifelse(data3[,4] > q1_monetary,1,0)

#分离
data3$RFM_score_by_mean=apply(data3[, 5:7], 1, function(x) paste(x, collapse = ""))
data3$RFM_score_by_CI=apply(data3[, 8:10], 1, function(x) paste(x, collapse = ""))
data3$RFM_score_by_mid=apply(data3[, 11:13], 1, function(x) paste(x, collapse = ""))
data3$RFM_score_by_q1=apply(data3[, 14:16], 1, function(x) paste(x, collapse = ""))

rfm_segmentation = function(rfm_score) {
  if (rfm_score == '111') {return('High Value Customer')} 
  else if (rfm_score == '101') {return('Potential Customer')} 
  else if (rfm_score == '011') {return('Loyal Customer')} 
  else if (rfm_score == '001') {return('At Risk Customer')} 
  else if (rfm_score == '110') {return('Average Value Customer')} 
  else if (rfm_score == '100') {return('Average Potential Customer')} 
  else if (rfm_score == '010') {return('Average Loyal Customer')} 
  else if (rfm_score == '000') {return('Average At Risk Customer')} 
  else {print(paste("Unknown RFM score:", rfm_score))
    return(NA) # 或者返回一个标识未知的值
  }
}
customer_type_by_mean <- character(nrow(data3))
customer_type_by_CI <- character(nrow(data3))
customer_type_by_q1 <- character(nrow(data3))
customer_type_by_mid <- character(nrow(data3))

for (i in 1:nrow(data3)) {
  customer_type_by_mean[i] <- rfm_segmentation(data3[i,17])
  customer_type_by_CI[i] <- rfm_segmentation(data3[i,18])
  customer_type_by_q1[i] <- rfm_segmentation(data3[i,19])
  customer_type_by_mid[i] <- rfm_segmentation(data3[i,20])
}

data3$customer_type_by_mean <- customer_type_by_mean
data3$customer_type_by_CI <- customer_type_by_CI
data3$customer_type_by_q1 <- customer_type_by_q1
data3$customer_type_by_mid <- customer_type_by_mid

write.csv(data3,file = "C:/Users/35842/Desktop/data4.csv",row.names = FALSE)


#########################################################
#读SAS表换列名

CUSTOMER_TYPE_D=read.csv("C:/Users/35842/Desktop/CUSTOMER_TYPE_D.csv")
rownames(CUSTOMER_TYPE_D) = c("High Value Customer", "Potential Customer", 
"Loyal Customer","At Risk Customer","Average Value Customer","Average Potential Customer",
"Average Loyal Customer","Average At Risk Customer")
CUSTOMER_TYPE_E=CUSTOMER_TYPE_D[,-1]

write.csv(CUSTOMER_TYPE_E,file = "C:/Users/35842/Desktop/CUSTOMER_TYPE_E.csv",row.names = FALSE)






CUSTOMER_TYPE_D=read.csv("C:/Users/35842/Desktop/CUSTOMER_TYPE_D.csv")
CUSTOMER_TYPE_F <- as.data.frame(t(CUSTOMER_TYPE_D))
colnames(CUSTOMER_TYPE_F) = c("High Value Customer", "Potential Customer", 
                              "Loyal Customer","At Risk Customer","Average Value Customer","Average Potential Customer",
                              "Average Loyal Customer","Average At Risk Customer")
CUSTOMER_TYPE_F=CUSTOMER_TYPE_F[-1,]




write.csv(CUSTOMER_TYPE_F,file = "C:/Users/35842/Desktop/CUSTOMER_TYPE_F.csv",row.names = FALSE)






df4=read.csv("C:/Users/35842/Desktop/state.csv")
df4=df4[,-1]
rownames(df4)=df4[,1]
df4=df4[,-1]
vect1=unlist(df4)

trans_df4 <- data.frame(
  row = rep(rownames(df4), ncol(df4)),
  col = rep(colnames(df4), each = nrow(df4)),
  value =  vect1)
# 重命名列名
row.names(trans_df4)=NULL

trans_df4
write.csv(trans_df4,file = "C:/Users/35842/Desktop/trans_state.csv",row.names = FALSE)


###################################################################

df5=read.csv("C:/Users/35842/Desktop/CUSTOMER_TYPE_D.csv")
df6=df5[c(1,2),c(1,2,5)]

rownames(df6)=df6[,1]
df6=df6[,-1]
vect1=unlist(df6)

trans_df6 <- data.frame(
  row = rep(rownames(df6), ncol(df6)),
  col = rep(colnames(df6), each = nrow(df6)),
  value =  vect1)
# 重命名列名
row.names(trans_df6)=NULL

write.csv(trans_df6,file = "C:/Users/35842/Desktop/fisher.csv",row.names = FALSE)











