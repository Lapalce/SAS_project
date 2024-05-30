PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/rfm.csv"
	OUT = rfm
	DBMS = CSV
	REPLACE;
RUN;

PROC SQL;
	CREATE TABLE rfm1 AS
	SELECT customer_unique_id,recency,frequency,monetary,customer_type_by_mean,customer_type_by_CI,customer_type_by_q1,customer_type_by_mid
	FROM rfm;
QUIT;

PROC FREQ DATA=rfm1;
  TABLES customer_type_by_mean/ OUT=mean_counts;
RUN;

PROC FREQ DATA=rfm1;
  TABLES customer_type_by_CI/ OUT=CI_counts;
RUN;

PROC FREQ DATA=rfm1;
  TABLES customer_type_by_q1/ OUT=q1_counts;
RUN;

PROC FREQ DATA=rfm1;
  TABLES customer_type_by_mid / OUT=mid_counts;
RUN;

/* 计算每种客户类型在不同划分标准下的数量 */
PROC SQL;
  CREATE TABLE customer_type_counts_mean AS
  SELECT customer_type_by_mean AS customer_type, SUM(count) AS count_by_mean
  FROM mean_counts
  GROUP BY customer_type_by_mean;
  
  CREATE TABLE customer_type_counts_CI AS
  SELECT customer_type_by_CI AS customer_type, SUM(count) AS count_by_CI
  FROM CI_counts
  GROUP BY customer_type_by_CI;
  
  CREATE TABLE customer_type_counts_q1 AS
  SELECT customer_type_by_q1 AS customer_type, SUM(count) AS count_by_q1
  FROM q1_counts
  GROUP BY customer_type_by_q1;

  CREATE TABLE customer_type_counts_mid AS
  SELECT customer_type_by_mid AS customer_type, SUM(count) AS count_by_mid
  FROM mid_counts
  GROUP BY customer_type_by_mid;
QUIT;

/* 合并四张表 */
DATA customer_type_counts_final;
  MERGE customer_type_counts_mean customer_type_counts_q1 customer_type_counts_CI customer_type_counts_mid;
  BY customer_type;
RUN;
/*NA值替换为0*/
data customer_type_b;
        set customer_type_counts_final;
        array xx _numeric_;
        do over xx;
                if xx=. then xx=0;
        end;
run;

data customer_type_c;
  set customer_type_b; 
  if customer_type = 'High Value Customer' then num = 1;
  else if customer_type = 'Potential Customer' then num = 2;
  else if customer_type = 'Loyal Customer' then num = 3;
  else if customer_type = 'At Risk Customer' then num = 4;
  else if customer_type = 'Average Value Customer' then num = 5;
  else if customer_type = 'Average Potential Customer' then num = 6;
  else if customer_type = 'Average Loyal Customer' then num = 7;
  else if customer_type = 'Average At Risk Customer' then num = 8;
  else num = .; 
run;

proc sort data=customer_type_c;
  by num;
run;

data customer_type_d;
  set customer_type_c;
  drop num;
run;
/*排序表*/
PROC PRINT DATA=customer_type_d;
RUN;









PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/CUSTOMER_TYPE_F.csv"
	OUT = CUSTOMER_TYPE_F
	DBMS = CSV
	REPLACE;
RUN;

/*检验用户类型是否因分类方法和产生明显差异*/
DATA CUSTOMER_TYPE_G;
INPUT Customer_Type $26. Segmentation_Method $14. N;
DATALINES;
High_Value_Customer        count_by_mean 5040	
High_Value_Customer        count_by_q1   3326
High_Value_Customer        count_by_CI   5029
High_Value_Customer        count_by_mid  6023
Potential_Customer         count_by_mean 6929
Potential_Customer         count_by_q1   14445
Potential_Customer         count_by_CI   4126
Potential_Customer         count_by_mid  17703
Loyal_Customer             count_by_mean 3990
Loyal_Customer             count_by_q1   9907
Loyal_Customer             count_by_CI   0
Loyal_Customer             count_by_mid  5912
At_Risk_Customer           count_by_mean 5966	
At_Risk_Customer           count_by_q1   42294	
At_Risk_Customer           count_by_CI	  0
At_Risk_Customer           count_by_mid  17059
Average_Value_Customer     count_by_mean 2590
Average_Value_Customer     count_by_q1   168
Average_Value_Customer     count_by_CI   8989	
Average_Value_Customer     count_by_mid  958
Average_Potential_Customer count_by_mean 36579
Average_Potential_Customer count_by_q1   5683
Average_Potential_Customer count_by_CI   75250
Average_Potential_Customer count_by_mid  22082
Average_Loyal_Customer     count_by_mean 2399
Average_Loyal_Customer     count_by_q1   618
Average_Loyal_Customer     count_by_CI   1 
Average_Loyal_Customer     count_by_mid  1126
Average_At_Risk_Customer   count_by_mean 29903	   
Average_At_Risk_Customer   count_by_q1   16955	
Average_At_Risk_Customer   count_by_CI   1
Average_At_Risk_Customer   count_by_mid  22533
;
RUN;

PROC FREQ DATA=CUSTOMER_TYPE_G;
TABLES Customer_Type * Segmentation_Method /CHISQ;
WEIGHT N; 
RUN;

/* 检验均值中位数 */
PROC NPAR1WAY DATA=CUSTOMER_TYPE_G WILCOXON;
    CLASS Customer_Type;
    VAR N;
RUN;

/* 检验按means的分类标准下，各用户类型和购买商品类型的关系 */

PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/trans_product.csv"
	OUT = product
	DBMS = CSV
	REPLACE;
RUN;

PROC FREQ DATA=product;
TABLES row * col /CHISQ;
WEIGHT value; 
RUN;

/* 检验按means的分类标准下，各用户类型和支付手段的关系 */
PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/trans_payment.csv"
	OUT = payment
	DBMS = CSV
	REPLACE;
RUN;

PROC FREQ DATA=payment;
TABLES row * col /CHISQ;
WEIGHT value; 
RUN;

/* 检验按means的分类标准下，各用户类型和地区的关系 */
PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/trans_state.csv"
	OUT = state
	DBMS = CSV
	REPLACE;
RUN;

PROC FREQ DATA=state;
TABLES row * col /CHISQ;
WEIGHT value; 
RUN;


/* 进行Fisher确切检验 */
PROC IMPORT
	DATAFILE = "/home/u63794779/zzzrq/SAS/Proj/fisher.csv"
	OUT = fisher
	DBMS = CSV
	REPLACE;
RUN;

PROC FREQ DATA = fisher;
	TABLES row * col / CHISQ; 
	EXACT FISHER;
	WEIGHT value; 
RUN;



















