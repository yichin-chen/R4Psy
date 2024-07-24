## 第一题 -----------------------------------
### 接 Chapter 6
files <- list.files(  #<<
  ## <- & =
  here::here("data", "match"), 
  pattern = "data_exp7_rep_match_.*\\.out$", 
  full.names = TRUE)
convert_data_types = function(df) {
  df = df %>% 
    dplyr::mutate(Date = as.character(Date),Prac = as.character(Prac),
                  Sub = as.numeric(Sub),Age = as.numeric(Age),
                  Sex = as.character(Sex),Hand = as.character(Hand),
                  Block = as.numeric(Block),Bin = as.numeric(Bin),
                  Trial = as.numeric(Trial),Shape = as.character(Shape),
                  Label = as.character(Label),Match = as.character(Match),
                  CorrResp = as.character(CorrResp),Resp = as.character(Resp),
                  ACC = as.numeric(ACC),RT = as.numeric(RT))
  return(df)
}


df3 = data.frame() 

for (i in seq_along(files)) {
  # 读取
  df = read.table(files[i], header = TRUE) %>%  
    dplyr::filter(Date != "Date") %>%  
    convert_data_types() 
  # 合并
  df3 = dplyr::bind_rows(df3, df) 
}
# 删除临时变量
rm(df, files, i)


#1. 读取match数据，对不同shape的击中率进行分组绘图，可使用boxplot观察差异。
#2. 在上一题中，如何按照特定的顺序来展现 boxplot，
#   比如按照moralSelf - immoralSelf - moralOther - immoralOther(提示：设置因子)

####  plot ######
df3 %>% group_by(Sub, Shape) %>%
  dplyr::summarise(
    hit = length(ACC[Match == "match" & ACC == 1]),
    fa = length(ACC[Match == "mismatch" & ACC == 0]),
    miss = length(ACC[Match == "match" & ACC == 0]),
    cr = length(ACC[Match == "mismatch" & ACC == 1]))%>% 
  mutate(
     hit_rate = hit /(hit+fa)
   ) %>%
  ## 修改 Shape 为因子
  mutate(Shape = factor(
    Shape,
    levels  = cc('moralSelf,immoralSelf,
                  moralOther ,immoralOther')
  )) %>% 
  ggplot(aes(x = Shape,y = hit_rate,fill = Shape)) + 
  geom_boxplot(staplewidth = 0.8) + 
  bruceR::theme_bruce()
  

##  第二题  ------------------------------
## 3. 读取penguin数据，选择自己感兴趣的两个变量进行处理并画出散点图。

pg_raw = bruceR::import(here::here(
  "data", "penguin","penguin_rawdata.csv"))
pg_raw %>% 
  mutate(
    # 计算均值
    m_ALEX = bruceR::MEAN(.,likert = 1:7,
                          var = 'ALEX',items = 1:16,rev = c(4,12,14,16)),
    m_stress = bruceR::MEAN(.,likert = 1:5,
                            var = 'stress',items = 1:14,rev = c(4:7,9,10,13)),
    m_ECR = bruceR::MEAN(.,likert = 1:8,
                         var = 'ECR',items = 1:36
    )
  ) %>% 
  ggplot(aes(x = m_ALEX, y = m_stress)) + 
  ## 将散点大小和颜色与m_ECR映射
  geom_point(aes(size = m_ECR, color = m_ECR)) + 
  ## 修改颜色属性
  scale_color_viridis_c() + 
  ## 修改坐标轴名称
  labs(x = 'Mean of ALEX', y = 'Mean of Stress') +
  ## 合并 legend
  guides(size = guide_legend(title = "ECR"),
         color = guide_legend(title = "ECR")) + 
  ## 使用 APA 主题
  bruceR::theme_bruce() 



