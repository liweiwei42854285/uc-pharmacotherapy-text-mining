# ==========================================
# 1. 读取两层 txt 字典，准备进行替换和分类
# ==========================================
# 假设 synonyms.txt 和 generation.txt 都在 E:/R_Data
syn_lines <- readLines("synonyms.txt")
syn_lines <- syn_lines[syn_lines != ""]

gen_lines <- readLines("generation.txt")
gen_lines <- gen_lines[gen_lines != ""]

# ==========================================
# 2. 提取数据，统一转小写，并手动合并同义词
# ==========================================
library(tidyverse)

# 提取年份和关键词，剔除没有关键词的文献
df <- M %>% 
  select(PY, DE) %>% 
  filter(!is.na(DE)) %>%
  mutate(PY = as.numeric(PY), DE = tolower(DE))

# 手动执行同义词替换 (处理第一层字典)
for (line in syn_lines) {
  # 拆分 "标准词: 别名1; 别名2"
  parts <- strsplit(line, ":")[[1]]
  if(length(parts) >= 2) {
    target <- tolower(trimws(parts[1]))
    aliases <- tolower(trimws(strsplit(parts[2], ";")[[1]]))
    # 遍历所有别名，替换为标准词
    for (alias in aliases) {
      df$DE <- str_replace_all(df$DE, fixed(alias), target)
    }
  }
}

# ==========================================
# 3. 根据第二层字典，打上代际标签
# ==========================================
# 解析代际字典
dict <- list()
for (line in gen_lines) {
  parts <- strsplit(line, ":")[[1]]
  if(length(parts) >= 2) {
    gen_name <- trimws(parts[1])
    drugs <- tolower(trimws(strsplit(parts[2], ";")[[1]]))
    dict[[gen_name]] <- drugs
  }
}

# 循环打标签 (匹配到该代的药，该列就记为 1)
for (gen in names(dict)) {
  pattern <- paste(dict[[gen]], collapse = "|")
  df[[gen]] <- ifelse(str_detect(df$DE, pattern), 1, 0)
}

# ==========================================
# 4. 按年份统计并画图
# ==========================================
# 按年求和，并只保留 2000 年及以后的数据
df_trend <- df %>%
  group_by(PY) %>%
  summarise(across(starts_with("G"), sum)) %>%
  filter(PY >= 2000)

# 转长格式
df_long <- df_trend %>%
  pivot_longer(cols = -PY, names_to = "Generation", values_to = "Count")

# 绘图（使用你之前喜欢的配色）
p <- ggplot(df_long, aes(x = PY, y = Count, fill = Generation)) +
  geom_area(alpha = 0.85) + 
  scale_fill_manual(values = c(
    "G1_conventional" = "#8DA0CB", 
    "G2_antiTNF" = "#66C2A5",
    "G3_nonTNF_biologic" = "#FC8D62",
    "G4a_panJAK" = "#E78AC3",
    "G4b_selectiveJAK1" = "#E31A1C"
  )) +
  labs(title = "Generational Evolution of Immunosuppressants in UC",
       x = "Publication Year",
       y = "Number of Publications",
       fill = "Drug Generation") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# 在右下角预览图片
print(p)

# 自动保存为高清 PDF 和 PNG 到 E:/R_Data
ggsave("UC_Generation_Evolution.pdf", plot = p, width = 10, height = 6, dpi = 300)
ggsave("UC_Generation_Evolution.png", plot = p, width = 10, height = 6, dpi = 300)
