# ==============================================================================
# 第一步：环境准备与数据导入
# ==============================================================================
# 加载必备包（如果没有安装请先运行 install.packages(c("bibliometrix", "tidyverse"))）
library(bibliometrix)
library(tidyverse)

# 【重要】设置工作目录为你存放 download_data.txt 的文件夹路径
# 请把下面的路径换成你自己的，注意斜杠用 / 或者 \\
setwd("E:/R_Data") 

# 导入 WoS 数据
cat("正在导入数据，请稍候...\n")
M <- convert2df(file = "download_data.txt", dbsource = "wos", format = "plaintext")

# ==============================================================================
# 第二步：核心数据清洗（同义词批量替换）
# ==============================================================================
# 你提供的超强同义词字典
my_synonyms <- c(
  "azathioprine: AZA; AZTP; Imuran; Azasan; Jayempi; BW 57-322; NSC-39084",
  "6-mercaptopurine: 6-MP; MP; Purinethol; Purixan; Xaluprine; mercaptopurine; 6-purinethiol; 6-thiopurine; 6-thioxopurine; purine-6-thiol; U-4748; WR-2785",
  "methotrexate: MTX; Rheumatrex; Trexall; Otrexup; Rasuvo; RediTrex; Jylamvo; Xatmep; Maxtrex; Metoject; amethopterin; emthexate; ledertrexate; Mexate; Folex; Abitrexate; CL 14377; WR-19039",
  "cyclosporine: CsA; CyA; CYA; cyclo; Sandimmune; Neoral; Gengraf; Ciclosporin; Cyclosporin A; Cyclosporine A",
  "tacrolimus: FK506; TAC; Tac; Prograf; Advagraf; Adoport; Dailiport; FK-506; FR-900506; Tsukubaenolide",
  "6-thioguanine: 6-TG; TG; ThG; Lanvis; Tabloid; Tioguanine; 2-Amino 6MP; 6-Mercaptoguanine; BW 5071; WR-1141; Wellcome U3B",
  "infliximab: IFX; Remicade; Zymfentra; cA2; infliximab-dyyb; infliximab-abda; infliximab-qbtx",
  "adalimumab: ADA; Humira; D2E7; adalimumab-atto; adalimumab-adbm; adalimumab-afzb; adalimumab-bwwd; adalimumab-fkjp",
  "golimumab: GLM; Simponi; Simponi Aria; CNTO 148; golimumab-sldi; Immgolis; Immgolis Intri",
  "certolizumab: CZP; Cimzia; Certolizumab pegol; CDP 870; CDP870; PHA-738144; Xcimzane",
  "vedolizumab: VDZ; VEDO; Entyvio; LDP-02; AVT80",
  "ustekinumab: UST; USTK; Stelara; CNTO-1275; L04AC05; TT-20; ustekinumab-auub; Wezlana; ustekinumab-aekn; Selarsdi; ustekinumab-ttwe; Pyzchiva; ustekinumab-aauz; Otulfi; ustekinumab-srlf; Imuldosa; ustekinumab-kfce; Yesintek; ustekinumab-hmny; Starjemza; ustekinumab-stba; Steqeyma; Wezenla; Uzpruvo; Fymskina; Absimky",
  "mirikizumab: MIRI; miri; Omvoh; LY3074828; mirikizumab-mrkz",
  "risankizumab: RIS; RISA; Skyrizi; BI 655066; risankizumab-rzaa",
  "guselkumab: GUS; GUSK; Tremfya; CNTO 1959",
  "tofacitinib: TOF; Tofa; Xeljanz; Xeljanz XR; CP-690,550; CP-690550; CP-690,550-10; Tasocitinib; Tofacitinib citrate",
  "upadacitinib: UPA; Rinvoq; ABT-494; ABT494",
  "filgotinib: FIL; Jyseleca; GLPG0634; GS-6034; GLPG-0634; GS6034"
)

# 将同义词转换为 R 的命名向量，并进行批量替换
cat("正在清洗同义词...\n")
syn_map <- list()
for (line in my_synonyms) {
  parts <- strsplit(line, ":")[[1]]
  standard_term <- trimws(parts[1])
  aliases <- trimws(strsplit(parts[2], ";")[[1]])
  for (alias in aliases) {
    syn_map[[alias]] <- standard_term
  }
}
syn_vector <- unlist(syn_map)

# 清洗后的数据集：M_clean
M_clean <- M %>% 
  mutate(DE = str_replace_all(DE, regex(syn_vector, ignore_case = TRUE)))

# ==============================================================================
# 第三步：绘制【王炸图】药物代际演化堆叠面积图
# ==============================================================================
cat("正在生成代际演化堆叠图...\n")
# 解析代际字典
generation_map <- c(
  "G1_conventional: azathioprine; 6-mercaptopurine; methotrexate; cyclosporine; tacrolimus; 6-thioguanine",
  "G2_antiTNF: infliximab; adalimumab; golimumab; certolizumab",
  "G3_nonTNF: vedolizumab; ustekinumab; mirikizumab; risankizumab; guselkumab",
  "G4a_panJAK: tofacitinib",
  "G4b_selectiveJAK1: upadacitinib; filgotinib"
)

dict <- list()
for (line in generation_map) {
  parts <- strsplit(line, ":")[[1]]
  gen_name <- trimws(parts[1])
  drugs <- tolower(trimws(strsplit(parts[2], ";")[[1]]))
  dict[[gen_name]] <- drugs
}

# 提取数据并打标签
df_trend <- M_clean %>%
  select(PY, DE) %>%
  mutate(PY = as.numeric(PY), DE = tolower(DE)) %>%
  filter(!is.na(DE))

for (gen in names(dict)) {
  pattern <- paste(dict[[gen]], collapse = "|")
  df_trend[[gen]] <- ifelse(str_detect(df_trend$DE, pattern), 1, 0)
}

# 按年汇总并绘图
df_trend_summary <- df_trend %>%
  group_by(PY) %>%
  summarise(across(all_of(names(dict)), sum)) %>%
  filter(PY >= 2000)

df_long <- df_trend_summary %>%
  pivot_longer(cols = -PY, names_to = "Generation", values_to = "Count")

p1 <- ggplot(df_long, aes(x = PY, y = Count, fill = Generation)) +
  geom_area(alpha = 0.85) + 
  scale_fill_manual(values = c(
    "G1_conventional" = "#8DA0CB", "G2_antiTNF" = "#66C2A5", 
    "G3_nonTNF" = "#FC8D62", "G4a_panJAK" = "#E78AC3", "G4b_selectiveJAK1" = "#E31A1C"
  )) +
  labs(title = "Generational Evolution of Immunosuppressants in UC",
       x = "Publication Year", y = "Number of Publications", fill = "Drug Generation") +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(family = "sans"), 
        plot.title = element_text(face="bold", hjust=0.5))

print(p1)
ggsave("1_Generation_Evolution_Stacked.pdf", plot = p1, width = 10, height = 6, dpi = 300)
ggsave("1_Generation_Evolution_Stacked.png", plot = p1, width = 10, height = 6, dpi = 300)


# ==============================================================================
# 第四步：绘制【经典图】发文量与累计引文双轴图
# ==============================================================================
cat("正在生成发文与累计引文双轴图...\n")
# 注意：双轴图使用原始 M 数据即可（引文总和不需要清洗药名）
df_cum <- M %>%
  select(PY, TC) %>%
  mutate(PY = as.numeric(PY), TC = as.numeric(TC)) %>%
  filter(!is.na(PY)) %>%
  group_by(PY) %>%
  summarise(Articles = n(), Yearly_Citations = sum(TC, na.rm = TRUE)) %>%
  filter(PY >= 2000) %>%
  mutate(Cumulative_Citations = cumsum(Yearly_Citations))

# 计算双轴系数
coeff_cum <- max(df_cum$Articles) / max(df_cum$Cumulative_Citations)

p2 <- ggplot(df_cum, aes(x = PY)) +
  geom_col(aes(y = Articles), fill = "#9EB4C9", width = 0.7) +
  geom_line(aes(y = Cumulative_Citations * coeff_cum), color = "#9E2A2B", linewidth = 1.2) +
  geom_point(aes(y = Cumulative_Citations * coeff_cum), color = "#9E2A2B", size = 1.5) +
  scale_y_continuous(
    name = "Articles", 
    sec.axis = sec_axis(~ . / coeff_cum, name = "Cumulative citations")
  ) +
  scale_x_continuous(breaks = seq(2000, max(df_cum$PY), by = 3)) +
  labs(x = "Year") +
  theme_classic() +
  theme(
    axis.title.y.left = element_text(color = "#9EB4C9", face = "bold", size = 12),
    axis.text.y.left = element_text(color = "#9EB4C9", face = "bold"),
    axis.line.y.left = element_line(color = "#9EB4C9", linewidth = 1),
    axis.ticks.y.left = element_line(color = "#9EB4C9"),
    axis.title.y.right = element_text(color = "#9E2A2B", face = "bold", size = 12, margin = margin(l = 10)),
    axis.text.y.right = element_text(color = "#9E2A2B", face = "bold"),
    axis.line.y.right = element_line(color = "#9E2A2B", linewidth = 1),
    axis.ticks.y.right = element_line(color = "#9E2A2B"),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

print(p2)
ggsave("2_Annual_Cumulative_DualAxis.pdf", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("2_Annual_Cumulative_DualAxis.png", plot = p2, width = 8, height = 6, dpi = 300)

# ==============================================================================
# 计算每年的百分比
df_pct <- df_long %>%
  group_by(PY) %>%
  mutate(Total = sum(Count),
         Percentage = (Count / Total) * 100)

# 画 100% 堆叠图
p_pct <- ggplot(df_pct, aes(x = PY, y = Percentage, fill = Generation)) +
  geom_area(alpha = 0.85, position = "fill") + 
  scale_fill_manual(values = c(
    "G1_conventional" = "#8DA0CB", "G2_antiTNF" = "#66C2A5", 
    "G3_nonTNF" = "#FC8D62", "G4a_panJAK" = "#E78AC3", "G4b_selectiveJAK1" = "#E31A1C"
  )) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relative Proportion of Drug Generations in UC (100% Stacked)",
       x = "Publication Year", y = "Percentage of Publications", fill = "Drug Generation") +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(family = "sans"), 
        plot.title = element_text(face="bold", hjust=0.5))

print(p_pct)
ggsave("3_Generation_Relative_Percent.pdf", plot = p_pct, width = 10, height = 6, dpi = 300)