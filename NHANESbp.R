library(haven)
library(tidyverse)
#lectura: 

demo <- read_xpt("DEMO_J.XPT")
dr1 <- read_xpt("DR1TOT_J.XPT")
bmx <- read_xpt("BMX_J.XPT")
dr2 <- read_xpt("DR2TOT_J.XPT")
dxx <- read_xpt("DXX_J.XPT")
bpx <- read_xpt("BPX_J.XPT")
hiq <- read_xpt("HIQ_J.XPT")
inq <- read_xpt("INQ_J.XPT")
alq <- read_xpt("ALQ_J.XPT")

#merge
merged_data <- merge(demo, dr1, by = "SEQN", )
merged_data <- merge(merged_data, bmx, by = "SEQN", )
merged_data <- merge(merged_data, dr2, by = "SEQN", )
merged_data <- merge(merged_data, bpx, by = "SEQN")
merged_data <- merge(merged_data, dxx, by = "SEQN", all = TRUE)
merged_data <- merge(merged_data, hiq, by = "SEQN")
merged_data <- merge(merged_data, inq, by = "SEQN")
merged_data <- merge(merged_data, alq, by = "SEQN",all = T)



dim(merged_data)
dim(selected_data)
#seleccionar campos de interes: 
selected_vars <- c("RIDAGEYR","SEQN", "RIDRETH3", "RIAGENDR", "RIDSTATR", "DMDEDUC3", "DMDEDUC2", 
                   "INDHHIN2", "DMDBORN4", "DR1DRSTZ","DR1TKCAL", "DRQSPREP", "DR1STY", "DR1_320Z", 
                   "DR1TALCO", "DR2TKCAL", "DR2TSUGR", "DR2STY", "DR2TALCO", "DR2_320Z", 
                   "BMXWT", "BPXSY1", "BPXDI1", "DXDTOPF", "HIQ011", "IND235", "ALQ111", 
                   "ALQ121")
selected_data <- merged_data[selected_vars]
write.csv(selected_data,file = 'ScorzaNhanesFormated.csv')

#cambiar manualmente algunos values numericos por sus valores reales 

df = read.csv('ScorzaNhanesReady.csv',header = T)

#Rename columns
library(dplyr)

df <- df %>%
  rename(
    
    seqn = SEQN,
    age = RIDAGEYR,
    race_ethnicity = RIDRETH3,
    gender = RIAGENDR,
    education_child = DMDEDUC3,
    education_adult = DMDEDUC2,
    household_income = INDHHIN2,
    country_birth = DMDBORN4,
    sodium_prep = DRQSPREP,
    sodium_table = DR1STY,
    water_intake = DR1_320Z,
    alcohol_intake = DR1TALCO,
    energy_intake = DR1TKCAL,
    energy_intake_dr2 = DR2TKCAL,
    sugar_intake = DR2TSUGR,
    sodium_table_dr2 = DR2STY,
    alcohol_intake_dr2 = DR2TALCO,
    water_intake_dr2 = DR2_320Z,
    weight = BMXWT,
    systolic_bp = BPXSY1,
    diastolic_bp = BPXDI1,
    total_percent_fat = DXDTOPF,
    health_insurance = HIQ011,
    monthly_income = IND235,
    alcohol_ever = ALQ111,
    alcohol_past_12mo = ALQ121
  )
view(df)

# menores a 8 no tienen data de la BP. 

df = df %>% filter(age >= 8)

#nos quedamos con quienes tienen datos de las dietas

df = df %>%  filter(DR1DRSTZ == 1)
dim(df)

#EDA 
install.packages("ggcorrplot")
library(tidyverse)
library(corrplot)

glimpse(df)





# Seleccionar las variables numéricas y categoricas
df_num <- df %>%
  select(
    age,
    water_intake,
    alcohol_intake,
    energy_intake,
    energy_intake_dr2,
    sugar_intake,
    alcohol_intake_dr2,
    water_intake_dr2,
    weight,
    systolic_bp,
    diastolic_bp,
    total_percent_fat
  )
df_cat = df %>% 
  select(
    race_ethnicity,
    gender,
    education_child,
    education_adult,
    household_income,
    country_birth,
    sodium_prep,
    sodium_table,
    sodium_table_dr2,
    health_insurance,
    monthly_income,
    alcohol_ever,
    alcohol_past_12mo
  )
# Cargar paquetes necesarios
library(ggcorrplot)
library(ggplot2)
library(janitor)

GGally::ggcorr(
  df_num, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)

cor_matrix = cor(df_num, method="spearman", use="pairwise")
cor_matrix[upper.tri(cor_matrix, diag=T)] = NA
df_cor = cor_matrix %>% as.table() %>% as.data.frame()
df_cor %>% 
  rename(corr = Freq) %>% 
  filter(!is.na(corr) & Var1 != Var2) %>% 
  arrange(-abs(corr)) %>% 
  head(20) %>% 
  knitr::kable() %>%
  kableExtra::kable_styling()

# Filtrar valores de presión diastólica iguales a cero o NA
df_filtered <- df %>% filter(!is.na(diastolic_bp) & diastolic_bp != 0)

# Gráfico de dispersión de presión arterial sistólica y diastólica
ggplot(df_filtered, aes(x = systolic_bp, y = diastolic_bp)) +
  geom_point(alpha = 0.4) +
  geom_vline(xintercept = 135, color = "red", linetype = "dashed")+
  geom_hline(yintercept = 85, color = "red", linetype = "dashed")+
  labs(x = "Presión arterial sistólica", y = "Presión arterial diastólica")

# Gráfico de densidad de la presión arterial sistólica
ggplot(df, aes(x = systolic_bp)) +
  geom_vline(xintercept = 135, color = "red", linetype = "dashed")+
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Presión arterial sistólica", y = "Densidad")+theme_minimal()

# Gráfico de densidad de la presión arterial diastólica
ggplot(df_filtered, aes(x = diastolic_bp)) +
  geom_vline(xintercept = 85, color = "red", linetype = "dashed")+
  geom_density(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "Presión arterial diastólica", y = "Densidad")+theme_minimal()




## Análisis de datos faltantes

#  Calculamos el % de datos faltantes por variable y por municipio:
  
map_dbl(df, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

head(df)

library(ggplot2)

p1 = ggplot(df, aes(x = "", y = weight)) +
  geom_boxplot(fill = "BLUE", color = "black") +
  ggtitle("PESO") +
  ylab("Kg") +
  theme_classic()

p2 = ggplot(df, aes(x = "", y = age)) +
  geom_boxplot(fill = "red", color = "black") +
  ggtitle("Edad") +
  ylab("Anos") +
  theme_classic()

ggplot(df, aes(x = "", y = total_percent_fat)) +
  geom_boxplot(fill = "green", color = "black") +
  ggtitle("Porcentaje de Grasa Corporal") +
  ylab("Porcentaje") +
  theme_classic()
# Une los gráficos en una única figura
gridExtra::grid.arrange(p1, p2,  ncol = 2)
head(df)
view(df)

library(tidyr)
df_clean <- drop_na(df, education_adult)

ggplot(df_clean, aes(x = education_adult, y = systolic_bp, fill = education_adult)) +
  geom_boxplot() +
  labs(x = NULL, y = "Presión arterial sistólica") +
  theme_minimal()






library(ggplot2)

# Gráfico de barras para race_ethnicity
ggplot(df, aes(x = race_ethnicity, fill= race_ethnicity )) + 
  geom_bar() +
  labs(title = "Distribución de raza/etnia",
       x = "Raza/etnia",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para gender
ggplot(df, aes(x = gender , fill = gender)) + 
  geom_bar() +
  labs(title = "Distribución de género",
       x = "Género",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para education_child
ggplot(df, aes(x = education_child, fill= education_child)) + 
  geom_bar() +
  labs(title = "Nivel educativo en la niñez",
       x = "Nivel educativo",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para education_adult
ggplot(df, aes(x = education_adult, fill = education_adult)) + 
  geom_bar(data = subset(df, !is.na(education_adult)), 
           na.rm = TRUE) +
  labs(title = "Nivel educativo Adultos",
       x = NULL, # Elimina etiqueta del eje x
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # Elimina etiquetas del eje x

#etnia
ggplot(df, aes(x = race_ethnicity, fill = race_ethnicity)) +
  geom_bar(data = subset(df, !is.na(race_ethnicity)),
           na.rm = TRUE) +
  labs(title = "Etnicidad de los participantes",
       x = NULL, # Elimina etiqueta del eje x
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # Elimina etiquetas del eje x


# Gráfico de torta para gender
ggplot(df, aes(x = "", fill = gender)) + 
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de género",
       fill = "Género") +
  scale_fill_manual(values = c("#87CEFA", "#FFC0CB"), 
                    labels = c("Male", "Female")) +
  theme_void() +
  theme(legend.position = c(1, 0.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))


#sodium
ggplot(df, aes(x = sodium_prep, fill = sodium_prep)) +
  geom_bar(data = subset(df, !is.na(sodium_prep)),
           na.rm = TRUE) +
  labs(title = "Uso de sal en la Cocina",
       x = NULL, # Elimina etiqueta del eje x
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # Elimina etiquetas del eje x

#alcohol
ggplot(df, aes(x = alcohol_ever, fill = alcohol_ever)) +
  geom_bar(data = subset(df, !is.na(alcohol_ever)),
           na.rm = TRUE) +
  labs(title = "Consume Alcohol",
       x = NULL, # Elimina etiqueta del eje x
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) # Elimina etiquetas del eje x

# Gráfico de torta para health_insurance
ggplot(df, aes(x = "", fill = health_insurance)) + 
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Tiene Seguro de salud",
       fill = "Seguro de salud") +
  scale_fill_manual(values = c("yellow","#C23B22", 'grey', "#03C03C"),
                    labels = c("Dont Know","No",'NA',  "Yes")) +
  theme_void() +
  theme(legend.position = c(1.1, 0.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Gráfico de torta para country_birth
ggplot(df, aes(x = "", fill = country_birth)) + 
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de país de nacimiento",
       fill = "País de nacimiento") +
  scale_fill_manual(values = c("#87CEFA", "#FFC0CB"),
                    labels = c("US", "Others"  )) +
  theme_void() +
  theme(legend.position = c(0.9, 0.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))


df$sodium_prep
view(df)

library(ggplot2)

ggplot(na.omit(df), aes(x=education_adult, fill=education_adult)) +
  geom_density(alpha=0.5) +
  labs(x = "Nivel educativo del sujeto como adulto", y = "Densidad") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

df %>%  select(sodium_prep, household_income)



library(ggplot2)

ggplot(df, aes(x = household_income)) +
  geom_dotplot(fill = "#87CEFA", binaxis = "y", dotsize = 3) +
  labs(title = "Frecuencia de ingresos", x = "Ingreso", y = "Frecuencia")


ggplot(df, aes(x = sodium_prep)) +
  geom_dotplot(fill = "#FFC0CB", binaxis = "y", dotsize = 0.7) +
  labs(title = "Frecuencia de preparación con sodio", x = "Preparación", y = "Frecuencia")


library(vcd)

mosaic(df, shade = TRUE, legend = TRUE, main = "Frecuencia de ingresos", xlab = "Ingreso", ylab = "Frecuencia", highlighting = "household_income", highlighting_fill = "#87CEFA")


# Cantidad de valores únicos en cada variable categórica
cat_vars <- c("household_income", "sodium_prep")
for (var in cat_vars) {
  print(paste("Variable", var, ":", length(unique(df[[var]]))))
  print(table(df[[var]])/nrow(df)*100)
}






df$education_adult <- factor(df$education_adult, levels = c(1,2,3,4,5,7,9),
                             labels = c("Less than 9th grade", "9-11th grade (Includes 12th grade with no diploma)", 
                                        "High school graduate/GED or equivalent", "Some college or AA degree", 
                                        "College graduate or above", "Refused", "Don't Know"))

df$education_child <- factor(df$education_child, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,55,66,77,99),
                             labels = c("Never attended / kindergarten only", "1st grade", "2nd grade", "3rd grade", 
                                        "4th grade", "5th grade", "6th grade", "7th grade", "8th grade", 
                                        "9th grade", "10th grade", "11th grade", "12th grade, no diploma", 
                                        "High school graduate", "GED or equivalent", "More than high school", 
                                        "Less than 5th grade", "Less than 9th grade", "Refused", "Don't Know"))
df$household_income <- factor(df$household_income, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 77, 99),
                              labels = c("$ 0 to $ 4,999", "$ 5,000 to $ 9,999", "$10,000 to $14,999", 
                                         "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $34,999", 
                                         "$35,000 to $44,999", "$45,000 to $54,999", "$55,000 to $64,999",
                                         "$65,000 to $74,999", "$20,000 and Over", "Under $20,000", 
                                         "$75,000 to $99,999", "$100,000 and Over", "Refused", "Don't know"))


#NUMERICAS

summary(df$age)
summary(df[c("age","total_percent_fat", "water_intake", "alcohol_intake", "energy_intake", "sugar_intake", "weight")])


# Diagrama de caja de la variable "water_intake" comparando por "gender"
ggplot(df, aes(x = gender, y = water_intake, fill = gender)) +
  geom_boxplot() +
  labs(title = "Comparación de consumo de agua por género",
       x = "Género",
       y = "Consumo de agua (ml)") +
  theme_minimal()
# Histograma de la variable "age"
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "#69b3a2") +
  labs(title = "Distribución de Edades",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de dispersión de "weight" vs. "energy_intake" con ajuste lineal
ggplot(df, aes(x = weight, y = energy_intake)) +
  geom_point(color = "#69b3a2", alpha = 0.5) +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(title = "Relación entre Peso y Kcal Consumidas",
       x = "Peso (kg)",
       y = "Consumo de energía (kcal)") +
  theme_minimal()

#presion vs peso
ggplot(df, aes(x = weight, y = systolic_bp)) +
  geom_point(color = "#C23B22",alpha = 0.8) +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(title = "Relación entre Peso y Presión Sistólica",
       x = "Peso (kg)",
       y = "Presión Sistólica (mmHg)") +
  theme_minimal()


#presion vs porcentaje grasa corporal

# Gráfico de dispersión de "total_percent_fat" vs. "systolic_blood_pressure" con ajuste lineal
ggplot(df, aes(x = total_percent_fat, y = systolic_bp)) +
  geom_point(color = "#69b3a2") +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(title = "Relación entre Porcentaje de Grasa Corporal y Presión Sistólica",
       x = "Porcentaje de Grasa Corporal",
       y = "Presión Sistólica (mmHg)") +
  theme_minimal()

ggplot(df, aes(x = energy_intake)) +
  geom_density(fill = "#69b3a2", alpha = 0.6) +
  labs(title = "Densidad de Consumo de Energía",
       x = "Consumo de energía (kcal)",
       y = "Densidad") +
  theme_minimal() +
  scale_y_continuous(labels = NULL)

#ANOVA

# Eliminar filas con valores NA en la variable education_adult
dfan <- df %>% filter(!is.na(education_adult))

# ANOVA de una vía para education_adult
mod_edu <- aov(systolic_bp ~ education_adult, data = dfan)
summary(mod_edu)

# ANOVA de una vía para gender
mod_gender <- aov(systolic_bp ~ gender, data = dfan)
summary(mod_gender)

# ANOVA de una vía para race_ethnicity
mod_race <- aov(systolic_bp ~ race_ethnicity, data = dfan)
summary(mod_race)

# ANOVA de una vía para sodium_prep
mod_sodium <- aov(systolic_bp ~ sodium_prep, data = dfan)
summary(mod_sodium)
install.packages('rsq')
library(rsq)



ggplot(drop_na(df, race_ethnicity), aes(x = race_ethnicity, y = systolic_bp, fill = race_ethnicity)) +
  geom_boxplot() +
  labs(x = NULL, y = "Presión arterial sistólica") +
  ggtitle("Relación entre la race_ethnicity y la presión arterial sistólica") +
  theme_minimal()



