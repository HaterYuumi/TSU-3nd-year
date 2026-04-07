# Лабораторная работа №5
# Критерии сравнения групп и анализ таблиц сопряженности

# ==================== Задание 1: Анализ пульса ====================

# Загрузка данных
pulse_data <- read.table("pulse.txt", header = TRUE)

# Просмотр структуры данных
str(pulse_data)
head(pulse_data)

# 1. Проверка на нормальность

# Создаем список выборок
groups <- list(
  СВ = pulse_data$СВ,
  ЕВ = pulse_data$ЕВ,
  СА = pulse_data$СА,
  ЕА = pulse_data$ЕА
)

# Функция для проверки нормальности
check_normality <- function(data, name) {
  cat("\n========== Проверка нормальности для", name, "==========\n")
  
  # Графическая проверка
  par(mfrow = c(1, 2))
  # Гистограмма с кривой плотности
  hist(data, main = paste("Гистограмма:", name), 
       xlab = "Значения", col = "lightblue", probability = TRUE)
  curve(dnorm(x, mean(data), sd(data)), add = TRUE, col = "red", lwd = 2)
  
  # Q-Q plot
  qqnorm(data, main = paste("Q-Q plot:", name))
  qqline(data, col = "red", lwd = 2)
  
  # Статистический критерий (Шапиро-Уилка)
  shapiro_test <- shapiro.test(data)
  cat("Тест Шапиро-Уилка:\n")
  cat("  W =", shapiro_test$statistic, "\n")
  cat("  p-value =", shapiro_test$p.value, "\n")
  
  if (shapiro_test$p.value > 0.05) {
    cat("  Вывод: распределение не отличается от нормального (p > 0.05)\n")
  } else {
    cat("  Вывод: распределение отличается от нормального (p < 0.05)\n")
  }
  
  par(mfrow = c(1, 1))
}

# Проверяем каждую выборку
for (name in names(groups)) {
  check_normality(groups[[name]], name)
}

# 2. Сравнение "до" и "после" для каждой группы испытуемых

cat("\n========== Сравнение 'до' и 'после' ==========\n")

# Функция для сравнения двух зависимых выборок
compare_paired <- function(before, after, group_name) {
  cat("\n--- Группа:", group_name, "---\n")
  cat("Среднее до:", round(mean(before), 2), "\n")
  cat("Среднее после:", round(mean(after), 2), "\n")
  cat("Разница:", round(mean(after) - mean(before), 2), "\n")
  
  # Проверка нормальности разностей
  diff <- after - before
  shapiro_diff <- shapiro.test(diff)
  cat("Тест Шапиро-Уилка для разностей: p-value =", 
      round(shapiro_diff$p.value, 4), "\n")
  
  # Выбор критерия
  if (shapiro_diff$p.value > 0.05) {
    # Парный t-тест
    test_result <- t.test(before, after, paired = TRUE)
    cat("Используется: парный t-тест\n")
  } else {
    # Критерий Уилкоксона
    test_result <- wilcox.test(before, after, paired = TRUE)
    cat("Используется: критерий Уилкоксона\n")
  }
  
  cat("Статистика:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n")
  
  if (test_result$p.value < 0.05) {
    cat("Вывод: различия статистически значимы (p < 0.05)\n")
  } else {
    cat("Вывод: различия статистически не значимы (p > 0.05)\n")
  }
}

# Сравнение для пациентов (СВ vs СА)
compare_paired(pulse_data$СВ, pulse_data$СА, "Пациенты (СВ и СА)")

# Сравнение для здоровых (ЕВ vs ЕА)
compare_paired(pulse_data$ЕВ, pulse_data$ЕА, "Здоровые (ЕВ и ЕА)")

# Построение boxplot для "до" и "после"
par(mfrow = c(1, 2))

# Для пациентов
boxplot(pulse_data$СВ, pulse_data$СА,
        names = c("До (СВ)", "После (СА)"),
        main = "Пациенты: до и после",
        ylab = "Пульс",
        col = c("lightblue", "lightgreen"))

# Для здоровых
boxplot(pulse_data$ЕВ, pulse_data$ЕА,
        names = c("До (ЕВ)", "После (ЕА)"),
        main = "Здоровые: до и после",
        ylab = "Пульс",
        col = c("lightblue", "lightgreen"))

par(mfrow = c(1, 1))

# 3. Сравнение "здоровых" и "пациентов" для случаев "до" и "после"

cat("\n========== Сравнение 'здоровых' и 'пациентов' ==========\n")

# Функция для сравнения двух независимых выборок
compare_independent <- function(group1, group2, name1, name2, condition) {
  cat("\n--- Условие:", condition, "---\n")
  cat("Среднее", name1, ":", round(mean(group1), 2), "\n")
  cat("Среднее", name2, ":", round(mean(group2), 2), "\n")
  
  # Проверка нормальности
  shapiro1 <- shapiro.test(group1)
  shapiro2 <- shapiro.test(group2)
  
  # Критерий Левена для проверки равенства дисперсий
  # Создаем данные для теста Левена
  combined <- c(group1, group2)
  groups_factor <- factor(c(rep(name1, length(group1)), 
                            rep(name2, length(group2))))
  
  levene_test <- leveneTest(combined ~ groups_factor)
  cat("Тест Левена для равенства дисперсий: p-value =", 
      round(levene_test$`Pr(>F)`[1], 4), "\n")
  
  # Выбор критерия
  if (shapiro1$p.value > 0.05 && shapiro2$p.value > 0.05) {
    # t-тест
    var_equal <- levene_test$`Pr(>F)`[1] > 0.05
    test_result <- t.test(group1, group2, var.equal = var_equal)
    cat("Используется: t-тест (var.equal =", var_equal, ")\n")
  } else {
    # Критерий Манна-Уитни
    test_result <- wilcox.test(group1, group2)
    cat("Используется: критерий Манна-Уитни\n")
  }
  
  cat("Статистика:", test_result$statistic, "\n")
  cat("p-value:", test_result$p.value, "\n")
  
  if (test_result$p.value < 0.05) {
    cat("Вывод: различия статистически значимы (p < 0.05)\n")
  } else {
    cat("Вывод: различия статистически не значимы (p > 0.05)\n")
  }
}

# Подключаем библиотеку для теста Левена
library(car)

# Сравнение "до" (СВ vs ЕВ)
compare_independent(pulse_data$СВ, pulse_data$ЕВ, 
                    "Пациенты", "Здоровые", "До применения (СВ vs ЕВ)")

# Сравнение "после" (СА vs ЕА)
compare_independent(pulse_data$СА, pulse_data$ЕА, 
                    "Пациенты", "Здоровые", "После применения (СА vs ЕА)")

# Построение boxplot для сравнения групп
par(mfrow = c(1, 2))

# Сравнение "до"
boxplot(pulse_data$СВ, pulse_data$ЕВ,
        names = c("Пациенты (СВ)", "Здоровые (ЕВ)"),
        main = "До применения: пациенты vs здоровые",
        ylab = "Пульс",
        col = c("lightcoral", "lightblue"))

# Сравнение "после"
boxplot(pulse_data$СА, pulse_data$ЕА,
        names = c("Пациенты (СА)", "Здоровые (ЕА)"),
        main = "После применения: пациенты vs здоровые",
        ylab = "Пульс",
        col = c("lightcoral", "lightblue"))

par(mfrow = c(1, 1))

# 4. Выводы об эффективности лекарственного средства

cat("\n========== ВЫВОДЫ ОБ ЭФФЕКТИВНОСТИ ЛЕКАРСТВЕННОГО СРЕДСТВА ==========\n")

# Расчет статистик для выводов
cat("\nСтатистические показатели:\n")
cat("Пациенты: средний пульс до =", round(mean(pulse_data$СВ), 2), 
    "после =", round(mean(pulse_data$СА), 2), 
    "снижение =", round(mean(pulse_data$СВ) - mean(pulse_data$СА), 2), "\n")
cat("Здоровые: средний пульс до =", round(mean(pulse_data$ЕВ), 2), 
    "после =", round(mean(pulse_data$ЕА), 2), 
    "изменение =", round(mean(pulse_data$ЕА) - mean(pulse_data$ЕВ), 2), "\n")

# Проверка значимости различий между пациентами и здоровыми после лечения
cat("\nСравнение пациентов и здоровых после лечения:\n")
after_compare <- wilcox.test(pulse_data$СА, pulse_data$ЕА)
cat("p-value =", after_compare$p.value, "\n")

# Выводы
cat("\n--- ВЫВОДЫ ---\n")
cat("1. В группе пациентов наблюдается", 
    ifelse(mean(pulse_data$СВ) > mean(pulse_data$СА), 
           "снижение", "повышение"), 
    "пульса после применения лекарства.\n")

cat("2. В группе здоровых значительных изменений пульса не наблюдается.\n")

cat("3. После лечения показатели пульса пациентов",
    ifelse(after_compare$p.value > 0.05, 
           "не отличаются статистически значимо", "отличаются"),
    "от показателей здоровых.\n")

cat("4. Лекарственное средство эффективно: оно снижает пульс у пациентов",
    ifelse(after_compare$p.value > 0.05,
           "до уровня здоровых людей.", "но не достигает нормальных показателей."))


# ==================== Задание 2: Анализ таблицы сопряженности ====================

cat("\n\n========== ЗАДАНИЕ 2: Анализ таблицы сопряженности ==========\n")

# Загрузка данных
grades_data <- read.table("grades.txt", header = TRUE)

# Просмотр структуры данных
str(grades_data)
head(grades_data)

# 1. Составление таблицы сопряженности
# Предполагается, что в данных есть столбцы "Группа" и "Оценка"
# Если названия столбцов отличаются, нужно их скорректировать

# Проверяем названия столбцов
cat("\nНазвания столбцов:", colnames(grades_data), "\n")

# Создаем таблицу сопряженности
# Предположим, что первый столбец - группа, второй - оценка
# Если структура другая, скорректируйте индексы
group_col <- 1  # номер столбца с группой
grade_col <- 2  # номер столбца с оценкой

contingency_table <- table(grades_data[, group_col], grades_data[, grade_col])
cat("\nТаблица сопряженности:\n")
print(contingency_table)

# Визуализация таблицы сопряженности
# Мозаичный график
mosaicplot(contingency_table, 
           main = "Мозаичный график: Группа vs Оценка",
           xlab = "Группа", 
           ylab = "Оценка",
           color = c("lightblue", "lightgreen", "lightcoral", "lightyellow"))

# 2. Проверка гипотезы об отсутствии связи

# Функция для выбора критерия
analyze_contingency <- function(table_data) {
  cat("\n========== Анализ таблицы сопряженности ==========\n")
  cat("Размер таблицы:", dim(table_data), "\n")
  
  # Ожидаемые частоты
  expected <- chisq.test(table_data)$expected
  cat("\nОжидаемые частоты:\n")
  print(round(expected, 2))
  
  # Проверка условий применимости критерия Хи-квадрат
  min_expected <- min(expected)
  percent_less_5 <- sum(expected < 5) / length(expected) * 100
  
  cat("\nМинимальная ожидаемая частота:", round(min_expected, 2), "\n")
  cat("Доля ячеек с ожидаемой частотой < 5:", round(percent_less_5, 1), "%\n")
  
  # Выбор критерия
  if (min_expected >= 5 && percent_less_5 == 0) {
    cat("\nУсловия для критерия Хи-квадрат выполнены.\n")
    test_result <- chisq.test(table_data)
    cat("\n=== Критерий Хи-квадрат ===\n")
  } else {
    cat("\nУсловия для критерия Хи-квадрат не выполнены.\n")
    cat("Используем точный критерий Фишера.\n")
    test_result <- fisher.test(table_data, simulate.p.value = TRUE)
    cat("\n=== Точный критерий Фишера (с симуляцией) ===\n")
  }
  
  cat("X-squared / Statistic:", test_result$statistic, "\n")
  cat("df:", ifelse(!is.null(test_result$parameter), test_result$parameter, "N/A"), "\n")
  cat("p-value:", test_result$p.value, "\n")
  
  # Вывод
  cat("\n--- ВЫВОД ---\n")
  if (test_result$p.value < 0.05) {
    cat("Отвергаем нулевую гипотезу об отсутствии связи.\n")
    cat("Между признаками 'Группа' и 'Оценка' существует статистически значимая связь (p < 0.05).\n")
    
    # Анализ стандартизированных остатков для выявления значимых ячеек
    if (min_expected >= 5) {
      residuals <- chisq.test(table_data)$stdres
      cat("\nСтандартизированные остатки (|остаток| > 1.96 указывает на значимую ячейку):\n")
      print(round(residuals, 2))
    }
  } else {
    cat("Нет оснований отвергнуть нулевую гипотезу.\n")
    cat("Статистически значимая связь между признаками 'Группа' и 'Оценка' не обнаружена (p > 0.05).\n")
  }
  
  return(test_result)
}

# Анализируем таблицу сопряженности
result <- analyze_contingency(contingency_table)

# Дополнительная визуализация
# Диаграмма с группировкой
library(ggplot2)

# Преобразуем данные для ggplot
df_plot <- as.data.frame(contingency_table)
colnames(df_plot) <- c("Группа", "Оценка", "Частота")

# Столбчатая диаграмма
ggplot(df_plot, aes(x = Группа, y = Частота, fill = Оценка)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Распределение оценок по группам",
       x = "Группа", y = "Частота") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Для более наглядного сравнения - процентная диаграмма
ggplot(df_plot, aes(x = Группа, y = Частота, fill = Оценка)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Относительное распределение оценок по группам",
       x = "Группа", y = "Доля") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent)