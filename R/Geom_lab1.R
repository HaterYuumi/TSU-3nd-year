# Случайный объем выборки от 100 до 200
N <- sample(100:200, 1)
cat("Объем выборки N =", N, "\n\n")

# ГЕОМЕТРИЧЕСКОЕ РАСПРЕДЕЛЕНИЕ

# Параметры
p_geom <- 0.3  # вероятность успеха

# 1 Генерация выборки (число испытаний до первого успеха)
Geom <- rgeom(N, p_geom)

# 2 Полигон частот и эмпирическую функцию распределения

# Создаем частотную таблицу
freq_table <- table(Geom)
freq_rel <- prop.table(freq_table)

# Значения и частоты
x_vals <- as.numeric(names(freq_table))
y_vals <- as.numeric(freq_rel)

# ПОЛИГОН ЧАСТОТ
plot(x_vals, y_vals, 
     type = "b",
     main = "Полигон относительных частот (геометрическое распределение)",
     xlab = "Значения",
     ylab = "Относительная частота",
     col = "blue",
     pch = 16,
     lwd = 2,
     xlim = c(min(x_vals)-1, max(x_vals)+1),
     ylim = c(0, max(y_vals)*1.1))
grid()

# ЭМПИРИЧЕСКАЯ ФУНКЦИЯ РАСПРЕДЕЛЕНИЯ
F_emp <- ecdf(Geom)

plot(F_emp, 
     main = "Эмпирическая функция распределения (геометрическое)",
     xlab = "x",
     ylab = "F_n(x)",
     col = "red",
     lwd = 2,
     verticals = TRUE,
     do.points = TRUE,
     col.points = "blue",
     pch = 19)
grid()

# 3 Коэффициенты асимметрии, коэффициенты эксцесса
Geom_mean <- mean(Geom)
Geom_var <- var(Geom)
Geom_sd <- sd(Geom)

# Функция нахождения моды
get_mode <- function(var) {
  uniqv <- unique(var)
  uniqv[which.max(tabulate(match(var, uniqv)))]
}

Geom_mode <- get_mode(Geom)
Geom_median <- median(Geom)

# Расчет коэффициента асимметрии
n <- length(Geom)
Geom_skewness <- (sum((Geom - Geom_mean)^3) / n) / (Geom_sd^3)

# Расчет коэффициента эксцесса
Geom_kurtosis <- (sum((Geom - Geom_mean)^4) / n) / (Geom_sd^4)

cat("ВЫБОРОЧНЫЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Среднее:", Geom_mean, "\n")
cat("  Дисперсия:", Geom_var, "\n")
cat("  СКО:", Geom_sd, "\n")
cat("  Мода:", Geom_mode, "\n")
cat("  Медиана:", Geom_median, "\n")
cat("  Коэффициент асимметрии:", Geom_skewness, "\n")
cat("  Коэффициент эксцесса:", Geom_kurtosis, "\n\n")

# 4 Теоретические характеристики
MX_theor <- (1 - p_geom) / p_geom
DX_theor <- (1 - p_geom) / p_geom^2

cat("ТЕОРЕТИЧЕСКИЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Теоретическое мат. ожидание:", MX_theor, "\n")
cat("  Теоретическая дисперсия:", DX_theor, "\n\n")

# Сравнение точечных оценок с теоретическими характеристиками
cat("СРАВНЕНИЕ ОЦЕНОК:\n")
cat("  Мат. ожидание:\n")
cat("    Теоретическое:", MX_theor, "\n")
cat("    Выборочное:", Geom_mean, "\n")
cat("    Абсолютная погрешность:", abs(Geom_mean - MX_theor), "\n")
cat("    Относительная погрешность:", abs(Geom_mean - MX_theor) / MX_theor * 100, "%\n\n")

cat("  Дисперсия:\n")
cat("    Теоретическая:", DX_theor, "\n")
cat("    Выборочная:", Geom_var, "\n")
cat("    Абсолютная погрешность:", abs(Geom_var - DX_theor), "\n")
cat("    Относительная погрешность:", abs(Geom_var - DX_theor) / DX_theor * 100, "%\n\n")

# 5 Оценка параметра распределения (метод моментов)
p_est <- 1 / (Geom_mean + 1)

cat("ОЦЕНКА ПАРАМЕТРА p (вероятность успеха):\n")
cat("  Метод моментов: p_hat =", p_est, "\n")
cat("  Истинное значение p:", p_geom, "\n")
cat("  Абсолютная погрешность:", abs(p_est - p_geom), "\n")
cat("  Относительная погрешность:", abs(p_est - p_geom) / p_geom * 100, "%\n\n")

# 6 Проверка гипотезы распределения с помощью критерия X^2
observed <- table(Geom)
x_unique <- as.numeric(names(observed))

# Теоретические вероятности
theor_probs <- dgeom(x_unique, p_geom)

# Теоретические частоты
expected <- theor_probs * N

# Выводим информацию о начальных данных
cat("Начальные данные:\n")
cat("Количество уникальных значений:", length(observed), "\n")
cat("Минимальная ожидаемая частота:", min(expected), "\n\n")

# Объединяем группы с малыми частотами
min_expected <- 5

# Проверяем, есть ли NA в данных
if(any(is.na(expected))) {
  # Удаляем NA из данных
  valid_indices <- !is.na(expected)
  observed <- observed[valid_indices]
  expected <- expected[valid_indices]
}

# Объединяем группы с малыми частотами
while(length(expected) > 1 && any(expected < min_expected, na.rm = TRUE)) {
  # Находим индекс с минимальной ожидаемой частотой
  min_idx <- which.min(expected)
  
  if(min_idx == 1) {
    # Объединяем первый и второй элементы
    observed <- c(observed[1] + observed[2], observed[-(1:2)])
    expected <- c(expected[1] + expected[2], expected[-(1:2)])
  } else if(min_idx == length(expected)) {
    # Объединяем последний и предпоследний элементы
    new_observed_last <- observed[length(observed)-1] + observed[length(observed)]
    new_expected_last <- expected[length(expected)-1] + expected[length(expected)]
    
    observed <- c(observed[1:(length(observed)-2)], new_observed_last)
    expected <- c(expected[1:(length(expected)-2)], new_expected_last)
  } else {
    # Объединяем элемент с минимальной частотой со следующим
    observed <- c(observed[1:(min_idx-1)], 
                  observed[min_idx] + observed[min_idx+1], 
                  observed[(min_idx+2):length(observed)])
    expected <- c(expected[1:(min_idx-1)], 
                  expected[min_idx] + expected[min_idx+1], 
                  expected[(min_idx+2):length(expected)])
  }
  
  # Даем имена элементам
  names(expected) <- paste0("group", 1:length(expected))
  names(observed) <- names(expected)
  
  cat("Промежуточный результат: количество групп =", length(expected), "\n")
}

cat("\nГруппированные данные (ожидаемые частоты >= 5):\n")
cat("Наблюдаемые частоты:\n")
print(observed)
cat("\nОжидаемые частоты:\n")
print(expected)
cat("\n")

# Проверяем, что все ожидаемые частоты положительные и не NA (пропущенные значения)
if(any(is.na(expected)) || any(expected <= 0)) {
  # Фильтруем проблемные значения
  valid_indices <- !is.na(expected) & expected > 0
  observed <- observed[valid_indices]
  expected <- expected[valid_indices]
  
  cat("После фильтрации:\n")
  cat("Наблюдаемые частоты:\n")
  print(observed)
  cat("\nОжидаемые частоты:\n")
  print(expected)
  cat("\n")
}

# Вычисляем статистику хи-квадрат
chi_sq_stat <- sum((observed - expected)^2 / expected)

# Число степеней свободы
k <- length(observed)
df <- k - 1  # так как параметр известен (теоретический)

# Критическое значение для уровня значимости 0.05
critical_value <- qchisq(0.95, df)
p_value <- 1 - pchisq(chi_sq_stat, df)

cat("РЕЗУЛЬТАТЫ КРИТЕРИЯ ХИ-КВАДРАТ:\n")
cat("  Статистика X^2 =", chi_sq_stat, "\n")
cat("  Число степеней свободы =", df, "\n")
cat("  Критическое значение (alpha=0.05) =", critical_value, "\n")
cat("  p-value =", p_value, "\n\n")

if(chi_sq_stat <= critical_value) {
  cat("ВЫВОД: Нет оснований отвергнуть гипотезу о том, что\n")
  cat("       выборка имеет геометрическое распределение с параметром p =", p_geom, "\n")
} else {
  cat("ВЫВОД: Гипотеза о том, что выборка имеет геометрическое распределение\n")
  cat("       с параметром p =", p_geom, "отвергается\n")
}