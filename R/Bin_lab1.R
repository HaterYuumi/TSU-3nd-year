# Случайный объем выборки от 100 до 200
N <- sample(100:200, 1)
cat("Объем выборки N =", N, "\n\n")

# БИНОМИАЛЬНОЕ РАСПРЕДЕЛЕНИЕ

# Параметры
n_binom <- 25
p_binom <- 0.37

# 1 Генерация выборки
Bin <- rbinom(N, n_binom, p_binom)


#2 Полигон частот и эмпирическую функцию распределения

# Создаем частотную таблицу
freq_table <- table(Bin)
freq_rel <- prop.table(freq_table)  # относительные частоты

# Значения и частоты
x_vals <- as.numeric(names(freq_table))
y_vals <- as.numeric(freq_rel)

# ПОЛИГОН ЧАСТОТ
plot(x_vals, y_vals, 
     type = "b",  # точки и линии
     main = "Полигон относительных частот",
     xlab = "Значения",
     ylab = "Относительная частота",
     col = "blue",
     pch = 16,  # закрашенные кружки
     lwd = 2,   # толщина линии
     xlim = c(min(x_vals)-1, max(x_vals)+1),
     ylim = c(0, max(y_vals)*1.1))
grid()

# ЭМПИРИЧЕСКАЯ ФУНКЦИЯ РАСПРЕДЕЛЕНИЯ
# Сортируем значения
Bin_sorted <- sort(Bin)
n <- length(Bin_sorted)

# Эмпирическая функция распределения F_n(x) = (число элементов <= x) / n
F_emp <- ecdf(Bin)

# Построение графика эмпирической функции распределения
plot(F_emp, 
     main = "Эмпирическая функция распределения",
     xlab = "x",
     ylab = "F_n(x)",
     col = "red",
     lwd = 2,
     verticals = TRUE,  # вертикальные линии в точках скачков
     do.points = TRUE,  # показывать точки
     col.points = "blue",
     pch = 19)

# Добавляем сетку
grid()

# 3 коэффиценты асимметрии, коэффиценты эксцесса
Bin_mean <- mean(Bin) # выборочное среднее
Bin_var <- var(Bin) # дисперсия
Bin_sd <- sd(Bin) # СКО

# Функция нахождения моды
get_mode <- function(var) {
  uniqv <- unique(var)
  uniqv[which.max(tabulate(match(var, uniqv)))]
}

Bin_mode <- get_mode(Bin) # мода
Bin_median <- median(Bin) # медиана

# Расчет коэффициента асимметрии
n <- length(Bin)
Bin_skewness <- (sum((Bin - Bin_mean)^3) / n) / (Bin_sd^3)

# Расчет коэффициента эксцесса
Bin_kurtosis <- (sum((Bin - Bin_mean)^4) / n) / (Bin_sd^4)

cat("  Среднее:", Bin_mean, "\n")
cat("  Дисперсия:", Bin_var, "\n")
cat("  СКО:", Bin_sd, "\n")
cat("  Мода:", Bin_mode, "\n")
cat("  Медиана:", Bin_median, "\n")
cat("  Коэффициент асимметрии:", Bin_skewness, "\n")
cat("  Коэффициент эксцесса:", Bin_kurtosis, "\n\n")

# 4 Теоретические характеристики
MX_theor <- n_binom * p_binom
DX_theor <- n_binom * p_binom * (1 - p_binom)

cat("ТЕОРЕТИЧЕСКИЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Теоретическое мат. ожидание:", MX_theor, "\n")
cat("  Теоретическая дисперсия:", DX_theor, "\n")

# Сравнение точечных оценок с теоретическими характеристиками
cat("СРАВНЕНИЕ ОЦЕНОК:\n")
cat("  Мат. ожидание:\n")
cat("    Теоретическое:", MX_theor, "\n")
cat("    Выборочное:", Bin_mean, "\n")
cat("    Абсолютная погрешность:", abs(Bin_mean - MX_theor), "\n")
cat("    Относительная погрешность:", abs(Bin_mean - MX_theor) / MX_theor * 100, "%\n\n")

cat("  Дисперсия:\n")
cat("    Теоретическая:", DX_theor, "\n")
cat("    Выборочная:", Bin_var, "\n")
cat("    Абсолютная погрешность:", abs(Bin_var - DX_theor), "\n")
cat("    Относительная погрешность:", abs(Bin_var - DX_theor) / DX_theor * 100, "%\n\n")

# 5 Оценки параметров распределения (метод моментов)
# Метод моментов - наиболее простой способ оценить параметры распределения, приравнивая теоретические моменты к выборочным.
# 
#
observed <- table(Bin)
# Оценка вероятности p
p_est <- 1 - Bin_var / Bin_mean
cat("ОЦЕНКА ПАРАМЕТРА p (вероятность успеха):\n")
cat("  Метод моментов: p_hat =", p_est, "\n")
cat("  Истинное значение p:", p_binom, "\n")
cat("  Абсолютная погрешность:", abs(p_est - p_binom), "\n")
cat("  Относительная погрешность:", abs(p_est - p_binom) / p_binom * 100, "%\n\n")

# Оценка числа испытаний n
n_est <- Bin_mean / p_est
n_est_round <- round(n_est)  # округляем до целого, т.к. n должно быть целым

cat("ОЦЕНКА ПАРАМЕТРА n (число испытаний):\n")
cat("  Метод моментов (несокругленно): n_hat =", n_est, "\n")
cat("  Метод моментов (округленно): n_hat =", n_est_round, "\n")
cat("  Истинное значение n:", n_binom, "\n")
cat("  Абсолютная погрешность (округл.):", abs(n_est_round - n_binom), "\n")
cat("  Относительная погрешность (округл.):", abs(n_est_round - n_binom) / n_binom * 100, "%\n\n")

# 6 Проверка гипотезы распределения с помощью критерия X^2

# Теоретические частоты для биномиального распределения
# Получаем уникальные значения из выборки
x_unique <- as.numeric(names(observed))

# Вычисляем теоретические вероятности для каждого значения
theor_probs <- dbinom(x_unique, n_binom, p_binom)

# Теоретические частоты
expected <- theor_probs * N

# Объединяем группы с малыми частотами (меньше 5)
min_expected <- 5
while(any(expected < min_expected) & length(expected) > 1) {
  
  min_idx <- which.min(expected)
  
  if(min_idx == 1) {
    # Объединяем с соседней справа
    observed_grouped <- c(observed[1] + observed[2], observed[-(1:2)])
    expected_grouped <- c(expected[1] + expected[2], expected[-(1:2)])
  } else if(min_idx == length(expected)) {
    # Объединяем с соседней слева
    observed_grouped <- c(observed[-(length(observed):(length(observed)-1))], 
                          observed[length(observed)-1] + observed[length(observed)])
    expected_grouped <- c(expected[-(length(expected):(length(expected)-1))], 
                          expected[length(expected)-1] + expected[length(expected)])
  } else {
    # Объединяем с соседней справа
    observed_grouped <- c(observed[1:(min_idx-1)], 
                          observed[min_idx] + observed[min_idx+1], 
                          observed[(min_idx+2):length(observed)])
    expected_grouped <- c(expected[1:(min_idx-1)], 
                          expected[min_idx] + expected[min_idx+1], 
                          expected[(min_idx+2):length(expected)])
  }
  
  observed <- observed_grouped
  expected <- expected_grouped
  names(expected) <- names(observed)
}

cat("Группированные данные (ожидаемые частоты >= 5):\n")
cat("Наблюдаемые частоты:\n")
print(observed)
cat("\nОжидаемые частоты:\n")
print(expected)
cat("\n")

# Вычисляем статистику хи-квадрат
chi_sq_stat <- sum((observed - expected)^2 / expected)

# Число степеней свободы
k <- length(observed)  # число групп после объединения
# Для биномиального распределения мы оценивали 2 параметра (n и p)

df <- k - 1  # так как параметры известны (теоретические)

# Критическое значение для уровня значимости 0.05
critical_value <- qchisq(0.95, df)

# p-value
p_value <- 1 - pchisq(chi_sq_stat, df)

cat("РЕЗУЛЬТАТЫ КРИТЕРИЯ ХИ-КВАДРАТ:\n")
cat("  Статистика X^2 =", chi_sq_stat, "\n")
cat("  Число степеней свободы =", df, "\n")
cat("  Критическое значение (alph=0.05) =", critical_value, "\n")
cat("  p-value =", p_value, "\n\n")

# Вывод о принятии/отвержении гипотезы
if(chi_sq_stat <= critical_value) {
  cat("ВЫВОД: Статистика X^2 не превышает критическое значение.\n")
  cat("       Нет оснований отвергнуть гипотезу о том, что\n")
  cat("       выборка имеет биномиальное распределение с параметрами\n")
  cat("       n =", n_binom, "и p =", p_binom, "\n")
} else {
  cat("ВЫВОД: Статистика X^2 превышает критическое значение.\n")
  cat("       Гипотеза о том, что выборка имеет биномиальное распределение\n")
  cat("       с параметрами n =", n_binom, "и p =", p_binom, "отвергается\n")
}