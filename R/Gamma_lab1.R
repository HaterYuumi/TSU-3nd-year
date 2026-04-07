# Случайный объем выборки от 100 до 200
N <- sample(100:200, 1)
cat("Объем выборки N =", N, "\n\n")

# ГАММА-РАСПРЕДЕЛЕНИЕ

# Параметры
shape_gamma <- 2    # параметр формы (k)
rate_gamma <- 0.5   # параметр интенсивности (theta = 1/rate)

# 1 Генерация выборки
Gamma <- rgamma(N, shape = shape_gamma, rate = rate_gamma)

# 2 Гистограмма и эмпирическая функция распределения

# ГИСТОГРАММА
hist(Gamma, 
     breaks = "FD",
     main = "Гистограмма относительных частот (гамма-распределение)",
     xlab = "Значения",
     ylab = "Плотность",
     col = "lightgreen",
     border = "black",
     freq = FALSE,
     probability = TRUE)
# Теоретическая плотность
curve(dgamma(x, shape = shape_gamma, rate = rate_gamma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
legend("topright", legend = c("Эмпирическая", "Теоретическая"), 
       fill = c("lightgreen", NA), col = c(NA, "red"), lwd = c(NA, 2))
grid()

# ЭМПИРИЧЕСКАЯ ФУНКЦИЯ РАСПРЕДЕЛЕНИЯ
F_emp <- ecdf(Gamma)

plot(F_emp, 
     main = "Эмпирическая функция распределения (гамма-распределение)",
     xlab = "x",
     ylab = "F_n(x)",
     col = "blue",
     lwd = 2,
     do.points = FALSE,
     verticals = TRUE)
# теоретическая функция распределения
curve(pgamma(x, shape = shape_gamma, rate = rate_gamma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
legend("bottomright", legend = c("Эмпирическая", "Теоретическая"), 
       col = c("blue", "red"), lwd = 2)
grid()

# 3 Коэффициенты асимметрии, коэффициенты эксцесса
Gamma_mean <- mean(Gamma)
Gamma_var <- var(Gamma)
Gamma_sd <- sd(Gamma)
Gamma_median <- median(Gamma)

# Расчет коэффициента асимметрии
n <- length(Gamma)
Gamma_skewness <- (sum((Gamma - Gamma_mean)^3) / n) / (Gamma_sd^3)

# Расчет коэффициента эксцесса
Gamma_kurtosis <- (sum((Gamma - Gamma_mean)^4) / n) / (Gamma_sd^4)

cat("ВЫБОРОЧНЫЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Среднее:", Gamma_mean, "\n")
cat("  Дисперсия:", Gamma_var, "\n")
cat("  СКО:", Gamma_sd, "\n")
cat("  Медиана:", Gamma_median, "\n")
cat("  Коэффициент асимметрии:", Gamma_skewness, "\n")
cat("  Коэффициент эксцесса:", Gamma_kurtosis, "\n\n")

# 4 Теоретические характеристики
MX_theor <- shape_gamma / rate_gamma
DX_theor <- shape_gamma / rate_gamma^2
Skewness_theor <- 2 / sqrt(shape_gamma)
Kurtosis_theor <- 3 + 6 / shape_gamma

cat("ТЕОРЕТИЧЕСКИЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Теоретическое мат. ожидание:", MX_theor, "\n")
cat("  Теоретическая дисперсия:", DX_theor, "\n")
cat("  Теоретический коэффициент асимметрии:", Skewness_theor, "\n")
cat("  Теоретический коэффициент эксцесса:", Kurtosis_theor, "\n\n")

# Сравнение точечных оценок с теоретическими характеристиками
cat("СРАВНЕНИЕ ОЦЕНОК:\n")
cat("  Мат. ожидание:\n")
cat("    Теоретическое:", MX_theor, "\n")
cat("    Выборочное:", Gamma_mean, "\n")
cat("    Абсолютная погрешность:", abs(Gamma_mean - MX_theor), "\n")
cat("    Относительная погрешность:", abs(Gamma_mean - MX_theor) / MX_theor * 100, "%\n\n")

cat("  Дисперсия:\n")
cat("    Теоретическая:", DX_theor, "\n")
cat("    Выборочная:", Gamma_var, "\n")
cat("    Абсолютная погрешность:", abs(Gamma_var - DX_theor), "\n")
cat("    Относительная погрешность:", abs(Gamma_var - DX_theor) / DX_theor * 100, "%\n\n")

# 5 Оценка параметров распределения (метод моментов)
# Для гамма-распределения: E(X) = shape/rate, Var(X) = shape/rate^2
# Система:
# shape_est / rate_est = Gamma_mean
# shape_est / rate_est^2 = Gamma_var

rate_est <- Gamma_mean / Gamma_var
shape_est <- Gamma_mean * rate_est

cat("ОЦЕНКА ПАРАМЕТРОВ (метод моментов):\n")
cat("  Оценка shape (k):", shape_est, "\n")
cat("  Истинное значение shape:", shape_gamma, "\n")
cat("  Абсолютная погрешность:", abs(shape_est - shape_gamma), "\n")
cat("  Относительная погрешность:", abs(shape_est - shape_gamma) / shape_gamma * 100, "%\n\n")

cat("  Оценка rate (lambda):", rate_est, "\n")
cat("  Истинное значение rate:", rate_gamma, "\n")
cat("  Абсолютная погрешность:", abs(rate_est - rate_gamma), "\n")
cat("  Относительная погрешность:", abs(rate_est - rate_gamma) / rate_gamma * 100, "%\n\n")

# 6 Проверка гипотезы распределения с помощью критерия X^2
# Разбиваем на интервалы
k_intervals <- ceiling(1 + log2(N))
breaks <- seq(min(Gamma), max(Gamma), length.out = k_intervals + 1)

observed <- hist(Gamma, breaks = breaks, plot = FALSE)$counts
theor_probs <- diff(pgamma(breaks, shape = shape_gamma, rate = rate_gamma))
expected <- theor_probs * N

# Объединяем интервалы с малыми частотами
min_expected <- 5
while(any(expected < min_expected) & length(expected) > 1) {
  min_idx <- which.min(expected)
  
  if(min_idx == 1) {
    observed <- c(observed[1] + observed[2], observed[-(1:2)])
    expected <- c(expected[1] + expected[2], expected[-(1:2)])
  } else if(min_idx == length(expected)) {
    observed <- c(observed[-(length(observed):(length(observed)-1))], 
                  observed[length(observed)-1] + observed[length(observed)])
    expected <- c(expected[-(length(expected):(length(expected)-1))], 
                  expected[length(expected)-1] + expected[length(expected)])
  } else {
    observed <- c(observed[1:(min_idx-1)], 
                  observed[min_idx] + observed[min_idx+1], 
                  observed[(min_idx+2):length(observed)])
    expected <- c(expected[1:(min_idx-1)], 
                  expected[min_idx] + expected[min_idx+1], 
                  expected[(min_idx+2):length(expected)])
  }
}

cat("Группированные данные (ожидаемые частоты >= 5):\n")
cat("Наблюдаемые частоты:\n")
print(observed)
cat("\nОжидаемые частоты:\n")
print(expected)
cat("\n")

chi_sq_stat <- sum((observed - expected)^2 / expected)

# Число степеней свободы
k <- length(observed)
df <- k - 1  # так как параметры известны

critical_value <- qchisq(0.95, df)
p_value <- 1 - pchisq(chi_sq_stat, df)

cat("РЕЗУЛЬТАТЫ КРИТЕРИЯ ХИ-КВАДРАТ:\n")
cat("  Статистика X^2 =", chi_sq_stat, "\n")
cat("  Число степеней свободы =", df, "\n")
cat("  Критическое значение (alpha=0.05) =", critical_value, "\n")
cat("  p-value =", p_value, "\n\n")

if(chi_sq_stat <= critical_value) {
  cat("ВЫВОД: Нет оснований отвергнуть гипотезу о том, что\n")
  cat("       выборка имеет гамма-распределение с параметрами\n")
  cat("       shape =", shape_gamma, "и rate =", rate_gamma, "\n")
} else {
  cat("ВЫВОД: Гипотеза о том, что выборка имеет гамма-распределение\n")
  cat("       с параметрами shape =", shape_gamma, "и rate =", rate_gamma, "отвергается\n")
}