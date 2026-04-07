# Случайный объем выборки от 100 до 200
N <- sample(100:200, 1)
cat("Объем выборки N =", N, "\n\n")

# ЭКСПОНЕНЦИАЛЬНОЕ РАСПРЕДЕЛЕНИЕ

# Параметры
lambda_exp <- 0.5  # интенсивность (обратная к среднему)

# 1 Генерация выборки
Exp <- rexp(N, rate = lambda_exp)

# 2 Гистограмма и эмпирическая функция распределения

# ГИСТОГРАММА (для непрерывных данных вместо полигона частот)
hist(Exp, 
     breaks = "FD",  # правило Фридмана-Диакониса для выбора числа интервалов
     main = "Гистограмма относительных частот (экспоненциальное распределение)",
     xlab = "Значения",
     ylab = "Плотность",
     col = "lightblue",
     border = "black",
     freq = FALSE,  # относительные частоты
     probability = TRUE)
# теоретическая плотность
curve(dexp(x, rate = lambda_exp), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
legend("topright", legend = c("Эмпирическая", "Теоретическая"), 
       fill = c("lightblue", NA), col = c(NA, "red"), lwd = c(NA, 2))
grid()

# ЭМПИРИЧЕСКАЯ ФУНКЦИЯ РАСПРЕДЕЛЕНИЯ
F_emp <- ecdf(Exp)

plot(F_emp, 
     main = "Эмпирическая функция распределения (экспоненциальное)",
     xlab = "x",
     ylab = "F_n(x)",
     col = "blue",
     lwd = 2,
     do.points = FALSE,  # для непрерывных данных точки не так информативны
     verticals = TRUE)
#теоретическая функцию распределения
curve(pexp(x, rate = lambda_exp), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
legend("bottomright", legend = c("Эмпирическая", "Теоретическая"), 
       col = c("blue", "red"), lwd = 2)
grid()

# 3 Коэффициенты асимметрии, коэффициенты эксцесса
Exp_mean <- mean(Exp)
Exp_var <- var(Exp)
Exp_sd <- sd(Exp)
Exp_median <- median(Exp)

# Расчет коэффициента асимметрии
n <- length(Exp)
Exp_skewness <- (sum((Exp - Exp_mean)^3) / n) / (Exp_sd^3)

# Расчет коэффициента эксцесса
Exp_kurtosis <- (sum((Exp - Exp_mean)^4) / n) / (Exp_sd^4)

cat("ВЫБОРОЧНЫЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Среднее:", Exp_mean, "\n")
cat("  Дисперсия:", Exp_var, "\n")
cat("  СКО:", Exp_sd, "\n")
cat("  Медиана:", Exp_median, "\n")
cat("  Коэффициент асимметрии:", Exp_skewness, "\n")
cat("  Коэффициент эксцесса:", Exp_kurtosis, "\n\n")

# 4 Теоретические характеристики
MX_theor <- 1 / lambda_exp
DX_theor <- 1 / lambda_exp^2
Median_theor <- log(2) / lambda_exp

cat("ТЕОРЕТИЧЕСКИЕ ХАРАКТЕРИСТИКИ:\n")
cat("  Теоретическое мат. ожидание:", MX_theor, "\n")
cat("  Теоретическая дисперсия:", DX_theor, "\n")
cat("  Теоретическая медиана:", Median_theor, "\n\n")

# Сравнение точечных оценок с теоретическими характеристиками
cat("СРАВНЕНИЕ ОЦЕНОК:\n")
cat("  Мат. ожидание:\n")
cat("    Теоретическое:", MX_theor, "\n")
cat("    Выборочное:", Exp_mean, "\n")
cat("    Абсолютная погрешность:", abs(Exp_mean - MX_theor), "\n")
cat("    Относительная погрешность:", abs(Exp_mean - MX_theor) / MX_theor * 100, "%\n\n")

cat("  Дисперсия:\n")
cat("    Теоретическая:", DX_theor, "\n")
cat("    Выборочная:", Exp_var, "\n")
cat("    Абсолютная погрешность:", abs(Exp_var - DX_theor), "\n")
cat("    Относительная погрешность:", abs(Exp_var - DX_theor) / DX_theor * 100, "%\n\n")

# 5 Оценка параметра распределения (метод моментов)
lambda_est <- 1 / Exp_mean

cat("ОЦЕНКА ПАРАМЕТРА lambda (интенсивность):\n")
cat("  Метод моментов: lambda_hat =", lambda_est, "\n")
cat("  Истинное значение lambda:", lambda_exp, "\n")
cat("  Абсолютная погрешность:", abs(lambda_est - lambda_exp), "\n")
cat("  Относительная погрешность:", abs(lambda_est - lambda_exp) / lambda_exp * 100, "%\n\n")

# 6 Проверка гипотезы распределения с помощью критерия X^2
# Для непрерывных данных разбиваем на интервалы

# Выбираем количество интервалов (правило Стерджеса)
k_intervals <- ceiling(1 + log2(N))
breaks <- seq(min(Exp), max(Exp), length.out = k_intervals + 1)

# Наблюдаемые частоты по интервалам
observed <- hist(Exp, breaks = breaks, plot = FALSE)$counts

# Теоретические вероятности для каждого интервала
theor_probs <- diff(pexp(breaks, rate = lambda_exp))

# Теоретические частоты
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

# Вычисляем статистику хи-квадрат
chi_sq_stat <- sum((observed - expected)^2 / expected)

# Число степеней свободы
k <- length(observed)
df <- k - 1  # так как параметр известен

critical_value <- qchisq(0.95, df)
p_value <- 1 - pchisq(chi_sq_stat, df)

cat("РЕЗУЛЬТАТЫ КРИТЕРИЯ ХИ-КВАДРАТ:\n")
cat("  Статистика X^2 =", chi_sq_stat, "\n")
cat("  Число степеней свободы =", df, "\n")
cat("  Критическое значение (alpha=0.05) =", critical_value, "\n")
cat("  p-value =", p_value, "\n\n")

if(chi_sq_stat <= critical_value) {
  cat("ВЫВОД: Нет оснований отвергнуть гипотезу о том, что\n")
  cat("       выборка имеет экспоненциальное распределение с параметром lambda =", lambda_exp, "\n")
} else {
  cat("ВЫВОД: Гипотеза о том, что выборка имеет экспоненциальное распределение\n")
  cat("       с параметром lambda =", lambda_exp, "отвергается\n")
}