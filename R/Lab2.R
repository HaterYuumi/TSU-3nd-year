# Лабораторная работа №2
# Интервальные оценки параметров нормального распределения

rm(list = ls())

# Устанавливаем параметры нормального распределения
a <- 5           # математическое ожидание
sigma <- 2        # стандартное отклонение
alpha <- 0.05     # уровень значимости

# Объемы выборок
N_values <- c(100, 500, 1000)

# Создаем список для хранения результатов
results <- list()

#4. Построить доверительные интервалы для математического ожидания
#при известном значении дисперсии на уровне значимости alph = 0,05.

# Функция для построения доверительного интервала для мат. ожидания 
# при известной дисперсии
ci_known_var <- function(x, sigma, alpha) {
  n <- length(x)
  mean_x <- mean(x)
  z <- qnorm(1 - alpha/2)
  margin <- z * sigma / sqrt(n)
  c(left = mean_x - margin, right = mean_x + margin)
}

#5. Построить доверительные интервалы для математического ожидания
#при неизвестном значении дисперсии (оцененном по выборке) на уровне значимости alph = 0,05.

# Функция для построения доверительного интервала для мат. ожидания 
# при неизвестной дисперсии
ci_unknown_var <- function(x, alpha) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  t <- qt(1 - alpha/2, df = n-1)
  margin <- t * sd_x / sqrt(n)
  c(left = mean_x - margin, right = mean_x + margin)
}

# Матрица для данных графика зависимости
plot_data <- NULL


#1. Сгенерировать выборки объема N (N = 100, 500, 1000) из нормального
#распределения с одинаковыми параметрами. Параметры задать самостоятельно.

# Основной цикл по объемам выборок
for (i in seq_along(N_values)) {
  N <- N_values[i]
  
  # Генерируем выборку
  set.seed(123 + i)  # для воспроизводимости
  sample <- rnorm(N, mean = a, sd = sigma) # нормальное распределение
  
  #3. Найти оценки числовых характеристик (выборочные среднее, дисперсию, СКО).
  
  # Точечные оценки
  sample_mean <- mean(sample) # выборочное среднее
  sample_var <- var(sample) # дисперсия
  sample_sd <- sd(sample) # СКО
  
  # Доверительные интервалы
  ci_known <- ci_known_var(sample, sigma, alpha)
  ci_unknown <- ci_unknown_var(sample, alpha)
  

  results[[i]] <- data.frame(
    N = N,
    mean = sample_mean,
    var = sample_var,
    sd = sample_sd,
    ci_known_left = ci_known[1],
    ci_known_right = ci_known[2],
    ci_unknown_left = ci_unknown[1],
    ci_unknown_right = ci_unknown[2]
  )
  
  # Добавляем данные для графика зависимости
  plot_data <- rbind(plot_data, 
                     data.frame(N = N, type = "Точечная оценка", value = sample_mean),
                     data.frame(N = N, type = "Левая граница (изв. дисп.)", value = ci_known[1]),
                     data.frame(N = N, type = "Правая граница (изв. дисп.)", value = ci_known[2]),
                     data.frame(N = N, type = "Левая граница (неизв. дисп.)", value = ci_unknown[1]),
                     data.frame(N = N, type = "Правая граница (неизв. дисп.)", value = ci_unknown[2]))
  
  # функция графического устройства для размещения двух графиков рядом
  par(mfrow = c(1, 2))

  #2. Построить квантильные графики (qqplot) и гистограммы для
  #построенных выборок.  
    
  # Построение гистограммы
  hist(sample, breaks = 20, freq = FALSE, 
       main = paste("Гистограмма (N =", N, ")"),
       xlab = "Значения", ylab = "Плотность",
       col = "lightblue", border = "black", 
       ylim = c(0, max(density(sample)$y, dnorm(mean(sample), mean = a, sd = sigma)) * 1.1))
  
  # Теоретическую плотность
  curve(dnorm(x, mean = a, sd = sigma), 
        add = TRUE, col = "red", lwd = 2)
  
  # Линия оценки плотности по выборке
  lines(density(sample), col = "green", lwd = 2, lty = 2)
  
  # Вертикальная линия для выборочного среднего
  abline(v = sample_mean, col = "blue", lty = 2, lwd = 2)
  abline(v = a, col = "darkgreen", lty = 3, lwd = 2)
  
  legend("topright", 
         legend = c("Теоретическая плотность", 
                    "Оценка плотности",
                    paste0("Выб. среднее = ", round(sample_mean, 2)),
                    paste0("Ист. среднее = ", a)),
         col = c("red", "green", "blue", "darkgreen"), 
         lty = c(1, 2, 2, 3), lwd = 2, cex = 0.7)
  
  # QQ-plot
  qqnorm(sample, main = paste("QQ-plot (N =", N, ")"),
         xlab = "Теоретические квантили", 
         ylab = "Выборочные квантили",
         col = "blue", pch = 19, cex = 0.7)
  qqline(sample, col = "red", lwd = 2)
  
  # Вывод результата в консоль
  cat("\n")
  cat("Объем выборки N =", N, "\n")
  cat("\n")
  cat("Точечные оценки:\n")
  cat("  Выборочное среднее:", round(sample_mean, 4), "\n")
  cat("  Выборочная дисперсия:", round(sample_var, 4), "\n")
  cat("  Выборочное СКО:", round(sample_sd, 4), "\n")
  cat("\n")
  cat("Доверительные интервалы для мат. ожидания (alph = 0.05):\n")
  cat("  При известной дисперсии:\n")
  cat("    [", round(ci_known[1], 4), "; ", round(ci_known[2], 4), "]\n", sep = "")
  cat("    Длина интервала:", round(ci_known[2] - ci_known[1], 4), "\n")
  cat("  При неизвестной дисперсии:\n")
  cat("    [", round(ci_unknown[1], 4), "; ", round(ci_unknown[2], 4), "]\n", sep = "")
  cat("    Длина интервала:", round(ci_unknown[2] - ci_unknown[1], 4), "\n")
  
}

# Возвращаем стандартные настройки графического устройства
par(mfrow = c(1, 1))

# Объединяем результаты в таблицу
results_table <- do.call(rbind, results)
cat("\n")
cat("Сводная таблица результатов:\n")
print(results_table)

#6. Построить график зависимости точечных оценок математического
#ожидания, а также левых и правых границ доверительных интервалов от объема выборки (на одном рисунке).

# График зависимости точечных оценок и границ доверительных интервалов от объема выборки
# Создаем график с помощью base R
par(mfrow = c(1, 1))

# Определяем цвета для разных типов линий
colors <- c("Точечная оценка" = "black",
            "Левая граница (изв. дисп.)" = "blue",
            "Правая граница (изв. дисп.)" = "blue",
            "Левая граница (неизв. дисп.)" = "red",
            "Правая граница (неизв. дисп.)" = "red")

# Создаем пустой график
plot(0, 0, type = "n", 
     xlim = c(0.5, length(N_values) + 0.5), 
     ylim = range(plot_data$value) + c(-0.5, 0.5),
     xlab = "Объем выборки", 
     ylab = "Значение",
     main = "Зависимость оценок и границ ДИ от объема выборки",
     xaxt = "n")
axis(1, at = 1:length(N_values), labels = N_values)

# Добавляем горизонтальную линию для истинного среднего
abline(h = a, lty = 2, col = "gray50", lwd = 2)

# Добавляем линии и точки для каждого типа
types <- unique(plot_data$type)
for (type in types) {
  subset_data <- plot_data[plot_data$type == type, ]
  lines(1:length(N_values), subset_data$value, 
        col = colors[type], lwd = 2, lty = ifelse(grepl("Левая", type), 2, 1))
  points(1:length(N_values), subset_data$value, 
         col = colors[type], pch = 19, cex = 1.5)
}

legend("topright", 
       legend = c("Точечная оценка", 
                  "Границы (изв. дисп.)",
                  "Границы (неизв. дисп.)",
                  paste("Истинное среднее =", a)),
       col = c("black", "blue", "red", "gray50"),
       lty = c(1, 1, 1, 2), lwd = 2, cex = 0.8)