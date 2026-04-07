# Лабораторная работа №3
# Критерии сравнения групп

rm(list = ls())

# Устанавливаем объем выборки
N <- 100
set.seed(42)  # для воспроизводимости

# Функция для проверки нормальности, равенства средних и дисперсий
analyze_samples <- function(X1, X2, situation_name) {
  cat("\n")
  cat("Ситуация:", situation_name, "\n")
  cat("\n")
  
  # Основные статистики
  mean1 <- mean(X1)
  mean2 <- mean(X2)
  sd1 <- sd(X1)
  sd2 <- sd(X2)
  var1 <- var(X1)
  var2 <- var(X2)
  
  cat("Описательные статистики:\n")
  cat("  X1: mean =", round(mean1, 4), ", sd =", round(sd1, 4), 
      ", var =", round(var1, 4), "\n")
  cat("  X2: mean =", round(mean2, 4), ", sd =", round(sd2, 4), 
      ", var =", round(var2, 4), "\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  # Проверка нормальности (критерий Шапиро-Уилка)
  shapiro_X1 <- shapiro.test(X1)
  shapiro_X2 <- shapiro.test(X2)
  
  cat("Критерий Шапиро-Уилка для нормальности:\n")
  cat("  X1: W =", round(shapiro_X1$statistic, 4), 
      ", p-value =", format(shapiro_X1$p.value, scientific = TRUE, digits = 4))
  if (shapiro_X1$p.value > 0.05) {
    cat(" -> распределение нормальное\n")
  } else {
    cat(" -> распределение не нормальное\n")
  }
  
  cat("  X2: W =", round(shapiro_X2$statistic, 4), 
      ", p-value =", format(shapiro_X2$p.value, scientific = TRUE, digits = 4))
  if (shapiro_X2$p.value > 0.05) {
    cat(" -> распределение нормальное\n")
  } else {
    cat(" -> распределение не нормальное\n")
  }
  
  # Проверка равенства дисперсий (критерий Фишера)
  var_test <- var.test(X1, X2)
  cat("\nКритерий Фишера для равенства дисперсий:\n")
  cat("  F =", round(var_test$statistic, 4), 
      ", p-value =", format(var_test$p.value, scientific = TRUE, digits = 4))
  if (var_test$p.value > 0.05) {
    cat(" -> дисперсии равны\n")
  } else {
    cat(" -> дисперсии различны\n")
  }
  
  # Проверка равенства средних (t-критерий Стьюдента)
  t_test <- t.test(X1, X2)
  cat("\nt-критерий Стьюдента для равенства средних:\n")
  cat("  t =", round(t_test$statistic, 4), 
      ", df =", round(t_test$parameter, 2),
      ", p-value =", format(t_test$p.value, scientific = TRUE, digits = 4))
  if (t_test$p.value > 0.05) {
    cat(" -> средние равны\n")
  } else {
    cat(" -> средние различны\n")
  }
  
  # Результаты
  invisible(list(
    mean_X1 = mean1, sd_X1 = sd1,
    mean_X2 = mean2, sd_X2 = sd2,
    shapiro_X1 = shapiro_X1$p.value,
    shapiro_X2 = shapiro_X2$p.value,
    var_test_p = var_test$p.value,
    t_test_p = t_test$p.value
  ))
}

# Функция для построения графиков
plot_samples_base <- function(X1, X2, situation_name) {
  
  par(mfrow = c(1, 2))
  
  # 1. Гистограммы с оценками плотности
  # Определяем общие границы для осей
  xlim <- range(c(X1, X2))
  ylim_hist <- c(0, max(hist(X1, plot = FALSE)$density, 
                        hist(X2, plot = FALSE)$density) * 1.2)
  
  # Гистограмма для X1
  hist(X1, breaks = 20, freq = FALSE, 
       col = rgb(0.7, 0.8, 1, 0.5),  # полупрозрачный голубой
       border = "black",
       xlim = xlim, ylim = ylim_hist,
       main = paste("Гистограммы и плотности -", situation_name),
       xlab = "Значения", ylab = "Плотность")
  
  # Гистограмма для X2 поверх
  hist(X2, breaks = 20, freq = FALSE, 
       col = rgb(1, 0.7, 0.7, 0.5),  # полупрозрачный красный
       border = "black",
       add = TRUE)
  
  # Оценки плотности
  lines(density(X1), col = "blue", lwd = 2)
  lines(density(X2), col = "red", lwd = 2)
  
  # Вертикальные линии для средних
  abline(v = mean(X1), col = "blue", lty = 2, lwd = 2)
  abline(v = mean(X2), col = "red", lty = 2, lwd = 2)
  
  legend("topright", 
         legend = c("X1", "X2", 
                    paste("Ср.X1 =", round(mean(X1), 2)),
                    paste("Ср.X2 =", round(mean(X2), 2))),
         col = c("blue", "red", "blue", "red"),
         lty = c(1, 1, 2, 2), lwd = 2,
         fill = c(rgb(0.7, 0.8, 1, 0.5), rgb(1, 0.7, 0.7, 0.5), NA, NA),
         border = c("black", "black", NA, NA),
         cex = 0.8)
  
  # 2. Ящики с усами (boxplots)
  boxplot(X1, X2, 
          names = c("X1", "X2"),
          col = c("lightblue", "lightcoral"),
          main = paste("Ящики с усами -", situation_name),
          xlab = "Группа", ylab = "Значения",
          outpch = 19, outcol = "darkgray")
  
  # Точки для средних значений
  points(1, mean(X1), pch = 18, cex = 2, col = "black")
  points(2, mean(X2), pch = 18, cex = 2, col = "black")
  
  # Текст со статистиками
  mtext(paste0("X1: mean=", round(mean(X1), 2), ", sd=", round(sd(X1), 2),
               "\nX2: mean=", round(mean(X2), 2), ", sd=", round(sd(X2), 2)),
        side = 3, line = -2, outer = TRUE, cex = 0.8)
}

# Данные для четырех ситуаций

# Ситуация a: Математические ожидания и дисперсии равны
set.seed(123)
X1_a <- rnorm(N, mean = 5, sd = 2)
X2_a <- rnorm(N, mean = 5, sd = 2)

# Ситуация b: Математические ожидания существенно различны, дисперсии равны
set.seed(124)
X1_b <- rnorm(N, mean = 5, sd = 2)
X2_b <- rnorm(N, mean = 8, sd = 2)

# Ситуация c: Математические ожидания равны, дисперсии существенно различны
set.seed(125)
X1_c <- rnorm(N, mean = 5, sd = 1)
X2_c <- rnorm(N, mean = 5, sd = 3)

# Ситуация d: Математические ожидания и дисперсии существенно различны
set.seed(126)
X1_d <- rnorm(N, mean = 5, sd = 1)
X2_d <- rnorm(N, mean = 8, sd = 3)

# Список для хранения результатов
results_list <- list()

# Анализируем каждую ситуацию и строим графики

# Ситуация a
cat("\n")
cat("СИТУАЦИЯ A: Средние равны, дисперсии равны\n")
cat("\n")
results_a <- analyze_samples(X1_a, X2_a, "a. Средние равны, дисперсии равны")
plot_samples_base(X1_a, X2_a, "a. Средние равны, дисперсии равны")
results_list$a <- results_a

# Ситуация b
cat("\n")
cat("СИТУАЦИЯ B: Средние различны, дисперсии равны\n")
cat("\n")
results_b <- analyze_samples(X1_b, X2_b, "b. Средние различны, дисперсии равны")
plot_samples_base(X1_b, X2_b, "b. Средние различны, дисперсии равны")
results_list$b <- results_b

# Ситуация c
cat("\n")
cat("СИТУАЦИЯ C: Средние равны, дисперсии различны\n")
cat("\n")
results_c <- analyze_samples(X1_c, X2_c, "c. Средние равны, дисперсии различны")
plot_samples_base(X1_c, X2_c, "c. Средние равны, дисперсии различны")
results_list$c <- results_c

# Ситуация d
cat("\n")
cat("СИТУАЦИЯ D: Средние различны, дисперсии различны\n")
cat("\n")
results_d <- analyze_samples(X1_d, X2_d, "d. Средние различны, дисперсии различны")
plot_samples_base(X1_d, X2_d, "d. Средние различны, дисперсии различны")
results_list$d <- results_d

# Возвращаем стандартные настройки графического устройства
par(mfrow = c(1, 1))

# Сводная таблица результатов
cat("\n")
cat("СВОДНАЯ ТАБЛИЦА РЕЗУЛЬТАТОВ\n")
cat("\n")

# Таблица в виде матрицы для красивого вывода
summary_matrix <- matrix(NA, nrow = 4, ncol = 10)
colnames(summary_matrix) <- c("Ситуация", "Mean_X1", "SD_X1", "Mean_X2", "SD_X2",
                              "Shap_X1", "Shap_X2", "Var_test", "T_test", "Результат")
rownames(summary_matrix) <- NULL

# Заполнение матрицы
situations <- c("a", "b", "c", "d")
for (i in seq_along(situations)) {
  res <- results_list[[situations[i]]]
  summary_matrix[i, 1] <- situations[i]
  summary_matrix[i, 2] <- round(res$mean_X1, 2)
  summary_matrix[i, 3] <- round(res$sd_X1, 2)
  summary_matrix[i, 4] <- round(res$mean_X2, 2)
  summary_matrix[i, 5] <- round(res$sd_X2, 2)
  summary_matrix[i, 6] <- format(res$shapiro_X1, scientific = TRUE, digits = 3)
  summary_matrix[i, 7] <- format(res$shapiro_X2, scientific = TRUE, digits = 3)
  summary_matrix[i, 8] <- format(res$var_test_p, scientific = TRUE, digits = 3)
  summary_matrix[i, 9] <- format(res$t_test_p, scientific = TRUE, digits = 3)
  
  # Результат
  var_equal <- res$var_test_p > 0.05
  mean_equal <- res$t_test_p > 0.05
  summary_matrix[i, 10] <- paste0(ifelse(mean_equal, "ср.=", "ср.≠"), 
                                  "/", ifelse(var_equal, "дисп.=", "дисп.≠"))
}

# Вывод таблицы
print(summary_matrix, quote = FALSE)

# Сравнительный график для всех выборок
par(mfrow = c(2, 2))

# Функция для создания boxplot для конкретной ситуации
plot_situation_boxplot <- function(X1, X2, situation_name, pos) {
  boxplot(X1, X2,
          names = c("X1", "X2"),
          col = c("lightblue", "lightcoral"),
          main = situation_name,
          ylab = "Значения",
          outpch = 19, outcol = "darkgray",
          ylim = range(c(X1_a, X2_a, X1_b, X2_b, X1_c, X2_c, X1_d, X2_d)))
  points(1, mean(X1), pch = 18, cex = 2, col = "black")
  points(2, mean(X2), pch = 18, cex = 2, col = "black")
}

# 4 boxplot на одном графике
par(mfrow = c(2, 2))
plot_situation_boxplot(X1_a, X2_a, "a. Ср.=, Дисп.=", 1)
plot_situation_boxplot(X1_b, X2_b, "b. Ср.≠, Дисп.=", 2)
plot_situation_boxplot(X1_c, X2_c, "c. Ср.=, Дисп.≠", 3)
plot_situation_boxplot(X1_d, X2_d, "d. Ср.≠, Дисп.≠", 4)

par(mfrow = c(1, 1))
