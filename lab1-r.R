#Лабораторная работа 1
#Задание 1
install.packages("readxl")

#Подключение библиотеки
library(readxl)

#Чтение первой таблицы
Table1 <- read_excel("zadanie1.xlsx", sheet = "Table1")

#Чтение второй таблицы
Table2 <- read_excel("zadanie1.xlsx", sheet = "Table2")

#Вывод результатов
print(Table1)
print(Table2)

#Задание 2
install.packages("openxlsx", type = "binary")
library(openxlsx)

write.xlsx(Table1, "Zadanie2.xlsx", rowNames = FALSE)

#Задание 3
#Создание вектора
my_vector <- c(10, 20, 30, 40, 50)

#Создание матрицы
my_matrix <- matrix(
  c(1, 2, 3, 4, 5, 6),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)

print(my_vector)
print(my_matrix)

#Создание дата-фрейма
my_data <- data.frame(
  Name = c("Ivan", "Maria", "Alex", "Anna"),
  Age = c(21, 23, 20, 22),
  Score = c(85, 92, 78, 95)
)

#Создание фактора
my_factor <- factor(
  c("High", "Medium", "Low", "High", "Medium")
)

#Просмотр уровней фактора
levels(my_factor)

print(my_factor)
print(my_data)

#Создание многомерного массива
my_array <- array(
  1:12,
  dim = c(2, 3, 2)
)

#Вывод массива и просмотр размерности 
dim(my_array)
print(my_array)

#Создание списка
my_list <- list(
  Student = "Ivan",
  Age = 21,
  Grades = c(4, 5, 5),
  Passed = TRUE
)

#Вывод списка
print(my_list)

#Получение отдельного элемента списка
my_list$Student

#Задание 4
install.packages("httr", type = "binary")
install.packages("xml2", type = "binary")
library(dplyr)
library(httr)
library(xml2)

#Получение данных с сайта ЦБ РФ
url <- "https://www.cbr.ru/scripts/XML_daily.asp"
response <- GET(url)

#Проверка статуса запроса
print(status_code(response))

#Чтение XML
content_text <- content(
  response,
  "text",
  encoding = "windows-1251"
)

xml_data <- read_xml(content_text)

#Извлечение информации о валютах
valutes <- xml_find_all(xml_data, ".//Valute")

#Создание дата-фрейма
currency_df <- data.frame(
  CharCode = xml_text(xml_find_first(valutes, "./CharCode")),
  Nominal = as.numeric(xml_text(xml_find_first(valutes, "./Nominal"))),
  Name = xml_text(xml_find_first(valutes, "./Name")),
  Value = as.numeric(gsub(
    ",", ".",
    xml_text(xml_find_first(valutes, "./Value"))
  )),
  stringsAsFactors = FALSE
)

#Расчёт курса за одну единицу валюты
currency_df$RatePerUnit <- currency_df$Value / currency_df$Nominal

#Округление значений
currency_df$Value <- round(currency_df$Value, 4)
currency_df$RatePerUnit <- round(currency_df$RatePerUnit, 4)

#Вывод результатов
options(scipen = 999)
print(currency_df)

#Просмотр таблицы в отдельном окне
View(currency_df)

#Задание 5
#Создание пяти текстовых файлов
writeLines("Мой номер 79001234567, звоните", "1.txt")
writeLines("Городской: 73432220000, рабочий", "2.txt")
writeLines("Тут нет телефонов вообще", "3.txt")
writeLines("Два номера: 89005554433 и 74995556677", "4.txt")
writeLines("Мобильный 79161112233 и ещё один 89262223344", "5.txt")

#Функция поиска телефонных номеров
find_phone <- function(text) {
  pattern <- "\\b[78]\\d{10}\\b"
  
  matches <- regmatches(
    text,
    gregexpr(pattern, text)
  )
  
  return(unlist(matches))
}

#Проверка функции на тестовой строке
test_string <- paste(
  "Это пример строки с номером 79001234567",
  "и городским номером 73432220000"
)

print("Тестовая строка:")
print(find_phone(test_string))

#Все пять файлов:
#Список файлов
files <- c("1.txt", "2.txt", "3.txt", "4.txt", "5.txt")

#Обработка файлов по очереди
for (file in files) {
  text <- paste(
    readLines(file, warn = FALSE),
    collapse = " "
  )
  
  phones <- find_phone(text)
  
  print(file)
  print(paste("Найдено номеров:", length(phones)))
  
  if (length(phones) > 0) {
    print(phones)
  } else {
    print("(номера не найдены)")
  }
}

#Задание 6
install.packages("ggplot2", type = "binary")
library(ggplot2)

#Часть 1 - чтение текста из файла и консоли
read_and_show <- function() {
  
#Создание текстового файла
  writeLines("Лабораторная работа №3", "lab3.txt")
  
#Чтение данных из файла
  file_data <- read.table(
    "lab3.txt",
    header = FALSE,
    stringsAsFactors = FALSE
  )
  
#Ввод текста пользователем
  cat("Введите текст: ")
  console_input <- readline()
  
#Объединение данных в дата-фрейм
  df <- data.frame(
    Source = c("Файл", "Консоль"),
    Text = c(file_data$V1[1], console_input),
    stringsAsFactors = FALSE
  )
  
#Вывод результата
  print(df)
  return(df)
}

result <- read_and_show()


#Часть 2 - обработка пяти файлов в одной функции
process_files <- function() {
  
  #Создание пяти файлов с числами
  for (i in 1:5) {
    numbers <- (1:10) * (i + 1)
    writeLines(as.character(numbers), paste0("file", i, ".txt"))
  }
  
  #Создание пяти отдельных окружений с переменными X
  environments <- vector("list", 5)
  
  for (i in 1:5) {
    environments[[i]] <- new.env()
    environments[[i]]$X <- scan(
      paste0("file", i, ".txt"),
      quiet = TRUE
    )
  }
  
  #Вывод переменных X
  for (i in 1:5) {
    cat("\nПеременная X из файла", i, ":\n")
    print(environments[[i]]$X)
  }
  
  #Объединение данных в дата-фрейм
  df_plot <- data.frame()
  
  for (i in 1:5) {
    values <- environments[[i]]$X
    
    df_plot <- rbind(
      df_plot,
      data.frame(
        Index = seq_along(values),
        Value = values,
        File = paste("Файл", i)
      )
    )
  }
  
  #Построение графика
  plot <- ggplot(
    df_plot,
    aes(x = Index, y = Value, color = File)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Числовые данные из пяти файлов",
      x = "Номер значения",
      y = "Числовое значение",
      color = "Источник данных"
    ) +
    theme_minimal()
  
  print(plot)
  
  #Вывод итогового дата-фрейма
  print(df_plot)
  
  return(df_plot)
}

#Запуск функции
result_files <- process_files()