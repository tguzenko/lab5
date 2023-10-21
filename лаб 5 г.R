# Задание №1 - создать класс Домашнее животное с подклассами (кошка, собака, корова), у которых есть общие методы (наследуемые) и собственные.
p <- list(
  type = "cat",
  name = "Sima",
  sound = "meow",
  age = 5
)
class(p) <- "pet"
print.pet <- function(pets){
  cat(pets$name, "\n")
  cat("Тип", pets$type, "\n")
  cat("Возраст", pets$age, "\n")
}
methods(,"pet")
print.pet
p
cw <- list(
  type = "cow",
  name = "Marta",
  sound = "moo",
  age = 8,
  place = "home"
)
class(cw) <- c("place", "p")
class(cw) <- "pet"
print.pet <- function(pets){
  cat(pets$name, "\n")
  cat("Тип", pets$type, "\n")
  cat("Возраст", pets$age, "\n")
  cat("Место", pets$place, "\n")
}
d <- list(
  type = "dog",
  name = "Barsik",
  sound = "bark",
  age = 4,
  place = "street",
  color = "black"
)
class(d)<-c("color", "cw")
class(d) <- "pet"
print.pet <- function(pets){
  cat(pets$name, "\n")
  cat("Тип", pets$type, "\n")
  cat("Возраст", pets$age, "\n")
  cat("Место", pets$place, "\n")
  cat("Цвет", pets$color, "\n")
}
d
# Практическое задание:
setClass("cars",
         representation(
           brand = "character",
           year = "numeric",
           mileage = "numeric")
)
mazda <- new("cars", brand = "Mazda", year = 2014, mileage = 1000)
mazda

setMethod("show", "cars",
          function(object){
            inorout <- ifelse(object@year, "is", "is not")
            cat(object@brand, "год производства", object@mileage,
                "and", inorout, "пробег", "\n")
          }
)

setClass("cars",
         slots = list(),
         contains = "environment")
setGeneric("ехать",
           function(object) 
           {
             standardGeneric("ехать")
           }
)
setGeneric("гудеть",
           function(object) 
           {
             standardGeneric("гудеть")
           }
)
setMethod("ехать",
          signature(object = "cars"),
          function(object) 
          {
            cat("I'm driving\n")
          }
)

setMethod("гудеть",
          signature(object = "cars"),
          function(object) 
          {
            cat("biiip-biiip!!\n")
          }
)
setClass("Дизельный автомобиль",
         slots = list(),
         contains = "cars")

setMethod("ехать",
          signature(object = "Дизельный автомобиль"),
          function(object) 
          {
            cat("I'm driving on diesel fuel\n")
          }
)

setClass("Бензиновый автомобиль",
         slots = list(),
         contains = "cars")

setMethod("ехать",
          signature(object = "Бензиновый автомобиль"),
          function(object) 
          {
            cat("I'm driving on gasoline\n")
          }
)

setClass("Электромобиль",
         slots = list(),
         contains = "cars")

setMethod("ехать",
          signature(object = "Электромобиль"),
          function(object) 
          {
            cat("I'm driving on electricity\n")
          }
)

запуск_автомобилей <- function() {
  выбор <- readline(prompt = "Какой класс автомобилей вас интересует? (Дизельный автомобиль, Бензиновый автомобиль, Электрический автомобиль): ")
  
  if (tolower(выбор) == "дизельный") {
    cars <- new("Дизельный автомобиль")
  } else if (tolower(выбор) == "бензиновый") {
    cars <- new("Бензиновый автомобиль")
  } else if (tolower(выбор) == "электрический") {
    cars <- new("Электромобиль")
  } else {
    cat("Неверный выбор!\n")
    return
  }
  cat("Родительские методы:\n")
  ехать(cars)
  гудеть(cars)
  cat("Собственные методы:\n")
  if (class(cars) == "Дизельный автомобиль") 
  {
    cat("Едет на солярке\n")
  } 
  else if (class(cars) == "Бензиновый автомобиль") 
  {
    cat("Едет на бензине\n")
  } 
  else if (class(cars) == "Электромобиль") 
  {
    cat("Едет на электричестве\n")
  }
}

запуск_автомобилей()

