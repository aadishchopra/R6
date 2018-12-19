#' ---
#' title: "R6"
#' author: "Aadish Chopra"
#' date: "November 14, 2018"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE,autodep=TRUE-----------------------------------
knitr::opts_chunk$set(echo = TRUE,results = 'asis',warning = FALSE,message = FALSE)

#' 
#' #Learning R6 
#' Click [here](https://cran.r-project.org/web/packages/R6/R6.pdf) to see.
#' 
#' Why use R6?
#' 
#' 1. Classes with reference semantics
#' 2. Doesn't require the methods package
#' 3. Most important is it supports inheritance **even** from different packages
#' 
#' 
#' Check to see if the package is there
#' 
## ----checkpackage--------------------------------------------------------

if (!require('R6')){
  install.packages("R6")
}else
{
require('R6')  
}



#' 
#' 
#' 
#' 
#' Optional Code: if the user wants to download the document as a pdf
#' 
#' 
## ----eval=FALSE----------------------------------------------------------
## 
## if(!file.exists('R6 Documentation')){
## download.file('https://cran.r-project.org/web/packages/R6/R6.pdf','R6 Documentation',mode = 'wb')
## }
## 

#' Learning by running the functions from the document
#' 
#' 
## ----functionsfromR6-----------------------------------------------------
class_generator <- R6Class()
object <- class_generator$new()
is.R6Class(class_generator)
is.R6(class_generator)
is.R6Class(object)
is.R6(object)



#' From Hadley Wickham's [Advanced R](https://adv-r.hadley.nz/r6.html)
#' 
#' R6 objects have reference semantics which means that they are modified in-place, not copied-on-modify. 
#' 
#' Modified in place means the object value is changed then and there, no copying of object and then changing the value of the copied object
#' 
#' ####Disadvantage
#' Which means you lose what was inside of the original object
#' 
#' ####Advantage
#' Saves memory
#' 
#' Before learning this let's look at an example explained in [Advanced R](https://adv-r.hadley.nz/r6.html). A little side note to runif. Before today I used to speak it as run-if. Although I knew its functionality I never captured the idea that it is actually r-unif which is uniform distribution in R.Silly me 
#' 
#' 
#' 
## ------------------------------------------------------------------------


x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}


#' 
#' 
#' Below is the code which shows how many times a object has been created. 
#' 
## ------------------------------------------------------------------------


for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}




#' 
#' 
## ----classGenerator------------------------------------------------------

Accumulator<-R6Class("Accumulator",list(
  sum=0,
  add=function(x=1){
    self$sum<-self$sum+x
    invisible(self)
    
    }
    
))



#' 
#' 1. Name of the class should be passed into the object itself
#' 2. Self is working like this in java
#' 
## ----Accumulator---------------------------------------------------------


# Creating an object from the class

x<-Accumulator$new()
x$add(1)
x$add(4)
x$sum





#' 
#' The output of the above code would be 5 instead of 4 because the object replaces the sum or overwrites it.
#' 
#' Method Chaining: x$add(2)$add(6)
#' 
#' 
#' The below code would throw an error since a non-numeric argument was passed to the function 
#' 
#' #Initialization and Print Methods
#' 
## ----Person--------------------------------------------------------------
# x$add('two')$sum

# Initialization method

Person<-R6Class("Person",list(
  name=NULL,
  age=NA,
  initialize=function(name,age){
    stopifnot(is.character(name),length(name)==1)
    stopifnot(is.numeric(age),length(age)==1)
    self$name<-name
    self$age<-age
  }
))

aadish<-Person$new("Aadish",age=38)
aadish$age
aadish$name

#Print method


Person<-R6Class("Person",list(
  name=NULL,
  age=NA,
  initialize=function(name,age){
    stopifnot(is.character(name),length(name)==1)
    stopifnot(is.numeric(age),length(age)==1)
    self$name<-name
    self$age<-age
  },
  print=function(...){
    cat("Person: \n")
    cat(" Name ",self$name,"\n",sep="")
    cat(" Age: ",self$age,"\n",sep="")
      }
  
))

aadish2<-Person$new("Aadish",74)
aadish2


#' 
#' #Inheritance using R6
#' 
#' 
## ----AccumulaorChatty----------------------------------------------------
AccumulatorChatty<-R6Class("Accumulator Chatty",
                           inherit=Accumulator,
                           public=list(
                             add=function(x=1){
                               cat("Adding ",x,"\n",sep=" ")
                               super$add(x=x)
                               
                              }
                           ))



#' 
#' In this example we learnt that we can inherit all the members( fields and methods) of the parent class. The add method invokes the add function from the parent class , a concept which is there in JAVA (Yes, I know JAVA only).
#' 
#' When Accumulator Chatty is run since it has it's own add class it will overide the parent class but we can use some functionalities like the **ADDING** operation from the parent class
#' 
#' 
#' 
## ------------------------------------------------------------------------
AccumulatorChatty_obj<-AccumulatorChatty$new()
AccumulatorChatty_obj$add(10)$add(20)$sum


#' 
#' I want to build an accumulator without using an R6 class
#' 
#' #Exercises
#' 
#' These exercises are part of Hadley's book
#' 
#' ####Question 1 
#' 
#' ####Solution Part 1 
#' 
## ----BankAccount---------------------------------------------------------

BankAccount<-R6Class("BankAccount",list(
  balance=0,
  deposit=function(amount){
    self$balance=self$balance+amount
    invisible(self)
  },
  withdraw=function(amount){
    self$balance=self$balance-amount
    invisible(self)
  }
  ))

BankAccountobj<-BankAccount$new()
BankAccountobj$deposit(20)
BankAccountobj$deposit(100)
BankAccountobj$withdraw(10)
BankAccountobj$balance


#' 
#' 
#' ####Solution Part 2
#' 
## ----BankAccountOverdraft------------------------------------------------
BankAccountOverdraft<-R6Class("BankAccountOverdraft",
                               inherit =BankAccount,
                               list(
    withdraw=function(amount){
      cat('your balance is',self$balance,"\n")    
      if(amount < self$balance){
      super$withdraw(amount)
      }else
      {
        print("Withdrawal amount more than balance")
      }
  }
  ))


BankAccountOverdraftobj<-BankAccountOverdraft$new()
BankAccountOverdraftobj$deposit(20)
BankAccountOverdraftobj$withdraw(40)
BankAccountOverdraftobj$balance

#' 
#' 
#' ####Solution Part 3
#' 
## ------------------------------------------------------------------------
BankAccountOverdraftFees<-R6Class("BankAccountOverdraftFees",
                               inherit =BankAccount,
                               
                               list(
    Fees=0,withdraw=function(amount){
        if(amount < self$balance){
        super$withdraw(amount)
      }else
      {
        self$Fees=(amount-self$balance)*0.035
        super$withdraw(amount)
        cat("Withdrawal amount more than balance",'Fees charged $',self$Fees)
      }
  }
  ))

BankAccountOverdraftFeesobj<-BankAccountOverdraftFees$new()
BankAccountOverdraftFeesobj$deposit(20)
BankAccountOverdraftFeesobj$deposit(40)
BankAccountOverdraftFeesobj$withdraw(40)
BankAccountOverdraftFeesobj$balance


#' 
#' 
#' ####Question 2 
#' 
#' Creating the cards
#' 
## ------------------------------------------------------------------------
suit <- c("♠", "♥", "♦", "♣")
value <- c("A", 2:10, "J", "Q", "K")
cards <- paste0(rep(value, 4), suit)



#' 
#' Defining a R6 class 
#' 
#' Question 2 Solution Part 1
#' 
## ----DeckCards,results='asis'--------------------------------------------



DeckCards<-R6Class("DeckCards",list(
suit = c("♠", "♥", "♦", "♣"),
value = c("A", 2:10, "J", "Q", "K"),
cards = paste0(rep(value, 4), suit),
picked=0,
# min=0,
# max=0,
# number=0,
draw=function(number=1){
  
  self$picked=sample(self$cards,number,replace=F)
  cat('your picked card is',self$picked," ")
  self$cards<-self$cards[!self$cards %in% self$picked] 
  invisible(self)
  },
reshuffle=function(){
  self$cards<-cards
}
))

DeckCards<-DeckCards$new()
DeckCards$cards
DeckCards$draw()
DeckCards$draw()



#' 
#' You can add an if-else statement to the draw function which checks the lenght of the cards.Because in this case after 52 attempts size of cards would be zero and the function would throw an error.
#' 
#' 
#' Controlling access: Private and active fields.
#' 
#' 
#' 
## ----Private-------------------------------------------------------------

Person <- R6Class("Person", 
  public = list(
    initialize = function(name, age = NA) {
      private$name <- name
      private$age <- age
    },
    print = function(...) {
      cat("Person: \n")
      cat("  Name: ", private$name, "\n", sep = "")
      cat("  Age:  ", private$age, "\n", sep = "")
    }
  ),
  private = list(
    age = NA,
    name = NULL
  )
)


#' 
#' Differences. Looking at the code we can see that the variables name, age are declared outside of the public. If you want to access these variables you have to use private$
#' 
#' You can not access them by using private $name or private $age. Example would be you don't want to show marks but only pass or fail, or if a person has cancer or not but not their vitals.
#' 
#' #Active Fields
#' 
## ----ActiveFields--------------------------------------------------------
Rando<-R6Class("Rando",active = list(
  random=function(value){
    if(missing(value)){
      runif(1)
    }
    else{
      stop("Can't set `$random'",call.=FALSE)
    }
  }
))


x<-Rando$new()


#' 
#' Here the function Rando is behaving as a variable rather than a function. It doesn't accept values passed to it and call to Rando is not parenthesized
#' 
## ------------------------------------------------------------------------
Person<-R6Class("Person",
                private = list(
                  .age=NA,
                  .name=NULL
                  ),
                active=list(
                  age=function(value){
                    if(missing(value)){
                      private$.age
                    }else
                    {
                      stop("`$age` is read only",call.=FALSE)
                    }
                  },
                  name=function(value){
                    if(missing(value)){
                      private$.name
                    }else
                    {
                      stopifnot(is.character(value),length(value)==1)
                      private$.name=value
                      self
                      
                    }
                  }),
                  public=list(
                    initialize=function(name,age=NA){
                      private$.name=name
                      private$.age=age
                    }
                  )
                )





#' 
#' The above example is a little confusing. Since you can set anything by calling object but not explicitly passing to it 
#' 
## ----finalize------------------------------------------------------------

TemporaryFile<-R6Class("TemporaryFile",list(
  path=NULL,
  initialize=function(){
    self$path=tempfile()
  },
  finalize=function(){
    message("Cleaning up", self$path)
    unlink(self$path)
  }
))

tf<-TemporaryFile$new()
tf$finalize()


#' 
#' In the book it is mentioned that rm would produce the same output as tf but it is wrong. In fact even finalize itself is not printing anything. 
#' 
#' If anyone finds the issue please raise it
#' 
## ------------------------------------------------------------------------


#' 
#' 
#' 
#' 
