#Homework 3
##Miguel Maria Pereira

########################
#Exercise 1
########################
#Here I create an object of class door
#That randomly picks one of the doors for you
#So you don't need to go through that
pickmeadoor<-structure(sample(1:3,1), class="door")
#As we can see, the object is class door
class(pickmeadoor)


#Here I create a function that allows for the creation 
#of objects of class door that are either 1, 2 or 3.
pick.door<-function(doornumber){
  if (is.numeric(doornumber) != TRUE | (doornumber %in% 1:3) == FALSE) {
    return("Please, follow the rules. You can do it. Pick a number from 1 to 3.")}
  else {
    class(doornumber)<-"door"
    return(doornumber)
  }
}

#Test
#Creating object of class door
mydoor<-pick.door(1)
class(mydoor)

#And checking what happens when I pick the exit door
pick.door("exit")

#########################
#Exercise 2/3
#########################
#Here I create a generic function
PlayGame<-function(x) UseMethod("PlayGame")

#Now I tell the generic function what to do with objects of class door
PlayGame.door<-function(d){
  success<-d[[1]]==sample(1:3,1)
  print("Are you ready? Let's open the door.")
  Sys.sleep(2)
  if (success==TRUE){
    print("You nailed it! Congrats!")
  }
  else {
    print("Oooooooooh! Better luck next time.")
  }
}


#And what to do with objects that are not class door
PlayGame.default <- function(x)
{
  print("You screwed up. I do not know how to handle this object.")
}

#Now PlayGame has two methods
methods(PlayGame)


#Testing
PlayGame(mydoor)
PlayGame(1)



######################
#Exercise 4
#######################
#Here I start by setting up a class door whose objects can take a numeric door number
#And add a validation function to signal when the door number doesn't make sense
setClass("door",
         #What the objects can take
         slots=list(doornumber="numeric"),
         
         #A validation function
         validity=function(object){
           if((object@doornumber %in% 1:3) != TRUE) {
             return("There are only three doors. Pick 1, 2 or 3.")
           }
           return(TRUE)
         })

#So now I create a door object with door number 3
newdoor<-new("door", doornumber=3)

#And when you include a number that is not 1,2,3 the validity function kicks in
new("door", doornumber=4)


#Here I set up a new generic from scratch
setGeneric("PlayGameS4", function(x){
  standardGeneric("PlayGameS4")
})

#And finally I create a method for it
#What the method does is randomly picking the door with the car
#And comparing it with the door chosen. It returns a message with the result
setMethod("PlayGameS4",
          c(x="door"),
          function(x){
            validObject(x)
            success<-x@doornumber==sample(1:3,1)
            return(ifelse(success==TRUE,"Congrats!","Bad luck."))
})

#And testing it.
PlayGameS4(newdoor)
