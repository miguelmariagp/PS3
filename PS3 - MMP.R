#Homework 3
##Miguel Maria Pereira

########################
#Exercise 1
########################
#Here I create an object of class door
#That randomly picks one of the doors for you
#So you don't need to go through that
onedoor<-structure(sample(1:3,1), class="door")
#As we can see, the object is class door
class(onedoor)


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
    print("You nailed it! Contrats!")
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
PlayGame(onedoor)
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
(newdoor<-new("door", doornumber=3))

#And when you include a number that is not 1,2,3 the validity function kicks in
new("door", doornumber=4)


#Here I set up a new generic from scratch
setGeneric("PlayGame", function(x){
  standardGeneric("PlayGame")
})

#And finally I create a method for it
#What the method does is randomly picking the door with the car
#And comparing it with the door chosen. It returns a message with the result
setMethod("PlayGame",
          c(x="door"),
          function(x){
            success<-x@doornumber==sample(1:3,1)
            return(ifelse(success==TRUE,"Congrats!","Bad luck."))
})

#And testing it.
PlayGame(newdoor)
