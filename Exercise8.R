#Question 1
#read data
scorefile <- read.table('UWvMSU_1-22-13.txt',header = TRUE)

#function to generate score file for each team
teamscore <- function(team)
{
  score <- scorefile[which(scorefile$team == team), ]
  filename <- paste(team,'.csv')
  write.table(score,filename,sep=',')
}

teamscore('UW')
teamscore('MSU')

#function to calculate the total score as a function of time for each team
count_teamscore <- function(team)
{
  sum <- numeric(length(team$score))
  for (i in 1:length(team$score))
  {
    sum[i] <- sum(team$score[1:i])
  }
  return(sum)
}

#append cumulative score to UWscore and MSUscore
UWscore <- read.csv('UW .csv',header = TRUE,sep =',')
MSUscore <- read.csv('MSU .csv',header =TRUE,sep=',')
UWscore$sum <- count_teamscore(UWscore)
MSUscore$sum <- count_teamscore(MSUscore)

#plot
UWx <- UWscore$time
UWy <- UWscore$sum
MSUx <- MSUscore$time
MSUy <- MSUscore$sum

plot(UWx,UWy,type='l',xlab='time',ylab='score',col='red')
lines(MSUx,MSUy)




#Question 2
#function to read input from user
readinput <- function()
{
  guess <- as.integer(readline(prompt="Guess: "))
  return(guess)
}
#generate random number
random <- c(1:100)
print("I'm thinking of a number 1-100\n")
mynumber <- sample(random,1)
guessN <- 0
maxGuesses <- 10

#real program
if(guessN != mynumber)
{ 
  i <- 0
#while loop to count trials
  while(i < maxGuesses)
  {
  guessN <- readinput()
#coditional structure 
  if (guessN > mynumber)
    {
    print("Higher")
    i <- i+1
    }
    else if (guessN < mynumber)
      {
        print("Lower")
        i <- i+1
      }
      else
        {
      print("Correct!")
      break
        }
  }
}  



