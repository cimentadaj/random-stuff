
## xor will always give a TRUE with one of the two arguments is TRUE and the other is FALSE.
xor(1,0)
xor(TRUE,FALSE)
xor(FALSE,FALSE) # will give false
xor(0,0)

## you can also compare paired vectors
xor(c(1,2,0),c(0,1,0))


## what would happen if the vectors had unequal lengths?
xor(c(1,2,1),c(0,1))
## it gives a TRUE even when the vector is unequal(though, gives you a warning)

# the function isTRUE tests if the object is an object of length 1 that has a TRUE 
isTRUE(TRUE)
isTRUE(c(TRUE,FALSE))
isTRUE(c(TRUE,TRUE))
isTRUE(FALSE)
isTRUE(list(TRUE)) ## even if there's a true inside the list, the functions tests for a vector


if(print(2)) {print(5)} else {print(3)}
## The && evaluates the first argument and if it's not TRUE then does not evaluate the second argument
if(TRUE && print(1)) {print(2)} else {print(3)} # here the first arg is TRUE, so it prints the second condition

if(FALSE && print(1)) {print(2)} else {print(3)} # here the first arg is FALSE so it jumps the first condition to the else
# statement

# it is exactly the same for the | (OR) operator
if (TRUE | a) {print(1)} else {print(3)} # first one is TRUE, but a doesn't exist, so it gives an error
if (TRUE || a) {print(1)} else {print(3)} # do exactly the same but with the || operator and it evaluates the first one only
if (FALSE || a) {print(1)} else {print(3)} # remember that the key thing with the | object is a TRUE object. The first
# is FALSE so it goes to evaluates the second and finds something that doesn't exists

all(c(TRUE,FALSE))

setequal(c(1,2,3,4),c(2,3,5,6)) ## I think setequals is to compare if two sets are equal, which I THINK is the same as identical and all.equal
setequal(F,F)
identical(F,F)
all.equal(F,F)

match(c(1:10),c(10:19)) ## it matched the 1st number in the 10th position
match(c(1:11),c(10:20)) ## here it matched the 1st and 2nd number in the 10th and 11th position
set.seed(1)
match(sample(1:100,20),sample(1:100,20)) ## here it matched the 6th number to the 2nd position, the 16th number to the third
# position, and so on..

for (i in 1:6) {
    assign(paste0("hi.",i),1:i)
}

a <- 1:4
assign("a[1]", 2) ## You can't subset with assign but it creates another vector
a[1] == 2          # FALSE
get("a[1]") == 2   # TRUE because there's a new vector names "a[1]". Type ls() to see.


?all.equal
all.equal(0.25,0.2) # They are not exactly identical but NEARLY equal
all.equal(0.25,0.2, tolerance=1) # Specify the tolerance as a criteria for equality. If there's a 1, it means
# that any difference smaller than 1 are not reported and thus a TRUE.

if (isTRUE(all.equal(0.25,0.25))) { # Here, naturally, either use isTRUE, add the tolerance argument or use identical
    print("Yes, it's TRUE!") 
}

pl <- list(hey=0.25, bo=T, pol=0.33)
pl2 <- list(hey=0.2,bo=T, pol=0.45)
all.equal(pl,pl2, tolerance=0) ## Here, I wanna see the exact matching and assign tolerance to 0.

##
# The %% operator gives you the residual of a division
# 10/2 is 5 so 0 residual
10 %% 5
6 %% 4 ## residual is 2
c(3,2,1,4,5,6) %% c(3,4,2,1,2,4) ## it operates on each space of a vector
1:12 %% 2

# For example, a prime number is one which is only divisible by another positive number
# and by itself.

prime_detector <- function(x) {
    if (x > 0) {
        ifelse(sum(x %% 1:x == 0) == 2 , "Yes, prime!","Nop, no luck")
    } else {
        message("all numbers must be positive!")
    }
}

primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349)
notprimes <- notprimes[!notprimes %in% primes]

for (i in primes) {
    print(prime_detector(i))
}


##
# %/% is simply the division operator but rounding up to the closest interger
vec1 <- 10:19
vec2 <- 1:10
try1 <- 0
try2 <- 0
for (i in 1:length(vec2)) {
    try1[i] <- vec1[i]/vec2[i]
    try2[i] <- vec1[i]%/%vec2[i]
}
data.frame(t=round(try1,2),m=try2) ## here you can see the difference

##
# sign() tells you the sign of all the elements of a vector: 1 for positive, -1 for negative and 0
# for neutral.

x <- -2:3
sign(x)
if (table(sign(x))[3] > 2) message("There's more than 2 positives")

##
# ceiling() and floor() are really simple functions. The first one
# rounds up to the highest number and floor to the lowest number

ceiling(0.5) # should yield a 1 because it rounds up to the highest number
floor(0.5) # should yield a 0 because it rounds up to the lowest number
# Its as simple

##
# trunc() does basically the same as floor. It rounds up to the l
for (i in rnorm(100, 1:100)) { ## If all the numbers are positive, then they are the same
    print(floor(i) == trunc(i))
}
set.seed(1)
for (i in rnorm(100, -50:50)) { ## For negative numbers it's different. 
    print(paste0(round(i,2), " is floor = ", floor(i), " and trunc is = ", trunc(i)))
} 
## As you can see, trunc ALWAYS rounds up towards ZERO, regardless of the sign.
## Floor takes the number and rounds up to the highest number BELOW the current number.
## So for the first example, it was -50.63. The highest number BELOW -50.63 is -51 whereas
## trunc() rounds up to the closest to 0 which would be 50

# prod
prod(5,4)
prod(5,5)
# It's the same as the * operator

# pmin and pmax:
# takes two vector arguments and compares each position to get either the min or the max
# pmax stands for parallel max
exam1 <- sample(50:100,50) # Students take exam1
exam2 <- sample(50:100,50) # Students take exam2
pmax(exam1, exam2) # This will give the highest score of both tests for each student
pmin(exam1, exam2) # Similarly, this will give the smaller number

# rle()
# rle stands for "Run Length Encoding". Basically, rle is a cumulative cout of consecutive
# occurrences. If I have vector c(T,T,F,T,T,F,F), rle will throw 2,1,2,2, so the first number
# is repeated twice, the third number only once and so on..
# Let's confirm it!
rle(c(T,T,F,T,T,F,F)) # There you go: 2,1,2,2 with its respective values
# The usefulness of this function is when wanting to record streaks, like coin flips
# or in longitudinal data.

# Suppose they asked someone if they were following the elections each year
(x <- rle(c(T,T,T,T,T,F,F,T,F,T))) # We might conclude that the first 5 years were pretty active because
# the person had a streak of 5 TRUES
str(x)
tapply(x$lengths, x$values,max) # The highest TRUE streak was of 5 and 2 for false.

# missing()
# This function is pretty simple: it tests whether the argument of a function
# is missing.

meantwo <- function(x,y) {
    if(missing(y)) return(x)
}
meantwo(5)
# If we tried is.na instead, it wouldn't have worke.d
meantwo <- function(x,y) {
    if(is.na(y)) return(x)
}
meantwo(5)

# on.exit()
# on.exit() purpose is to reset some options or parameter, normally within a function, regardless
# if there was an error or not. This is useful when attempting risky behaviour (http://stackoverflow.com/questions/28300713/how-and-when-should-i-use-on-exit)
# or when setting graphical parameters.

# This function will produce an error. See that I set par to be different from the baseline. 
# Regardless of the possibility of an error, on.exit() will set the parameter back to the 
plot_with_big_margins <- function(...) {
    old_pars <- par("mfrow","mar") # Save old parameters
    on.exit(par(old_pars)) # On exit, reset these parameters
    
    par(mar= c(10,9,9,7)) # Change parameters
    plot(...) # Plot
}

plot_with_big_margins(with(cars, speed, dist))
with(cars, plot(speed, dist))

## Let's throw in an error
plot_with_big_margins <- function(...) {
    old_pars <- par("mfrow","mar") # Save old parameters
    on.exit(par(old_pars)) # On exit, reset these parameters
    
    par(mar= c(10,9,9,7)) # Change parameters
    print(par()$mar)# confirm parameters changed
    plot(s) # Error
}

plot_with_big_margins(with(cars, speed, dist)) # Error, but the parameters changed before the error
with(cars, plot(speed, dist)) # Parameters were restored even if there's an error

# on.exit() is usually applied for changing directories, changing graphical parameters, database connections,
# file connections.

# invisible()
f1 <- function(x) x
f2 <- function(x) invisible(x)
f1(1)  # prints
f2(1)  # does not
# But if you save the function to an object name and call it, it will print the 1.

# which()
# which() outputs the position number in a vector of observations that meets a logical condition
x <- rep(c(F,T), each=10)
which(x) # Position of all the TRUE's
which(x == F) # Position of all the FALSE's
which(x < 1) # Also with numbers

# diag()
# the diag function creates a matrix and fills the diagonal with the values specified in
# x

diag(5) # If you don't specify the nrow and ncol the function assigns 5 rows and 5 columns with a 1 as a diagonal
diag(1:2) # If length is > 1 it automatically sets nrow and ncol to length of vector and assigns the vector as the diagonal
diag(1:10, 3,4) # If the vector is longer than the dimensions, the matrix cuts the vector

# sweep()
# Sweeps applies an operation over columns or rows
A <- matrix(1:4,2,2)
sweep(A, 2,2,"-") # it subtracts the number 2 from each column
A <- matrix(1:10,2,5)
# You can also apply different things to different columns(this is the difference between apply and sweep)
sweep(A, 2, c(0.5,0.6,0.7,1,3), "+")
# each column is added each of the numbers

# rep_len()
# rep_len is a faster way of using rep(). It takes two arguments: the vector and the length. The function will repeat until
# reaches the length
rep_len(c(1,2,3),10)

#seq_len()
# This function simply creates a sequence from 1 to the specified length.
seq_len(10)
seq_len(20)
# I don't see the use; simply use the semi colon
1:10

#seq_along()
# This function will output a sequence of the length of the vector(regardless of the number inside)
seq_along(c(5,6,2) # will output 1:3
seq_along(rnorm(5)) # will output 1:5
seq_along(100) # will be the same as seq_len(1)
