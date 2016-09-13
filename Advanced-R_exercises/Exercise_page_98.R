# 1.
# All the information on in_dir() si deprecated from devtools. I can't seem to find any
# info on it. But I'll trust the distinction made from @peterhurford 

# The R-Doc for `source()` (use `help(source)`) shows that chdir indicates that "the R working directory is
# temporarily changed to the directory containining file" if TRUE, that is whether or not your file is not
# in the current working directory.  On the other hand, `in_dir` (`help(in_dir)`) can save the previous
# working directory whenever a new working directory is set, so that it can still be accessed.

# The methodology used in `in_dir` is more free, allowing more programmer customization, whereas the methodology in
# `chdir` is more constrained, reducing programmer error.  This is generally a trade-off that different people will
# want to take different sides of, usually depending on how much they know what they are doing.

# Taken from: https://github.com/cimentadaj/adv-r-book-solutions/blob/master/04_functions/05_returns/exercise1.r

# 2.

# After loading ggplot2
library(ggplot2)
# you can unload it with the detach function
detach("package:ggplot2")

# How do you save and restore the settings of options() and par()?
options() # is actually a list containing all settings
par() # as well

# Let's save each one
options <- options()
par <- par()

# Change some things:
0.0001/100 # scientific numbering
options(scipen = 999); 0.0001/100 # Disabled
options(options) # changed it back
0.0001/100 # scientific numbering

par()$mfrow # mfrow is 1 1
par(mfrow = c(2, 2)) # change it
par()$mfrow # check it's changed
par(par) # set it back
par()$mfrow # check it's back


# 3. 
# Write a function that opens a graphics device, runs the supplied code,
# and closes the graphics device (always, regardless of whether or not
# the plotting code worked).

graphdv <- function(x, y, data) {
    if (!is.numeric(x) || !is.numeric(y)) stop("both vars need to be numerics")
    require("ggplot2", quietly = T)
    on.exit(detach("package:ggplot2"))
    qplot(x, y, data = data, geom = "point")
}

graphdv(mtcars$mpg, mtcars$hp, mtcars)

# 4.
capture.output2 <- function(code) {
    temp <- tempfile() # Save a temporary file
    on.exit(file.remove(temp), add = TRUE) # When function exists, remove this temporary file
    
    sink(temp) # divert any output to that folder (same as setwd()? )
    on.exit(sink(), add = TRUE) # 
    
    force(code) # run code
    readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))

# You avoid filtering the classes of argument "directory" and simply established the connection
# You specified the on.exit() call earlier in the code, making it more readable and explicit
# You removed the evaluation of the arguments and changing their classes, to simply running the code
