require(beepr)

timer <- function(mins, sound = 8) {
  
total_mins <- mins * 60
timeisup <- Sys.time() + total_mins

while(Sys.time() < timeisup) {
# do nothing  
}

beep(sound)
}