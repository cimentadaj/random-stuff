# 1.
structure(1:5, comment = "my attribute")
# No, it's not missing, it simply doesn't print it, but sets is.

x <- structure(1:5, comment = "my attribute")
attributes(x) # will show you the attributes

# 2.
f1 <- factor(letters)
levels(f1) <- rev(levels(f1))
# You're reversing the factor levels, but that also reverses the element order
# in the f1 vector
# Remember that you can't add a new element unless it is already
# specified in the levels.

# 3.
# In f1 the levels are reversed, whereas in f2 only the actual elements in f2 are reversed
# the levels are still in the original order
f2 <- rev(factor(letters))

# In f1 both the levels and the elements are reversed, but here only the levels
# are reversed
f3 <- factor(letters, levels=rev(letters))

# Key take away:
# If you reverse the element order in a factor, levels are not affected
# When creating a factor, if the specified levels have a different order than the elements
# nothing will be changed in the elements. But if the levels attribute is set
# with a different order after the factor is created, the order of the elements will be changed
