xx <- list("01" = list(c("A","B"), "C"),
          "02" = list("C"),
          "03" = list("B", "B"))
## coerce
s <- as(xx, "sequences")
as(s, "data.frame")
## get reference set
as(itemsets(s), "data.frame")



similarity(x, y=NULL, method = c("jaccard", "dice", "cosine", "subset"), strict = FALSE)


dfX


summary(x)

# xt <- as(x, "sequences")

similarity(x@data[1:5,], method = "jaccard", which="items")


data(zaki)
z <- as(zaki, "timedsequences")
similarity(z)

# require equality
similarity(z, strict = TRUE)
## emphasize common
similarity(z, method = "dice")
##
is.subset(z)
is.subset(z, proper = TRUE)




## S4 method for signature 'sequences'
similarity(x, y = NULL,
           method = c("jaccard", "dice", "cosine", "subset"),
           strict = FALSE)

## S4 method for signature 'sequences'
is.subset(x, y = NULL, proper = FALSE)
## S4 method for signature 'sequences'
is.superset(x, y = NULL, proper = FALSE)














summary(Income)
dfIncome <- data.frame(Income)
data("Income")
## find and some rules (we only use 5 rules here) and calculate coverage
rules <- apriori(Income)[1:5]

quality(rules) <- cbind(quality(rules), coverage = coverage(rules))
inspect(rules)



