require(data.table)
## simple example:
x = data.table(start=c(1,10,15,35), end=c(9,14,30,40)) #ranges
y = data.table(start=c(11, 20, 32), end=c(11, 20, 32), val1 = letters[8:10]) #actual values
x
y

setkey(y, start, end)
foverlaps(x, y, type="any", which=TRUE) ## return overlap indices
foverlaps(x, y, type="any") ## return overlap join
foverlaps(x, y, type="any", mult="first") ## returns only first match
foverlaps(x, y, type="within") ## matches iff 'x' is within 'y'

dt.g <- data.table(diag.g) #rename these 
# dt.g <- dt.g[,c(1,2,5,6)]
# d1 <- dt.g[,c(2,3,4)]
# names(d1) <- c("val1","start","end")
setkey(dt.g,Start.n,End.n)

dt.ed05 <- data.table(ed05.df)
dt.ed05[, Code.n2 := Code.n] #create a range of zero for overlap
#names(d2) <- c("valdiag","valfreq","Code","start","end")
# setkey(d2)
setkey(dt.ed05)
# x then y in foverlaps
#foverlaps(d2, d1, by.y = c("start","end"), by.x = c("start","end"), type = "any", mult="all", nomatch = 0)
foverlaps(dt.ed05, dt.g, by.y = c("Start.n","End.n"), by.x = c("Code.n","Code.n2"), type = "any", mult="all", nomatch = 0L)
