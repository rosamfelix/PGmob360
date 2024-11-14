# basics

1+1
3*3
1594-1254

1:10
round(3.14)
round(pi, 5)
 b = 1+2*(6-4)
 
 c(1,2,3)
 c(1:3)
 
 tabela = data.frame(a = 1:10,
                     b = cumsum(1:10))
 
tabela$a
tabela$b

modo <- c("car", "bike", "walk")
viagens = c(1000, 50, 200)

tab = data.frame(modo, viagens)
View(tab)

# linha - coluna
tab[1,]
tab[,1]
tab[1,1]


TABLE = readRDS("data/TRIPSmun.Rds")
head(TABLE)
ncol(TABLE)
nrow(TABLE)
names(TABLE)

str(TABLE)

sum(TABLE$Total)

summary(TABLE)
summary(TABLE$Car)

mean(TABLE$Walk)

hist(TABLE$Total, breaks = 50)

plot(TABLE$Total)
plot(TABLE$Car, TABLE$Total)
# save image

sum(TABLE$Car) / sum(TABLE$Total)

(sum(TABLE$Walk) + sum(TABLE$Bike)) / sum(TABLE$Total)


# export
write.csv(TABLE, "data/dataset.csv") #row.names = FALSE
saveRDS(TABLE, "data/TABLE.Rds")

# import
csv_file = read.csv("data/dataset.csv")
rds_file = readRDS("data/dataset.Rds")