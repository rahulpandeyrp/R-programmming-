
#q1


testdf <- data.frame(
  row.names=c("Jack", "Rosa", "Dawn", "Vicki", "Blake", "Guillermo"),
  age=c(24, 23, NA, 25, 32, 19),
  city=c("Harlem", NA, "Queens", "Brooklyn", "Brooklyn", NA),
  gpa=c(3.5, 3.6, 4.0, NA, 3.8, NA))


count_na <- function (testdf,byrow=TRUE)
{
  if(byrow==FALSE)
    colSums(is.na(testdf))
  else
    rowSums(is.na(testdf)) 
}
count_na(testdf,FALSE)



#q2

testdf <- data.frame(
  row.names=c("Jack", "Rosa", "Dawn", "Vicki", "Blake", "Guillermo"),
  age=c(24, 23, NA, 25, 32, 19),
  city=c("Harlem", NA, "Queens", "Brooklyn", "Brooklyn", NA),
  gpa=c(3.5, 3.6, 4.0, NA, 3.8, NA))

my_mode <- function(x) {# Create mode function 
  x <- na.omit(x)
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
  
}

#print(my_mode(testdf[,"city"]))   na and brooklyn

imputedf <- function(testdf,usemean=FALSE)
{
  y <- colnames(select_if(testdf,is.numeric))
  z <- colnames(select_if(testdf, is.factor))
  z <- append(z, colnames(select_if(testdf, is.character)))
  if(usemean==TRUE)
  {
    for(i in y)
    {
      testdf[is.na(testdf[,i]), i] <- mean(testdf[,i], na.rm = TRUE)
    }
  }
  
  else
  {
    for(i in y)
    {
      testdf[is.na(testdf[,i]), i] <- median(testdf[,i], na.rm = TRUE)
    }
  }
  for(i in z)
  {
    testdf[is.na(testdf[,i]), i] <- my_mode(testdf[,c(z)])
  }
  
  
  return(testdf)
}
imputedf(testdf,TRUE)

#q3
ggplot(congress_age,aes(x=congress,y=age,group=congress))+ geom_boxplot()+facet_grid(~chamber)


#q5

ggplot(bechdel,aes(x=budget_2013,y=intgross_2013,color=binary))+ geom_point()+geom_smooth()

#q4

police_killing_a <- na.omit(police_killings)
police_killing_a$h_quintile <- ntile(police_killing_a$h_income, 5)
ggplot(police_killing_a,aes(x=raceethnicity))+geom_bar()+facet_grid(~h_quintile)





