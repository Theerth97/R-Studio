# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Manish","Rohan","Mac","Ryan","Kaushik"),
  salary = c(6723.3,3515.2,2611.0,7329.0,8343.25), 
  
  start_date = as.Date(c("2020-01-01", "2011-09-23", "2018-11-15", "2016-05-11",
                         "2015-03-03")),
  stringsAsFactors = FALSE
)
# Print the data frame.   
print(summary(emp.data))

result <- data.frame(emp.data$emp_name,emp.data$salary)
print(result)
#Extract the first two rows and then all columns
# Extract first two rows.
result <- emp.data[1:2,]
print(result)

# Extract 3rd and 5th row with 2nd and 4th column.
result <- emp.data[c(3,5),c(2,4)]
print(result)
emp.data$dept <- c("IT","Purchase","HR","HR","Finance")
v <- emp.data
print(v)
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("A","B","C","D","E"),
  salary = c(6323.3,5315.2,3611.0,3729.0,8343.25), 
  
  start_date = as.Date(c("2015-01-01", "2020-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-25")),
  dept = c("IT","HR","IT","HR","Finance"),
  stringsAsFactors = FALSE
)

# Create the second data frame
emp.newdata <-  data.frame(
  emp_id = c (6:8), 
  emp_name = c("X","Y","Z"),
  salary = c(5578.0,5722.5,6352.8), 
  start_date = as.Date(c("2015-05-21","2015-07-30","2014-06-17")),
  dept = c("IT","Operations","HR"),
  stringsAsFactors = FALSE
)

# Bind the two data frames.
emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)
# Assignment using equal operator.
var.1 = c(0,1,2,3,4,5)           

# Assignment using leftward operator.
var.2 <- c("learn","R","and","analytics")   

# Assignment using rightward operator.   
c(TRUE,1) -> var.3           

print(var.1)
cat ("var.1 is ", var.1 ,"\n")
cat ("var.2 is ", var.2 ,"\n")
cat ("var.3 is ", var.3 ,"\n")
