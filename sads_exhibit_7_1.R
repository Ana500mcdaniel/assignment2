# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

######### STUDENT'S SUMMARIZATION ##################
#.  1. Load the data and store it in a data frame
#   2. Understand the overall structure of the data, data type, colomns, rows etc.
#.  3. Data Dummification, cleaning --- transformation process. 
#.  4. Data visualization
#.  5. Model training and testing

library(car)  # special functions for linear regression.        ## STUDENT COMMENT ## Imported CAR for regression modeling
library(lattice)  # graphics package       ## STUDENT COMMENT ## Imported lattics, a high level data visualization tool with features to visualize 
#                                                                    data even in various non standard needs

# read in data and create a data frame called dodgers.     
dodgers <- read.csv("dodgers.csv")    ## STUDENT COMMENT ##   read.csv(...), reading csv as a dataframe and assigning it to a variable dodgers
print(str(dodgers))  # check the structure of the data frame    ## STUDENT COMMENT ## str is used to display structure of the data frame (DF) 

# define an ordered day-of-week variable 
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers, ## STUDENT COMMENT ## from dodgers, reading the data and storing the categorical values as numerical values
  ifelse ((day_of_week == "Monday"),1,            ## STUDENT COMMENT ## using ifelse to locate categorical values and assigning numerical values in increasing order
  ifelse ((day_of_week == "Tuesday"),2,           ## STUDENT COMMENT ## i.e Monday = 1, Tuesday = 2, so on and so forth
  ifelse ((day_of_week == "Wednesday"),3,
  ifelse ((day_of_week == "Thursday"),4,
  ifelse ((day_of_week == "Friday"),5,
  ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,   ## STUDENT COMMENT ## ==> storing days-in-week as a factor type variable
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

# exploratory data analysis with standard graphics: attendance by day of week
with(data=dodgers,plot(ordered_day_of_week, attend/1000, ## STUDENT COMMENT ## ==> plotting data to visualize 
xlab = "Day of Week", ylab = "Attendance (thousands)",  ########################.  labeling x and y axi
col = "violet", las = 1))

# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday  ## STUDENT COMMENT ## ==> Creating a new Table with bobblehead and ordered day of the week as rows and colomns

# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,        ## STUDENT COMMENT ## ==> using the data from dodgers' DF and manipulating it using the conditional ifelse function   
  ifelse ((month == "APR"),4,                      ## STUDENT COMMENT ## ==> assigning numerical values to categorical variables i.e months (dummy variable concept)
  ifelse ((month == "MAY"),5,
  ifelse ((month == "JUN"),6,
  ifelse ((month == "JUL"),7,
  ifelse ((month == "AUG"),8,
  ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,  ## STUDENT COMMENT ## ==> Storing months as vector (factor data type) of integers in orderly format
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month",  ## STUDENT COMMENT ## ==> plotting data to visualize. labeling x-axis as ordered month y-axis, attendence
ylab = "Attendance (thousands)", col = "light blue", las = 1))

# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data
group.labels <- c("No Fireworks","Fireworks")         ## STUDENT COMMENT ## ==> combining labels 
group.symbols <- c(21,24)                             ## STUDENT COMMENT ## ==> using c function to combine the symbols, pch in a group
group.colors <- c("black","black")                    ## STUDENT COMMENT ## ==> using c function to combine 2 variables (colors)
group.fill <- c("black","red")                        ## STUDENT COMMENT ## ==> using c function to group 2 fill color values in a vector 
xyplot(attend/1000 ~ temp | skies + day_night,        ## STUDENT COMMENT ## ==> visualizing data using lattice's xyplot for a scatterplot
    data = dodgers, groups = fireworks, pch = group.symbols, # ## STUDENT COMMENT ## ==> pch for type of points, dataframe specified as data, and 
    aspect = 1, cex = 1.5, col = group.colors, fill = group.fill, 
    layout = c(2, 2), type = c("p","g"),
    strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),  ## STUDENT COMMENT ## ==> to draw strip along the plot, customizing it's value
    xlab = "Temperature (Degrees Fahrenheit)",        ## STUDENT COMMENT ## ==> labeling x-axis
    ylab = "Attendance (thousands)",                  ## STUDENT COMMENT ## ==> labeling y-axis
    key = list(space = "top",                         
        
        ## STUDENT COMMENT ## ==> Adding feature characterstics like space or margin, text, col, fill etc.
        text = list(rev(group.labels),col = rev(group.colors)),
        points = list(pch = rev(group.symbols), col = rev(group.colors),
        fill = rev(group.fill))))                  
# attendance by opponent and day/night game
group.labels <- c("Day","Night")        ## STUDENT COMMENT ## ==> using labels and combining them in a group using c
group.symbols <- c(1,20)                ## STUDENT COMMENT ## ==> Grouping pch- type of points
group.symbols.size <- c(2,2.75)         ## STUDENT COMMENT ## ==> Grouping, points' size
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night,    ## STUDENT COMMENT ## ==> plotting a series of vertical box and whisker plots for opponents game
    xlab = "Attendance (thousands)",    ## STUDENT COMMENT ## ==> labeling x axis with attendance
    panel = function(x, y, groups, subscripts, ...)       ## STUDENT COMMENT ## ==> panel functions are used for 3D visualization to view the relationship of different variable 
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)        ## STUDENT COMMENT ## ==> defining features of the panels and 
        panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
        cex = group.symbols.size, pch = group.symbols, col = "darkblue")
       },
       
    ## STUDENT COMMENT ## ==> Adding feature characterstics like space or margin, text, col, fill etc.
    key = list(space = "top",            
    text = list(group.labels,col = "black"),
    points = list(pch = group.symbols, cex = group.symbols.size, 
    col = "darkblue")))
     
# employ training-and-test regimen for model validation
set.seed(1234) # set seed for repeatability of training-and-test split    ## STUDENT COMMENT ## set.seed used for generating sequence of random numbers that can be reproduced later
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),   ## STUDENT COMMENT ## closest integer to 2/3rd of the total number of rows in droger DF, will be the size of our training dataset
rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))    ## STUDENT COMMENT ## remainder of the data will be used for model testing and validation
dodgers$training_test <- sample(training_test) # random permutation  ## STUDENT COMMENT ## Using sample to randomize the sample size for the training data 
dodgers$training_test <- factor(dodgers$training_test, 
  levels=c(1,2), labels=c("TRAIN","TEST"))    ## STUDENT COMMENT ## labeling the TRAIN and TEST, the categorical values to numerical values for data plotting in future
dodgers.train <- subset(dodgers, training_test == "TRAIN")  ## STUDENT COMMENT ## storing  data with training_test values = TRAIN in training dataset
print(str(dodgers.train)) # check training data frame       ## STUDENT COMMENT ## checking the structure of training data
dodgers.test <- subset(dodgers, training_test == "TEST").   ## STUDENT COMMENT ## storing  data with training_test values = TEST in training dataset
print(str(dodgers.test)) # check test data frame            ## STUDENT COMMENT ## checking the structure of testing data

## STUDENT COMMENT ##      There are two parts to modeling:

#.                              1. Defining the Family of models
#.                              2. Generating a fitted model

# specify a simple model with bobblehead entered last

## STUDENT COMMENT ## Defined a simple model equation with "attendance" as dependent variable and month, day-of-week, and bobblehead distribution as independent

my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead} 
# fit the model to the training set

## STUDENT COMMENT ## Using lm function to fit a linear model
train.model.fit <- lm(my.model, data = dodgers.train)
# summary of model fit to the training set
print(summary(train.model.fit))
# training set predictions from the model fit to the training set

## STUDENT COMMENT ## predict is a method specifically for lm function predictions. Here it uses the model generated from the training dataset to make predictions
## about testing data before we compare the results.
dodgers.train$predict_attend <- predict(train.model.fit) 
# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit, 
  newdata = dodgers.test)

# compute the proportion of response variance
# accounted for when predicting out-of-sample
## STUDENT COMMENT ## calculating the proportion of response variance from test data and prediction
cat("\n","Proportion of Test Set Variance Accounted for: ",
round((with(dodgers.test,cor(attend,predict_attend)^2)),
  digits=3),"\n",sep="")
# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test) ## STUDENT COMMENT ## Using rbind to merge training and test data

# generate predictive modeling visual for management
group.labels <- c("No Bobbleheads","Bobbleheads")   ## ## STUDENT COMMENT ## grouping labels to see the predicted effect bobbleheads on attendence w.r.t no bobbleheads
group.symbols <- c(21,24) ## STUDENT COMMENT ## combining pch symbole of points
group.colors <- c("black","black") ## STUDENT COMMENT ## grouping colors 
group.fill <- c("black","red")  ## STUDENT COMMENT ## grouping fills
xyplot(predict_attend/1000 ~ attend/1000 | training_test,  ## STUDENT COMMENT ## plotting scatterplot of predicted attendence with bobblehead
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
            {panel.xyplot(x,y,...)
             panel.segments(25,25,60,60,col="black",cex=2)
            },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)",         ## STUDENT COMMENT ## labeling x axis
       ylab = "Predicted Attendance (thousands)",       ## STUDENT COMMENT ## labeling y axis
       key = list(space = "top", 
              text = list(rev(group.labels),col = rev(group.colors)),
              points = list(pch = rev(group.symbols), 
              col = rev(group.colors),
              fill = rev(group.fill))))                   
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
my.model.fit <- lm(my.model, data = dodgers)  # use all available data.   ## STUDENT COMMENT ## Using the model on the entire dataset of 2012 to predict the attendence
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  
cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
digits = 0),"\n",sep="")
# standard graphics provide diagnostic plots
plot(my.model.fit)
# additional model diagnostics drawn from the car package
library(car)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))


