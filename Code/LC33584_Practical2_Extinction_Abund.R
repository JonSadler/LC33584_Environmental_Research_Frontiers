#**********************************************************************************************************
# BIOGEOGRAPHY - PRACTICAL CLASS 4 - Abundance_extinction links
# The background to this is covered in the lecture and also the lab class CANVAS page
# LC33584_Extinction_Abund.R
# Last updated 12th Jan 2024
# Author: Jon Sadler
#**********************************************************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOADING THE DATA INTO R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMEMBER YOU HAVE TO RUN THE CODE FROM TOP TO BOTTOM!!!
# PLEASE export or screenshot the graphics and answer the questions as you get to them in the code

# ------------------------
# LAB BOOK REQUIREMENTS
# Any elements/questions you cover in the practical classes for Weeks 1-2 might be used in the lab book.
# ------------------------

# Load in the libraries we need
# Now were are going to load them into the memory so they are available to you to use.
library(ggplot2)

# PLEASE MAKE SURE THAT YO SET YOUR WORKING DIRECTORY CORRECTLY
# THERE IS GUIDANCE HERE CANVAS SITE ON HOW TO DO THAT.

# READ IN THE DATAFILES:
# There are three - we'll pull them in now.
Butterfly  <- read.csv("Butterfly_trends.csv", header=T)
Bird <- read.csv("Bird_trends.csv", header=T)
Extinction <- read.csv("BirdExtinction.csv", header=T)
#If they import you see them in the 'Environment' window [see top right]

# PART ONE: ABUNDANCE PATTERNS OF THE SPECIALIST AND GENERALIST BUTTERFLIES
# We revisit and update our butterfly story and compare the generalist butterflies
# BUT this time we will check for whether our patterns are likely significant by 
# adding confidence intervals on our plots.
# We've already loaded it in. Have a look at it

# This time we will use the str (means 'structure') function

 str(Butterfly)
# This tells us we have 7 columns
# Year - The year the data were collected. It is an interger [hence 'int']

# Spec_Abund - The abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Spec_Abund_2.5_CI - lower 5% CI of the specialist abundance. It is number [hence 'num]
# Spec_Abund_97.5_CI - Upper 95% CI of the specialist abundance. It is number [hence 'num]

# Gen_Abund - The abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Gen_Abund_2.5_CI - lower 5% CI of the specialist abundance. It is number [hence 'num]
# Gen_Abund_97.5_CI - Upper 95% CI of the specialist abundance. It is number [hence 'num]

# Plot using base R plotting [lots of code]
# 1. Plot first line showing the generalist abundance
plot(Butterfly$Year, Butterfly$Gen_Abund, type = "l",
     col = "blue", xlab = "Year", ylab = "Abundance", lty = 1, lwd = 1, ylim=c(0,100), 
     main = "Time series of abundance of UK butterflies")
# 2. Lower 5% CI in dashed blue
lines(Butterfly$Year, Butterfly$Gen_Abund_5_CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 3. Upper 95% CI in dashed blue
lines(Butterfly$Year, Butterfly$Gen_Abund_95_CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 4. Plot first line Specialist abundance
lines(Butterfly$Year, Butterfly$Spec_Abund, type = "l",
     col = "red", lty = 1, lwd = 1)
# 5. Lower 5% CI in dashed blue
lines(Butterfly$Year, Butterfly$Spec_Abund_5_CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 6. Upper 95% CI in dashed blue
lines(Butterfly$Year, Butterfly$Spec_Abund_95_CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 7. Add a legend to the plot and set legend lty
legend("topright", legend = c("Generalist", "Specialist"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)

#-------------------------------
# QUESTION
#There are three evident patterns here. Please desibe them.

# ANNOTATE YOUR ANSWERS HERE
#
# 1.
#
# 2. 
#
# 3. 

# The confidence intervals on these lines do no overlap. What do you think 
# this means?

#---------------------------------
# ANNOTATE YOUR ANSWERS HERE
#
#
#
#---------------------------------

# PART TWO: ABUNDANCE PATTERNS OF THE SPECIALIST AND GENERALIST BIRDS
# We've already loaded the data "Bird", let's look at it.
str(Bird)

# This tells us we have 16 columns or variables
# Year - The year the data were collected. It is an interger [hence 'int']

# Fm_gen_Abund_Ind - The farmland bird abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Fm_Gen_2_5CI - lower 2.5% CI of generalist farmland bird abundance. It is number [hence 'num]
# Fm_Gen_97_5CI - Upper 97.5% CI of generalist farmland bird abundance. It is number [hence 'num]
# Fm_Spec_Abund_Ind - The abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Fm_Spec_2_5CI - lower 2.5% CI of specialist abundance. It is number [hence 'num]
# Fm_Spec_97_5CI - Upper 97.5% CI of specialist abundance. It is number [hence 'num]

# Wood_Gen_Abund_Ind - The woodland bird abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Wood_Gen_2_5CI - lower 2.5% CI of generalist farmland bird abundance. It is number [hence 'num]
# Wood_Gen_97_5CI - Upper 97.5% CI of generalist farmland bird abundance. It is number [hence 'num]
# Wood_Spec_Abund_Ind - The abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Wood_Spec_2_5CI - lower 2.5% CI of specialist abundance. It is number [hence 'num]
# Wood_Spec_97_5CI - Upper 97.5% CI of specialist abundance. It is number [hence 'num]

# Wet_Abund_Ind - The wetland bird abundance index varies from 100; <100 = decrease in abundance; >100 increase in abundance
# Wet_2_5CI - lower 2.5% CI of wetland bird abundance. It is number [hence 'num]
# Wet_97_5CI - Upper 97.5% CI of wetland bird abundance. It is number [hence 'num]


#---------------------------------------------
# PART 2.1 FARMLAND SPECIALIST V GENERALISTS
# 1. Plot first line showing the farmland generalist abundance
plot(Bird$Year, Bird$Fm_Gen_Abund_Ind, type = "l", 
     col = "blue", xlab = "Year", ylab = "Abundance", 
     lty = 1, lwd = 1, ylim=c(0,125), 
     main = "Time series of abundance of UK Farmland Birds")
# 2. Lower 5% CI in dashed blue
lines(Bird$Year, Bird$Fm_Gen_2_5CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 3. Upper 95% CI in dashed blue
lines(Bird$Year, Bird$Fm_Gen_97_5CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 4. Plot first line Specialist abundance
lines(Bird$Year, Bird$Fm_Spec_Abund_Ind, type = "l",
      col = "red", lty = 1, lwd = 1)
# 5. Lower 5% CI in dashed blue
lines(Bird$Year, Bird$Fm_Spec_2_5CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 6. Upper 95% CI in dashed blue
lines(Bird$Year, Bird$Fm_Spec_97_5CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 7. Add a legend to the plot and set legend lty
legend("bottomleft", legend = c("Generalist", "Specialist"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)


#-------------------------------
# QUESTION
# There are four patterns here. Please describe them.

# ANNOTATE YOUR ANSWERS HERE
# 1.
#
# 2. 
#
# 3. 
#
# 4. 
#
#
#---------------------------------

# PART 2.2 WOODLAND SPECIALIST V GENERALISTS
# 1. Plot first line showing the generalist abundance
plot(Bird$Year, Bird$Wood_Gen_Abund_Ind, type = "l",
     col = "blue", xlab = "Year", ylab = "Abundance", lty = 1, lwd = 1, ylim=c(0,115), 
     main = "Time series of abundance of UK woodland birds")
# 2. Lower 5% CI in dashed blue
lines(Bird$Year, Bird$Wood_Gen_2_5CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 3. Upper 95% CI in dashed blue
lines(Bird$Year, Bird$Wood_Gen_97_5CI, col = "blue", type = "l", 
      lty = 2, lwd = 1)
# 4. Plot first line Specialist abundance
lines(Bird$Year, Bird$Wood_Spec_Abund_Ind, type = "l",
      col = "red", lty = 1, lwd = 1)
# 5. Lower 5% CI in dashed blue
lines(Bird$Year, Bird$Wood_Spec_2_5CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 6. Upper 95% CI in dashed blue
lines(Bird$Year, Bird$Wood_Spec_97_5CI, col = "red", type = "l", 
      lty = 2, lwd = 1)
# 7. Add a legend to the plot and set legend lty
legend("bottomleft", legend = c("Generalist", "Specialist"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)

#-------------------------------
# QUESTION
# We have numerous patterns visible here too. Please describe four of them
# ANNOTATE YOUR ANSWERS HERE
# 1.
#
# 2. 
#
# 3. 
#
# 4. 
#
# Briefly interpret what you think is causing the patterns [2-3 sentences only]





#---------------------------------
# PART 2.3 COMPARISONS OF TRENDS OF FARMLAND AND WOODLAND BIRDS
# We want both of our farmland and woodland bird plots in one image. 
# You can instruct R to do this using the mfrow() function
# This tells R to make a graphics window that is 1 row by 2 columms in this case. 

op <- par(mfrow = c(1, 2)) # stores the graphics parameters in the variable op.

# Copy farmland bird code in from above, [YOU NEED ALL THE CHUNK]. 
# Paste it in below and run it a line at a time 




# copy in woodland bird plot code from above, and repeat


# put the screen back to one graph per screen

par(op) # sets the graphics parameters to default.
#-------------------------------
# QUESTION
# Describe the main differences between farmland and woodland birds
# ANNOTATE YOUR ANSWERS HERE

# Differences
#
# 
#
# Similarities 
#
# 
#
#

#
# Briefly interpret what you think is causing the similarities and differences [2-3 sentences only]

#---------------------------------

# PART 3:  A TIMESERIES OF GLOBAL BIRD EXTINCTIONS

# We'll use ggplot to plot the data
# We've already imported the data and we called it "Extinction"
#Let's look at it

View(Extinction)

# It's got three columns
# Year - this is the year of extinction
# N - is the number of known extinctions in a year
# Location - where the extinctions took place - either on islands or continents

# Code to create barchart. We will use ggplot2 to do this.
ggplot(data=Extinction, aes(x=Year, y=N, fill=Location)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Year of Extinction") +
  ylab("Number of Extinctions")

# If you want to save this plot click on 'Export' from the 'Plots' window [bottom right of the sButterflyeen]. 
# Select the option you want. If you save it as a PNG or PDF you can scale it
# It will place it in your current working directory.
# OR you can copy it to clipboard and paste it into a wordfile

#-------------------------------
# QUESTION
# Describe the main differences extinction rates on continents and islands
# ANNOTATE YOUR ANSWERS HERE

# 1.
#
#
# 2.
#
#
# 3.
# 
#
#---------------------------------
