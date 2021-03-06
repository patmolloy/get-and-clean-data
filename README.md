# Getting and Cleaning Data Course Project (Coursera)

https://www.coursera.org/course/getdata

January 2015

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.
You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit:

1. a tidy data set as described below
2. a link to a Github repository with your script for performing the analysis
3. a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md.

You should also include a README.md in the repo with your scripts. This README.md explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

* You should create one R script called run_analysis.R that does the following. 
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## How to create the tidy data set

1. set your working directory as required (setwd)
2. download [zipped raw data and other files](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
3. unzip the file downloaded at (2) into a sub-directory of the working directory called UCI_HAR_Dataset (maintaining the subdirectory structure)
4. open a R console (or R-Studio) with setwd as at 1 above. This directory needs to contain run_analysis.R (and the sub directory UCI_HAR_Dataset)
5. source run_analisys.R script (it requires the plyr package): `source('run_analysis.R')`

The file `sensor_averages.txt` is created in the working directory, and contains the tidy data set.

A successful execution produces the following output

    > source('~/R-Coursera/GetAndCleanData/run_analysis.R')
    ... loading data
    ... merging data
    ... selecting only required columns
    ... tidying variable names
    ... creating output in sensor_averages.txt



The original data for the project requires the following reference ..

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.