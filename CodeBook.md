Code Book
========

Raw data collection
-------------------

### Collection

Raw data are obtained from UCI Machine Learning repository. In particular we used
the *Human Activity Recognition Using Smartphones Data Set* [[1](#uci-har)],
that was used by the original collectors to conduct experiments exploiting
Support Vector Machine (SVM) [[2](#har-smart)].

Activity Recognition (AR) aims to recognize the actions and goals of one or more agents
from a series of observations on the agents' actions and the environmental conditions
[[3](#activity-recognition)]. The collectors used a sensor based approach employing
smartphones as sensing tools. Smartphones are an effective solution for AR, because
they come with embedded built-in sensors such as microphones, dual cameras, accelerometers,
gyroscopes, etc.

The data set was built from experiments carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six activities
(walking, walking upstairs, walking downstairs, sitting, standing, laying)
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded
accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity
were captured at a constant rate of 50Hz. The experiments have been video-recorded
to label the data manually [[4](#har-smart2)].

The obtained data set has been randomly partitioned into two sets, where 70% of
the volunteers was selected for generating the training data and 30% the test data.

### Signals

The 3-axial time domain [[5](#time-domain)] signals from accelerometer and gyroscope
were captured at a constant rate of 50 Hz [[6](#hertz)]. Then they were filtered
to remove noise.
Similarly, the acceleration signal was then separated into body and gravity
acceleration signals using another filter.
Subsequently, the body linear acceleration and angular velocity were derived in time
to obtain Jerk signals [[7](#jerk)]. Also the magnitude [[8](#magnitude)] of these
three-dimensional signals were calculated using the Euclidean norm [[9](#euclidean-norm)]. 
Finally a Fast Fourier Transform (FFT) [[10](#fft)] was applied to some of these
time domain signals to obtain frequency domain [[11](#freq-domain)] signals.

The signals were sampled in fixed-width sliding windows of 2.56 sec and 50% 
overlap (128 readings/window at 50 Hz).
From each window, a vector of features was obtained by calculating variables
from the time and frequency domain.

The set of variables that were estimated from these signals are: 

*  mean(): Mean value
*  std(): Standard deviation
*  mad(): Median absolute deviation 
*  max(): Largest value in array
*  min(): Smallest value in array
*  sma(): Signal magnitude area
*  energy(): Energy measure. Sum of the squares divided by the number of values. 
*  iqr(): Interquartile range 
*  entropy(): Signal entropy
*  arCoeff(): Autoregression coefficients with Burg order equal to 4
*  correlation(): Correlation coefficient between two signals
*  maxInds(): Index of the frequency component with largest magnitude
*  meanFreq(): Weighted average of the frequency components to obtain a mean frequency
*  skewness(): Skewness of the frequency domain signal 
*  kurtosis(): Kurtosis of the frequency domain signal 
*  bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT
   of each window.
*  angle(): Angle between some vectors.

No unit of measures is reported as all features were normalized and bounded
within [-1,1].

The original raw data contains 10299 rows/instances (2947 in test, 7352 in train)
There are 30 subjects (30 subject IDs)
There are 561 attributes

Data transformation
-------------------

The raw data sets are processed with run_analisys.R script to create a tidy data
set [[12](#tidy-dataset)]. The Tidy dataset is to contain only the average of each variable (and we
are only interested in variables which are means or standard deviations in the input) for each activity and each subject.

### The training and test data sets are merged as are various other support files

* test and training data (X_train.txt, X_test.txt)
* subject ids (subject_train.txt,subject_test.txt) 
* activity ids (y_train.txt, y_test.txt)

The Variables are initially labelled with the names assigned by original
collectors (features.txt tile).

### Extract only the mean and standard deviation variables

From the merged data set is extracted an intermediate (internal) data set with only the
values of estimated mean (variables with labels that contain "mean()") and standard
deviation (variables with labels that contain "std()"). We ignore case.

    pattern <- "mean()|std()|Subject|ActivityId"

    new_sensor_data <- sensor_data[,grep(pattern,  names(sensor_data), ignore.case=T)]


### Descriptive activity names are added to the dataset

A new column is added to include activity description.
Activity id column is used to look up descriptions in activity_labels.txt.

The activity lables are ...

    1 WALKING
    2 WALKING_UPSTAIRS
    3 WALKING_DOWNSTAIRS
    4 SITTING
    5 STANDING
    6 LAYING


### Labels are modified

Some slight changes to the variable labels are made, but we do not stray too far so that 
data provenance is preserved and a clear relationship between the tidy data and it's origins
is maintained. We remove parentheses and convert commas to dashes in the tidy data set.

### The tidy data set is created.

It contains 180 rows (observations). That is 30 subjects by 6 activities. Each row contains
Subject ID, Activity and 86 other variables (for a total of 88 variables)

The structure is as follows ..

     'data.frame':	180 obs. of  88 variables:
     $ subject   : int  1 1 1 1 1 1 2 2 2 2 ...
     $ activity  : Factor w/ 6 levels "LAYING","SITTING",..: 1 2 3 4 5 6 1 2 3 4 ...
     $ tbodyacc-mean-x   : num  0.222 0.261 0.279 0.277 0.289 ...
     $ tbodyacc-mean-y   : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
     $ tbodyacc-mean-z   : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
     $ tbodyacc-std-x: num  -0.928 -0.977 -0.996 -0.284 0.03 ...
     $ tbodyacc-std-y: num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
     $ tbodyacc-std-z: num  -0.826 -0.94 -0.98 -0.26 -0.23 ...
     $ tgravityacc-mean-x: num  -0.249 0.832 0.943 0.935 0.932 ...
     $ tgravityacc-mean-y: num  0.706 0.204 -0.273 -0.282 -0.267 ...
     $ tgravityacc-mean-z: num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...
     $ tgravityacc-std-x : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...
     $ tgravityacc-std-y : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...
     $ tgravityacc-std-z : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...
     $ tbodyaccjerk-mean-x   : num  0.0811 0.0775 0.0754 0.074 0.0542 ...
     $ tbodyaccjerk-mean-y   : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...
     $ tbodyaccjerk-mean-z   : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
     $ tbodyaccjerk-std-x: num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
     $ tbodyaccjerk-std-y: num  -0.924 -0.981 -0.986 0.067 -0.102 ...
     $ tbodyaccjerk-std-z: num  -0.955 -0.988 -0.992 -0.503 -0.346 ...
     $ tbodygyro-mean-x  : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
     $ tbodygyro-mean-y  : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
     $ tbodygyro-mean-z  : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...
     $ tbodygyro-std-x   : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...
     $ tbodygyro-std-y   : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
     $ tbodygyro-std-z   : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...
     $ tbodygyrojerk-mean-x  : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
     $ tbodygyrojerk-mean-y  : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
     $ tbodygyrojerk-mean-z  : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
     $ tbodygyrojerk-std-x   : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...
     $ tbodygyrojerk-std-y   : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...
     $ tbodygyrojerk-std-z   : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...
     $ tbodyaccmag-mean  : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
     $ tbodyaccmag-std   : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
     $ tgravityaccmag-mean   : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
     $ tgravityaccmag-std: num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
     $ tbodyaccjerkmag-mean  : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
     $ tbodyaccjerkmag-std   : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
     $ tbodygyromag-mean : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
     $ tbodygyromag-std  : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...
     $ tbodygyrojerkmag-mean : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...
     $ tbodygyrojerkmag-std  : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...
     $ fbodyacc-mean-x   : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
     $ fbodyacc-mean-y   : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
     $ fbodyacc-mean-z   : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...
     $ fbodyacc-std-x: num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
     $ fbodyacc-std-y: num  -0.834 -0.917 -0.972 0.056 -0.113 ...
     $ fbodyacc-std-z: num  -0.813 -0.934 -0.978 -0.28 -0.298 ...
     $ fbodyacc-meanfreq-x   : num  -0.1588 -0.0495 0.0865 -0.2075 -0.3074 ...
     $ fbodyacc-meanfreq-y   : num  0.0975 0.0759 0.1175 0.1131 0.0632 ...
     $ fbodyacc-meanfreq-z   : num  0.0894 0.2388 0.2449 0.0497 0.2943 ...
     $ fbodyaccjerk-mean-x   : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
     $ fbodyaccjerk-mean-y   : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
     $ fbodyaccjerk-mean-z   : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...
     $ fbodyaccjerk-std-x: num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
     $ fbodyaccjerk-std-y: num  -0.932 -0.983 -0.987 0.107 -0.135 ...
     $ fbodyaccjerk-std-z: num  -0.961 -0.988 -0.992 -0.535 -0.402 ...
     $ fbodyaccjerk-meanfreq-x   : num  0.132 0.257 0.314 -0.209 -0.253 ...
     $ fbodyaccjerk-meanfreq-y   : num  0.0245 0.0475 0.0392 -0.3862 -0.3376 ...
     $ fbodyaccjerk-meanfreq-z   : num  0.02439 0.09239 0.13858 -0.18553 0.00937 ...
     $ fbodygyro-mean-x  : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...
     $ fbodygyro-mean-y  : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
     $ fbodygyro-mean-z  : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
     $ fbodygyro-std-x   : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...
     $ fbodygyro-std-y   : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
     $ fbodygyro-std-z   : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...
     $ fbodygyro-meanfreq-x  : num  -0.00355 0.18915 -0.12029 0.01478 -0.10045 ...
     $ fbodygyro-meanfreq-y  : num  -0.0915 0.0631 -0.0447 -0.0658 0.0826 ...
     $ fbodygyro-meanfreq-z  : num  0.010458 -0.029784 0.100608 0.000773 -0.075676 ...
     $ fbodyaccmag-mean  : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
     $ fbodyaccmag-std   : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...
     $ fbodyaccmag-meanfreq  : num  0.0864 0.2367 0.2846 0.1906 0.1192 ...
     $ fbodybodyaccjerkmag-mean  : num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
     $ fbodybodyaccjerkmag-std   : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...
     $ fbodybodyaccjerkmag-meanfreq  : num  0.2664 0.3519 0.4222 0.0938 0.0765 ...
     $ fbodybodygyromag-mean : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...
     $ fbodybodygyromag-std  : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...
     $ fbodybodygyromag-meanfreq : num  -0.139775 -0.000262 -0.028606 0.268844 0.349614 ...
     $ fbodybodygyrojerkmag-mean : num  -0.942 -0.99 -0.995 -0.319 -0.282 ...
     $ fbodybodygyrojerkmag-std  : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...
     $ fbodybodygyrojerkmag-meanfreq : num  0.176 0.185 0.334 0.191 0.19 ...
     $ angletbodyaccmean-gravity : num  0.021366 0.027442 -0.000222 0.060454 -0.002695 ...
     $ angletbodyaccjerkmean-gravitymean : num  0.00306 0.02971 0.02196 -0.00793 0.08993 ...
     $ angletbodygyromean-gravitymean: num  -0.00167 0.0677 -0.03379 0.01306 0.06334 ...
     $ angletbodygyrojerkmean-gravitymean: num  0.0844 -0.0649 -0.0279 -0.0187 -0.04 ...
     $ anglex-gravitymean: num  0.427 -0.591 -0.743 -0.729 -0.744 ...
     $ angley-gravitymean: num  -0.5203 -0.0605 0.2702 0.277 0.2672 ...
     $ anglez-gravitymean: num  -0.3524 -0.218 0.0123 0.0689 0.065 ...