# Activity: Bicycle Sharing

## Instructions 

- Each student will submit the activity individually to Canvas, but students are encouraged to help one another complete the work
- Complete the Bicycle Sharing Activity found in the DataComputing eBook
    - make sure you carefully follow the instructions and mirror the code in the activity as you work 
    - **every task in the activity should have narrative text describing your observations; most steps also require code chunks and corresponding output.**
    - you should make commits in GitHub as you complete the activity
- submit the completed R Notebook as HTML to Canvas before deadline

## Warning

The data are slightly larger for this activity when compared to most of the others.  You'll have more than 650,000 rows of data and several variables, so it's a good idea to use the "small" subset of the data to do your programming (i.e. read in data_site that ends with -Small.rds) and then switch to the large data file at the very end (i.e. remove the -Small in the  data_site read in to end with Data.rds)  Also, you'll have to be patient with joins as R is working with larger data sets (though it really shouldn't take more than 2 minutes).


## Grading

Assignment is worth a total of 10 points.

- Turn in HTML with embedded .Rmd file 
- [2 points] Side-by-side boxplots of rental durations (*How Long* section)
- [4 points total] following listed plots (*When are bikes used* section)
    - [0.5 point] density plot: day of the year (1 to 365)
    - [0.5 point] density plot: day of the week (Sunday to Saturday; numeric is fine)
    - [0.5 point] density plot: hour of the day (0 to 24)
    - [0.5 point] density plot: minute in the hour (0 to 60)
    - [1 points] Plot rental frequency by hour, weekday, & client type
    - [1 points] Plot rental density by hour, weekday, & client type
- [2 points] Distance plots (*How Far?* section)
    - [1 point] density plot of station to station distances
    - [1 point] density plot of ride distances
- [2 points] *Mapping the stations* leaflet (interactive map)
- [EXTRA CREDIT: 2 pts] *Long Distance Stations* challenge task

