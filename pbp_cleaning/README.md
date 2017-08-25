# BlueSCar Play by Play Cleaning

Recently Play by Play data set from ESPN was posted on the r/cfbanalysis 
subreddit. The files are all in JSON format so this is my attempt to clean them 
for quicker and easier use. You can view a full explanation of the data [here]( 
https://www.reddit.com/r/CFBAnalysis/comments/6htfc6/play_by_play_data_dump_20012016/
)

### Output

I'd like to at least seperate out the following information into seperate files

- A table of all games with the teams, scores, dates, and locations
- A table of all yearly conference affiliations
- A table of drive level information
- A table of all play information
- A table of all run/pass data with more specific info
- Seperate special teams files, perhaps all in one, perhaps not

Each of these should be able to be easily merged with another by `id`s and I 
want each year to be a seperate file. 


### Process

I'm going to generate each file from a seperate function. This may be more time
consuming but it will be easier to update what information I save for each file.
I assume I will make some mistakes along the way to being able to just update
the game file or run file will make that process quicker. 