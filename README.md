# ufo-uap-data-science-project

This is a hobby project where I am looking at a dataset that shows UFO sightings nationwide up to December 2022. The dataset is collected from the National UFO Reporting Center by [Tim Renner who maintains an up to date version on data.world](https://data.world/timothyrenner/ufo-sightings).

I'm using RStudio to explore the data, do text analysis, and create Shiny dashboards. It's a work in progress and in a lot of flux, but generally I organize major tasks into numbered folders. If you were following along with this project, you would have to start at **01.build.dataset** and go on from there.

## About Sourcing the Data

Github doesn't easily allow me to store large csv files and so if you fork this project you will need to provide the original data yourself. You can do that by [going to Tim's project on data.world](<(https://data.world/timothyrenner/ufo-sightings)>) and then downloading the most recent file as a csv file. Name it **nuforc_reports_download.csv** and place it in the directory **01.build.dataset**. Once you have done this you can step through the code to recreate the sanitized files I used in this project.

If you need to use the new files you created in this step in other tasks in this project, you will need to copy the files to those folders or change the file reference to point to the build.dataset directory.

## What I'm Planning

Today, I'm working on looking at local (to me) UFO sightings over the past ten years. I really wanted to zoom in on a smaller area so I could carefully study a set of cases and I choose a place close to me so I could have some extra context. Plus, it's fun to hear about sightings in places you visit very day.

In order to examine these stories I'm going to do some data exploration on the dataset as a whole and then I am going to do some text mining. This will give me a broad view and maybe a starting point to identify trends.

Then I'm creating a dashboard that will make it easy to view each case one it's own chronologically. [That dashboard is published here](https://mattjcamp.shinyapps.io/Bucks_UFO_Browser/). The code used to created in located in this project under **03.bucks.dashboards**.

Finally, I'm using all of this to inform some articles that I am writing on my blog about UFOs in Bucks County, PA.

-[UFOs Over Bucks County - Data Exploration](https://www.mattjcamp.com/posts/2023-05-25-bucks-ufo-data)

-[Will Any Proof of UFOs be Good Enough?](https://www.mattjcamp.com/posts/2023-05-23-ufo-proof)

-[NUFORC Dataset Thoughts](https://www.mattjcamp.com/posts/2023-04-08-nuforc-dataset-thoughts)
