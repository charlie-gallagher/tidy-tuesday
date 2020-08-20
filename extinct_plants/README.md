# Extinct Plants
This project is in two parts. The first part, in `extinct_plants.Rmd`, explores the data and figures out the technique for the final visualization. The second part, `rfiles\extint_plants.R`, produces the graphic.

![Extinct plants](/pdf/extinct_plants.jpg)

This week, I wanted to move away from the crowd a little and look at plant groups (more or less divisions in this case) and how each group is affected by different environmental factors. It's a brief visualization, but interesting, I think.

I chose to make an array of pie charts. Normally, pie charts are the wrong choice of graph, because people can't read angles very well. However, this data was binary (affected or not affected), which makes the pie chart fairly intuitive and familiar. Also, pie charts en masse (as they are here) allow one to get a sense of density for each cause and for each plant.

N.B. The graphic was produced in both RStudio and GIMP. I especially have bad font support in R right now because I've been too lazy to learn how to integrate interesting fonts. The visualization of the data is done in R, but the presentation is done in GIMP (file not included).
