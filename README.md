# Doing Meta-Analysis in R <img src="_figs/hexagon.png" width="100" align="right" />

[![DOI](https://zenodo.org/badge/152492192.svg)](https://zenodo.org/badge/latestdoi/152492192)
[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/MathiasHarrer.svg?style=social&label=Follow%20%40MathiasHarrer)](https://twitter.com/MathiasHarrer)
[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/pimcuijpers.svg?style=social&label=Follow%20%40pimcuijpers)](https://twitter.com/pimcuijpers)
[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/pimcuijpers.svg?style=social&label=Follow%20%40Toshi_FRKW)](https://twitter.com/Toshi_FRKW)
[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/DDEbert.svg?style=social&label=Follow%20%40DDEbert)](https://twitter.com/DDEbert)

This is a online handbook on how to perform meta-analyses in R.

## How to get and run the R code for the guide

We hope you're enjoying our guide on how to do [Meta-Analysis in R](https://www.protectlab.org/meta-analysis-in-r). Here's a description on how you can download the R Code to run your Meta-Analyses yourself.

## Accessing the repository

We have stored all R Code in a repository which you can download onto your computer. To access the repository, click **View on GitHub** on top of this page. There, you can access all code files, and copy & paste them into RStudio.

You can directly download the repository on **GitHub** by clicking on **Clone or Download** and then on **Download Zip**.

<img src="_figs/clone.PNG">

In case you are a GitHub user already, you can **star** the repository to access it more easily in the future.

## Downloading the files directly

You can download the entire repository on the top of this site by clicking on **Download .zip**. This downloads a `zip` file containing all files onto your computer.


## Running the code

Of course, you can run all code one by one if you simply copy and paste the code into RStudio and run it.
If you want to run all code in RStudio along with reading the book, an easier way might be to access the **R Markdown** files in the download folder and run the code chunks in them. Here's how you do that.

1.  Save the downloader folder on your computer in a folder where you can easily find it.
2.  In **R Studio**, under the **Files** pane (bottom-left corner), search for the downloaded folder **"Doing_Meta_Analysis_in_R-master"** and open it.
3.  In the folder, click on **bookdown-demo.Rproj**. This opens the R Project with which the all analyses were conducted.
4.  Now, you can access all the chapters in the guide. They are stored as so-called **R Markdown** files (`.Rmd`) in the same folder and have the following names:

* **Chapter 1**: `index.Rmd`
* **Chapter 2**: `01-rstudio_and_basics.Rmd`
* **Chapter 3**: `02-getting_data_in_R.Rmd`
* **Chapter 4**: `03-pooling_effect_sizes.Rmd`
* **Chapter 5**: `04-forest_plots.Rmd`
* **Chapter 6**: `05-heterogeneity.Rmd`
* **Chapter 7**: `06-subgroup_analyses.Rmd`
* **Chapter 8**: `07-metaregression.Rmd`
* **Chapter 9**: `08-publication_bias.Rmd`
* **Chapter 10**: `09-risk_of_bias_summary.Rmd`
* **Chapter 11**: `10-network_metaanalysis.Rmd`
* **Chapter 12**: `11-Effectsizeconverter.Rmd`
* **Chapter 13**: `12-power_analysis.Rmd`

5. Click to open the file you want to see. You'll see that the files contain the exact same **text** and **code** as the one used in the guide, but there are a few lines of code more **which are not displayed in the guide**. These are needed, for example, to set the layout for the book.
6. To run the **code chunks** between the text, click on the **little green arrow** on the top-right corner of each code chunk. Once you do this, the code in the chunk is run by R. 

<img src="_figs/greenarrow.PNG">

**Important: most code chunks are not independent, and draw on objects or functions which are created in preceeding chunks. Therefore, you should stick with running one chunk after another, from top to bottom.**

We hope these files will make it easier for you to do your on Meta-Analysis in R.
If you're having problems with the **download**, running the **code chunks**, or if you stumble upon errors, let us now at mathias.harrer@fau.de

*Mathias & David*
