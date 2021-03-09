# CapStone

1- Get the data as .txt files (News, Blogs, Twitter)
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip",method="curl")
  unzip("Coursera-SwiftKey.zip")
}

2- Establish the n-grams in report_phase2_ter.Rmd

3- A step further with LDA Topic modelling in Report_Phase2_quanteda_1.R

4- Presentation of report on the .html file

5- The shiny App for a demo tool
