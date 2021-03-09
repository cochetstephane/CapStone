# CapStone

1- Get the data as .txt files (News, Blogs, Twitter)
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip",method="curl")
  unzip("Coursera-SwiftKey.zip")
}

2- Establish the n-grams

3- A step further with LDA Topic modelling

4- The shiny App for a demo tool
