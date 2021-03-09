# CapStone

Get the data as .txt files (News, Blogs, Twitter)
if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip",method="curl")
  unzip("Coursera-SwiftKey.zip")
}

