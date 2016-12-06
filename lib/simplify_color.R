setwd("C:/Users/CATHY/Desktop/Fall2016-proj5-proj5-grp10-master/Fall2016-proj5-proj5-grp10-master")

data <- shelter_data_new
data <- as.data.frame(data)

Color.final <- array()
one_type <- !grepl("/", data$Color)

for( i in 1:length(data$Color) ){
  if(one_type[i]){
    Color.final[i] <- gsub( " .*$", "", data$Color[i])

  }else{
    temp <- sort(unlist(strsplit(as.character(data$Color[i]),'/',fixed=TRUE)))
    temp[1] <- gsub( " .*$", "", temp[1])
    temp[2] <- gsub( " .*$", "", temp[2])
    Color.final[i] <- paste(temp[1], temp[2])
    
  }
}

save(Color.final, file = "./data/color_final.RData")
