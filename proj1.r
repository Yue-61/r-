setwd("C:/Users/lenovo/Desktop") ## comment out of submitted
a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
open_idx <- grep("\\[", a)

stage_idx <- integer(0)  # to store all indices to remove

for (i in open_idx) {
  # look ahead max 100 words (or until end of text)
  search_range <- (i+1):min(i+100, length(a))
  
  # find closing bracket
  close_idx <- grep("\\]", a[search_range])
  
  if (length(close_idx) > 0) {
    # matched closing bracket found → record full span
    stage_idx <- c(stage_idx, i:(i + close_idx[1]))
  } else {
    # unmatched [ → record from [ onward (decide policy)
    stage_idx <- c(stage_idx, i)
  }
}

clean_a <- a[-unique(stage_idx)]

print(clean_a)
