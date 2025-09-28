setwd("C:/Users/lenovo/Desktop") ## comment out of submitted
a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")

print(a)
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

clean_a1 <- a[-unique(stage_idx)]

print(clean_a1)

#4(b)
#找出是全大写的单词
is_upper <- clean_a1 == toupper(clean_a1)
#找出只包含大写字母的单词（不包括标点或数字）
only_letters <- grepl("^[A-Z]+$", clean_a1)
#排除特殊情况 "I" 和 "A"
not_special <- !(clean_a1 %in% c("I", "A"))
#找出是纯数字的单词
is_number <- grepl("^[0-9]+$", clean_a1)
#找出需要删除的内容（大写但不是 "I"/"A"，或者是数字）
to_remove <- (is_upper & only_letters & not_special) | is_number
#保留不是 to_remove 的内容
clean_a2 <- clean_a1[!to_remove]
print(clean_a2)
#4(c)
clean_a3 <- gsub("-", "", clean_a2)
clean_a4 <- gsub("_", "", clean_a3)
print(clean_a4)
#4(d)
split_punct <- function(words, punct) {
  # 构造正则表达式，匹配指定的标点符号
  pattern <- paste0("[", paste(punct, collapse=""), "]")
  # 找出所有含有标点的单词位置
  punct_idx <- grep(pattern, words)
  # 计算结果向量的长度：不含标点的保留一个位置，含标点的占两个位置
  total_len <- length(words) + length(punct_idx)
  # 初始化结果向量
  result <- rep("", total_len)
  index <- 1
  # 遍历每个单词
  for (i in seq_along(words)) {
    if (i %in% punct_idx) {
      # 如果单词包含标点，去除标点
      clean_word <- gsub(pattern, "", words[i])
      # 提取单词中的标点字符（逗号或句号）
      punct_char <- gsub(paste0("[^", paste(punct, collapse=""), "]"), "", words[i])
      # 将去除标点后的单词和标点分别插入结果
      result[index] <- clean_word
      result[index+1] <- punct_char
      index <- index + 2
    } else {
      # 不含标点的单词直接插入
      result[index] <- words[i]
      index <- index + 1
    }
  }
  return(result)
}
# 定义标点符号向量
punct <- c(",", ".")
# 调用函数，把 cleana 和 punct 传进去
clean_a5 <- split_punct(clean_a4, punct)
# 查看结果
print(clean_a5)
#4(f)
punct <- c(";", "!", ":","?" )
# 调用函数，把 cleana 和 punct 传进去
clean_a6 <- split_punct(clean_a5, punct)
# 查看结果
print(clean_a6)
#4(g)
clean_a7 <- tolower(clean_a6)
print(clean_a7)


unique_b <- unique(clean_a7) ## find the vector of unique words
print(unique_b)
index_vector <- match(clean_a7,unique_b) ## find the position of each element of clean_a7 in unique_b
print(index_vector)
counts <- tabulate(index_vector) ## count up how many time each unique word occurs
print(counts)
rank_b <- rank(counts,ties.method = "first") ## rank the counts
print(rank_b)
b <- unique_b[rank_b <= 1000] ##creat a vector b containing the 1000 most common words
print(b)
vector_6 <- match(b,a)
print(vector_6)
