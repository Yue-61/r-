setwd("D:/") ## comment out of submitted
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


#(4df)Define split_punct function
# Function: split_punct
# Purpose: Separate punctuation marks from attached words
# Input: 
#   - words: vector of words (some may have attached punctuation)
#   - punct: vector of punctuation marks to separate (default: common English punctuation)
# Output: vector with punctuation separated as individual elements
split_punct <- function(words, punct = c(",", ".", ";", "!", ":", "?")) {
  out <- vector("list", length(words)) #Create a list with same length as input words
  for (i in seq_along(words)) { 
    w <- words[i]; chars <- strsplit(w, "")[[1]] #Split the word into individual characters for inspection
    
    if (!any(chars %in% punct)) out[[i]] <- w #Check if the word contains any of the specified punctuation marks
    else {  
      keep <- chars[!(chars %in% punct)] #Non-punctuation characters (to be kept as part of the word)
      pnts <- chars[chars %in% punct] #Punctuation characters (to be separated)
      
      core <- paste(keep, collapse = "") #Reconstruct the word core from non-punctuation characters
      
      if (nzchar(core)) out[[i]] <- c(core, pnts) else out[[i]] <- pnts #Handle the case where the word consists only of punctuation
    }
  }
  unlist(out, use.names = FALSE)
}
punct = c(",", ".", ";", "!", ":", "?")

clean_a5 <- split_punct(clean_a4, punct)
print(clean_a5)

#4(g)
clean_a7 <- tolower(clean_a5)
print(clean_a7)


#(5)(6)
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


token_vector <- match(clean_a7,b)
print(token_vector)


n <- length(clean_a7)
mlag <- 4
M <- matrix(nrow = n - mlag, ncol = mlag + 1) ##creat a matrix M
for (j in 0:mlag) {
  M[, j + 1] <- token_vector[(1 + j):(n - mlag + j)]
}
print(M)
#7
M1 <- token_vector
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1)) {
  mlag <- ncol(M) - 1   # 最大序列长度
  key_len <- length(key)
  
  if (key_len > mlag) {
    key <- tail(key, mlag)
    key_len <- mlag
  }
  
  u_all <- c()
  prob_all <- c()
  
  # 从最长序列往下逐步缩短
  for (i in seq_len(key_len)) {
    mc <- mlag - key_len + i   # 起始列
    sub_key <- key[i:key_len]
    
    # 找到匹配的行
    ii <- colSums(!(t(M[, mc:mlag, drop=FALSE]) == sub_key))
    match_rows <- which(ii == 0 & is.finite(ii))
    
    if (length(match_rows) > 0) {
      u <- M[match_rows, mlag + 1]   # 收集后续词
      u <- u[!is.na(u)]              # 去掉 NA
      if (length(u) > 0) {
        prob <- rep(w[key_len - i + 1] / length(u), length(u))
        u_all <- c(u_all, u)
        prob_all <- c(prob_all, prob)
      }
    }
  }
  
  # 如果找不到匹配随机选一个常见词
  if (length(u_all) == 0) {
    u_all <- M1[!is.na(M1)]
    prob_all <- rep(1 / length(u_all), length(u_all))
  }
  
  # 按概率采样一个 token
  if (length(u_all) == 1) {
    next_token <- u_all[1]
  } else {
    idx <- sample.int(length(u_all), 1, prob = prob_all)
    next_token <- u_all[idx]
  }
  return(next_token)
}
#8
# 随机选择一个非标点符号的词作为起始词、
start_token <- sample(M1[!is.na(M1)], 1)
print(start_token)
print(b[start_token])
#9
simulate_sentence <- function(start_token, M, M1, b, 
                              w = rep(1, ncol(M) - 1), max_len = 50) {
  key <- start_token
  sentence_tokens <- key
  
  for (step in 1:max_len) {   # 限制最多生成 max_len 个词
    new_token <- next.word(key, M, M1, w)
    sentence_tokens <- c(sentence_tokens, new_token)
    
    mlag <- ncol(M) - 1
    if (length(key) >= mlag) {
      key <- c(key[-1], new_token)
    } else {
      key <- c(key, new_token)
    }
    
    if (!is.na(new_token) && b[new_token] == ".") break
  }
  
  sentence <- paste(b[sentence_tokens], collapse=" ")
  return(sentence)
}

result <- simulate_sentence(start_token, M, M1, b)
print(result)

#(5)(6)
b <- unique(clean_a7)

# (b) 使用 match 找 a 中每个元素对应 b 的索引
index <- match(clean_a7, b)

# (c) 使用 tabulate 统计每个唯一单词出现次数
counts <- tabulate(index)

# (d) 创建包含约1000个最常见单词的向量 b_top
# 利用 rank 找频率最高的单词
top_n <- 1000
if(length(counts) < top_n) {
  top_n <- length(counts)
}
# 频率越大 rank 越小，先对 counts 排序取前 top_n 个
ranks <- rank(-counts, ties.method = "first")  # 负号使频率高的排名靠前
b_top <- b[ranks <= top_n]

# 输出结果
print(b)
print(index)
print(counts)
print(b_top)







