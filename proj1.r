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
split_punct_efficient <- function(words, punctuation) {
  # --- 1. 标点符号转义与模式构建 (与之前相同，确保正确性) ---
  
  # 需要转义的特殊字符：. \ | ( ) [ { ^ $ * + ?
  # 修正后的转义模式：匹配并捕获任何一个特殊字符
  escaped_punc_pattern <- "([.\\|()\\[\\]\\{\\}\\*+?$])" 
  
  # 将用户传入的标点符号向量中的特殊字符进行转义，用于构建 pattern
  escaped_punc <- gsub(escaped_punc_pattern, "\\\\\\1", punctuation)
  
  # 构建一个匹配任何指定标点的正则表达式字符集： e.g., "[.,]"
  pattern <- paste0("[", paste(escaped_punc, collapse = ""), "]")
  
  # --- 2. 识别需要分割的词语和标点符号 ---
  
  # 使用 grep 找出包含标点符号的词语的**索引**
  # 这一步是高效的查找
  indices_to_split <- grep(pattern, words)
  
  # 如果没有需要分割的词语，直接返回原向量
  if (length(indices_to_split) == 0) {
    return(words)
  }
  
  # 找出需要处理的词语
  words_to_split <- words[indices_to_split]
  
  # a. 提取标点符号 (高效的向量化操作)
  # 匹配所有非指定标点字符，替换为 ""，剩下的就是标点
  pattern_non_punc <- paste0("[^", paste(escaped_punc, collapse = ""), "]")
  punct_marks <- gsub(pattern_non_punc, "", words_to_split)
  
  # b. 移除标点符号 (获得纯词语) (高效的向量化操作)
  pure_words <- gsub(pattern, "", words_to_split)
  
  # --- 3. 构建新的向量结构 (最高效的部分) ---
  
  # 预先计算最终向量的长度
  # 原始长度 + 需要分割的词语数量 (因为每个词语都会在后面插入一个标点)
  final_length <- length(words) + length(indices_to_split)
  
  # 创建一个最终向量 (预分配内存)
  result_vector <- character(final_length)
  
  # 利用 rep 函数和索引来构建最终向量的骨架
  # 这比在循环中使用 c() 快得多
  
  # 找出**不**需要分割的词语的索引
  indices_not_to_split <- setdiff(seq_along(words), indices_to_split)
  
  # 构建原始词语在**新**向量中的位置索引
  # 1. 创建一个与原始向量等长的索引向量 (1, 2, 3, 4, 5, 6, 7...)
  original_indices <- seq_along(words)
  
  # 2. 对于每个需要分割的词语，在新向量中的位置会比它原始位置多一个（因为插入了标点）
  #    因此，我们需要一个累积的和，来计算偏移量
  #    例如：第 2 个词分割，第 7 个词分割。
  #    第 1 个词在新向量位置 1。
  #    第 2 个词在新向量位置 2。
  #    标点在新向量位置 3。
  #    第 3 个词在新向量位置 4 (原位置 3 + 1 个偏移)。
  #    ...
  #    第 7 个词在新向量位置 9 (原位置 7 + 2 个偏移)。
  
  # 计算每个元素在新向量中的最终索引
  # 创建一个指示向量，1 表示该位置需要偏移 (插入)，0 表示不需要
  is_split_indicator <- rep(0, length(words))
  is_split_indicator[indices_to_split] <- 1
  
  # 累积求和，得到每个位置的偏移量
  offset <- cumsum(is_split_indicator) - is_split_indicator
  
  # 词语在新向量中的最终索引 (原始位置 + 偏移量)
  word_final_indices <- original_indices + offset
  
  # 标点符号在新向量中的最终索引 (词语最终索引 + 1)
  punct_final_indices <- word_final_indices[indices_to_split] + 1
  
  # --- 4. 填充结果向量 ---
  
  # 填充所有词语
  result_vector[word_final_indices] <- words
  
  # 填充需要分割的词语为“纯词语”
  result_vector[word_final_indices[indices_to_split]] <- pure_words
  
  # 填充标点符号
  result_vector[punct_final_indices] <- punct_marks
  
  # 5. 返回更新后的向量
  return(result_vector)
}

# --- 示例用法 ---
# 输入向量

# 标点符号向量
input_punc <- c(",", ".") 

# 调用函数
clean_a5 <- split_punct_efficient(clean_a4, input_punc)


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
  next_token <- sample(u_all, 1, prob = prob_all)
  return(next_token)
}
#8
# 随机选择一个非标点符号的词作为起始词、
start_token <- sample(M1[!is.na(M1)], 1)
print(start_token)
print(b[start_token])
#9
simulate_sentence <- function(start_token, M, M1, b, w = rep(1, ncol(M) - 1)) {
  key <- start_token
  sentence_tokens <- key
  
  repeat {
    new_token <- next.word(key, M, M1, w)
    sentence_tokens <- c(sentence_tokens, new_token)
    
    # 更新 key，始终保持最后 (mlag) 个 token
    mlag <- ncol(M) - 1
    if (length(key) >= mlag) {
      key <- c(key[-1], new_token)
    } else {
      key <- c(key, new_token)
    }
    
    # 如果生成句号（"."），结束
    if (!is.na(new_token) && b[new_token] == ".") {
      break
    }
  }
  
  # 转换回单词
  sentence <- paste(b[sentence_tokens], collapse=" ")
  return(sentence)
}
result <- simulate_sentence(start_token, M, M1, b)
print(result)







