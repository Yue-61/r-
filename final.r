setwd("C:/Users/lenovo/Desktop") ## comment out of submitted
a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")

#4(a)
open_idx <- grep("\\[", a)

stage_idx <- integer(0)  # to store all indices to remove

for (i in open_idx) {
  
  search_range <- (i+1):min(i+100, length(a))  # look ahead max 100 words (or until end of text)
  
  
  close_idx <- grep("\\]", a[search_range]) # find closing bracket
  
  if (length(close_idx) > 0) {
    
    stage_idx <- c(stage_idx, i:(i + close_idx[1])) # matched closing bracket found → record full span
  } else {
    
    stage_idx <- c(stage_idx, i) # unmatched [ → record from [ onward (decide policy)
  }
}

a <- a[-unique(stage_idx)]


#4(b)

is_upper <- a == toupper(a) # identify words that are entirely uppercase

only_letters <- grepl("^[A-Z]+$", a) # find words that contain only uppercase letters.

not_special <- !(a %in% c("I", "A")) # excluding special cases “I” and “A”

is_number <- grepl("^[0-9]+$", a) # identify words that are purely numerical

to_remove <- (is_upper & only_letters & not_special) | is_number # find the content that needs to be deleted

a <- a[!to_remove]

#4(c)
a <- gsub("-", "", a) # delete "-"
a <- gsub("_", "", a) # delete "_"
a <- gsub("—", "", a) # delete "—"


#(4de)
split_punct <- function(words, punct) # words: vector of words, punct: vector of punctuation marks to separate 
{
  
  punct_esc <- paste0("\\", punct, collapse = "")
  pattern <- paste0("[", punct_esc, "]")
  
  
  idx <- grep(pattern, words) # identify words containing punctuation marks in the index
  
  
  result <- words[-idx] # copy words without punctuation marks
  
  
  words_no_punct <- gsub(pattern, "", words[idx]) # remove punctuation marks with gsub function
  
  punct_found <- regmatches(words[idx], regexpr(pattern, words[idx])) # use grep function to extract punctuation marks
  
  
  final_result <- character(length(words) + length(idx))
  
  
  final_result[-( (length(words)+1 - length(result)) : length(final_result))] <- words[-idx] # replace the words without punctuation marks in their original positions
  
  
  final_result[rep(idx*2-1, each=1)] <- words_no_punct # insert the split words into their corresponding positions
  final_result[rep(idx*2, each=1)] <- punct_found # insert the split punctuation marks into their corresponding positions
  
  
  final_result[final_result != ""]
}

punct = c(",", ".", ";", "!", ":", "?") # input the punctuation marks to be separated
a <- split_punct(a, punct)

#4(f)
a <- tolower(a) # convert vector a to lower case


#(5)(6)
unique_b <- unique(a) ## find the vector of unique words
print(unique_b)
index_vector <- match(a,unique_b) ## find the position of each element of clean_a7 in unique_b
print(index_vector)
counts <- tabulate(index_vector) ## count up how many time each unique word occurs
print(counts)
rank_b <- rank(counts,ties.method = "first") ## rank the counts
print(rank_b)
b <- unique_b[rank_b <= 1000] ##creat a vector b containing the 1000 most common words
print(b)

token_vector <- match(a,b)
print(token_vector)

n <- length(a)
mlag <- 4
M <- matrix(nrow = n - mlag, ncol = mlag + 1) ##creat a matrix M
for (j in 0:mlag) {
  M[, j + 1] <- token_vector[(1 + j):(n - mlag + j)]
}
print(M)

#7
M1 <- token_vector
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1)) {
  mlag <- ncol(M) - 1   # maximum sequence length
  key_len <- length(key)
  
  if (key_len > mlag) {
    key <- tail(key, mlag)   # keep only the last mlag tokens
    key_len <- mlag
  }
  
  u_all <- c()
  prob_all <- c()
  
  for (i in seq_len(key_len)) {   # gradually shorten from the longest sequence
    mc <- mlag - key_len + i   # starting column
    sub_key <- key[i:key_len]
    
    ii <- colSums(!(t(M[, mc:mlag, drop=FALSE]) == sub_key))   # find matching rows
    match_rows <- which(ii == 0 & is.finite(ii))
    
    if (length(match_rows) > 0) {
      u <- M[match_rows, mlag + 1]   # collect subsequent words
      u <- u[!is.na(u)]              # remove NA
      if (length(u) > 0) {
        prob <- rep(w[key_len - i + 1] / length(u), length(u))   # assign probability
        u_all <- c(u_all, u)
        prob_all <- c(prob_all, prob)
      }
    }
  }
  
  if (length(u_all) == 0) {   # if no match found, randomly choose a common word
    u_all <- M1[!is.na(M1)]
    prob_all <- rep(1 / length(u_all), length(u_all))
  }
  
  if (length(u_all) == 1) {   # sample one token according to probability
    next_token <- u_all[1]
  } else {
    idx <- sample.int(length(u_all), 1, prob = prob_all)
    next_token <- u_all[idx]
  }
  return(next_token)
}

#8
start_token <- sample(M1[!is.na(M1)], 1)   # randomly select a non-punctuation word as the starting word
print(start_token)
print(b[start_token])

#9
simulate_sentence <- function(start_token, M, M1, b, 
w = rep(1, ncol(M) - 1), max_len = 50) {
key <- start_token
sentence_tokens <- key
  
for (step in 1:max_len) {   # limit maximum of max_len generated words
    new_token <- next.word(key, M, M1, w)
    sentence_tokens <- c(sentence_tokens, new_token)
    
    mlag <- ncol(M) - 1
    if (length(key) >= mlag) {
      key <- c(key[-1], new_token)   # maintain sliding window of length mlag
    } else {
      key <- c(key, new_token)
    }
    
    if (!is.na(new_token) && b[new_token] == ".") break   # stop if end of sentence
  }
  
  sentence <- paste(b[sentence_tokens], collapse=" ")   # convert tokens to sentence
  return(sentence)
}

result <- simulate_sentence(start_token, M, M1, b)
print(result)




