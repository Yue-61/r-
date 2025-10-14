n <- 1000
h <- rep(1:n, times = sample(1:5, n, replace = TRUE))[1:n]

get.net <- function(beta, h, nc = 15) {
  n <- length(beta)
  mean_beta <- mean(beta)
  alink <- vector("list", n)
  
  prob_matrix <- matrix(0, n, n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # 如果是同家庭成员，概率为0
      if (h[i] == h[j]) {
        prob_matrix[i, j] <- 0
        prob_matrix[j, i] <- 0
      } else {
        # 计算连接概率
        prob_ij <- (nc * beta[i] * beta[j]) / (mean_beta^2 * (n-1))
        prob_matrix[i, j] <- prob_ij
        prob_matrix[j, i] <- prob_ij
        
        # 根据概率决定是否建立连接
        if (runif(1) < prob_ij) {
          alink[[i]] <- c(alink[[i]], j)
          alink[[j]] <- c(alink[[j]], i)
        }
      }
    }
  }
  
  return(alink)
}


nseir <- function(beta, h, alink, alpha = c(0.1, 0.01, 0.01), delta = 0.2, 
                  gamma = 0.4, nc = 15, nt = 100, pinf = 0.005) {
  
  n <- length(beta)
  mean_beta <- mean(beta)
  
  # 0=S, 1=E, 2=I, 3=R
  # 初始所有人都是S
  x <- rep(0, n)  
  
  # 随机选择初始感染者
  initial_infected <- sample(n, size = max(1, round(n * pinf)))
  # 设置为I
  x[initial_infected] <- 2  
  
  # 初始化结果存储，长度为nt，不包含初始状态
  S <- E <- I <- R <- rep(0, nt)  
  
  # 每日模拟循环
  for (t in 1:nt) {
    # 生成随机数向量，用于所有概率判断
    u <- runif(n)
    
    # I -> R: 
    x[x == 2 & u < delta] <- 3
    
    # E -> I:
    x[x == 1 & u < gamma] <- 2
    
    # S -> E: 
    s_indices <- which(x == 0)  # 所有易感者
    i_indices <- which(x == 2)  # 所有感染者
    
    if (length(s_indices) > 0 && length(i_indices) > 0) {
      # 对每个易感者计算感染概率
      for (j in s_indices) {
        # 计算不被任何感染者感染的概率
        prob_not_infected <- 1
        
        # 考虑每个感染者的感染风险
        for (i in i_indices) {
          prob_not_infected_by_i <- 1
          
          # 家庭感染 (如果i和j是同一家庭)
          if (h[i] == h[j]) {
            prob_not_infected_by_i <- prob_not_infected_by_i * (1 - alpha[1])
          }
          
          # 网络感染 (如果j在i的联系人列表中)
          if (j %in% alink[[i]]) {
            prob_not_infected_by_i <- prob_not_infected_by_i * (1 - alpha[2])
          }
          
          # 随机感染
          random_prob <- (alpha[3] * nc * beta[i] * beta[j]) / (mean_beta^2 * (n - 1))
          prob_not_infected_by_i <- prob_not_infected_by_i * (1 - random_prob)
          
          # 更新总的不被感染概率
          prob_not_infected <- prob_not_infected * prob_not_infected_by_i
        }
        
        # 决定是否感染 
        if (runif(1) > prob_not_infected) {
          # S -> E
          x[j] <- 1 
        }
      }
    }
    
    # 记录当天状态 (第t天)
    S[t] <- sum(x == 0)
    E[t] <- sum(x == 1) 
    I[t] <- sum(x == 2)
    R[t] <- sum(x == 3)
  }
  
  return(list(S = S, E = E, I = I, R = R, t = 1:nt, beta = beta))
}


plot_seir <- function(seir_results, main = "SEIR Model Dynamics") {
  # 提取数据
  S <- seir_results$S
  E <- seir_results$E
  I <- seir_results$I
  R <- seir_results$R
  t <- seir_results$t
  
  # 设置图形参数
  old_par <- par(no.readonly = TRUE)  # 保存当前图形参数
  on.exit(par(old_par))  # 退出函数时恢复原参数
  
  # 设置单图布局和边距 
  par(mar = c(4, 4, 3, 1))  # 下、左、上、右边距
  
  # 确定y轴范围 
  y_max <- max(c(S, E, I, R), na.rm = TRUE)
  y_min <- 0
  
  # 创建基础绘图框架 
  plot(t, S, type = "l", col = "black", lwd = 2,
       ylim = c(y_min, y_max),
       xlab = "Day", ylab = "Population",
       main = main,
       frame.plot = FALSE)  
  
  # 添加其他状态曲线 
  lines(t, E, col = "blue", lwd = 2)    # 潜伏者 - 蓝色
  lines(t, I, col = "red", lwd = 2)     # 感染者 - 红色
  lines(t, R, col = "green", lwd = 2)   # 康复者 - 绿色
  
  # 添加图例 
  legend("top", legend = c("Susceptible", "Exposed", "Infectious", "Recovered"),
         col = c("black", "blue", "red", "green"),
         lty = 1, lwd = 2, bty = "n", horiz = TRUE,
         cex = 0.8, xjust = 0.5, text.width = max(t) * 0.15)
  
  # 添加网格线，提高可读性 
  grid()
}


# 增强版绘图函数：支持多场景对比 
plot_seir_comparison <- function(results_list, scenario_names = NULL, 
                                 main = "SEIR Model Comparison") {
  n_scenarios <- length(results_list)
  
  # 设置多图布局
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(ceiling(n_scenarios/2), 2),  # 自动计算行列数
      mar = c(4, 4, 2, 1),  # 紧凑的边距
      oma = c(0, 0, 2, 0))  # 外边界，用于总标题
  on.exit(par(old_par))
  
  # 为每个场景绘制图形
  for (i in 1:n_scenarios) {
    plot_seir_single_panel(results_list[[i]], 
                           main = scenario_names[i])
  }
  
  # 添加总标题 (在外边界)
  mtext(main, side = 3, outer = TRUE, cex = 1.2, font = 2)
}

# 辅助函数：单面板绘图，用于多图布局
plot_seir_single_panel <- function(seir_results, main = "") {
  S <- seir_results$S
  E <- seir_results$E
  I <- seir_results$I
  R <- seir_results$R
  t <- seir_results$t
  
  y_max <- max(c(S, E, I, R), na.rm = TRUE)
  
  # 简洁的单面板绘图
  plot(t, S, type = "l", col = "black", lwd = 1.5,
       ylim = c(0, y_max),
       xlab = "Day", ylab = "Population",
       main = main)
  
  lines(t, E, col = "blue", lwd = 1.5)
  lines(t, I, col = "red", lwd = 1.5)
  lines(t, R, col = "green", lwd = 1.5)
  
  # 简化的图例，适合小图
  if (main != "") {
    legend("top", legend = c("S", "E", "I", "R"),
           col = c("black", "blue", "red", "green"),
           lty = 1, lwd = 1.5, bty = "n", cex = 0.7,
           horiz = TRUE)
  }
}


set.seed(123)
beta_random <- runif(n)
alink <- get.net(beta = beta_random, h = h)

# 场景1: 完整模型 (默认参数)
cat("运行场景1: 完整模型\n")
scenario1 <- nseir(beta = beta_random, h = h, alink = alink)

# 场景2: 无家庭和网络结构 (纯随机混合)
cat("运行场景2: 无社会结构 (纯随机混合)\n")
scenario2 <- nseir(beta = beta_random, h = h, alink = alink, 
                   alpha = c(0, 0, 0.04))  # α_h=0, α_c=0, α_r=0.04

# 场景3: 完整结构但均匀社交性
cat("运行场景3: 完整结构但均匀社交性\n")
beta_uniform <- rep(mean(beta_random), n)  # 所有人为平均社交性
alink_uniform <- get.net(beta_uniform, h)
scenario3 <- nseir(beta = beta_uniform, h = h, alink = alink_uniform)

# 场景4: 均匀社交性 + 纯随机混合
cat("运行场景4: 均匀社交性 + 纯随机混合\n")
scenario4 <- nseir(beta = beta_uniform, h = h, alink = alink_uniform, 
                   alpha = c(0, 0, 0.04))

# 绘制四种场景的对比图
plot_seir_comparison(
  list(scenario1, scenario2, scenario3, scenario4),
  c("完整模型\n(随机beta + 社会结构)", 
    "无社会结构\n(随机beta + 纯随机混合)", 
    "均匀社交性\n(平均beta + 社会结构)", 
    "均匀社交性+无结构\n(平均beta + 纯随机混合)"),
  "社会结构对疫情传播的影响"
)
















































