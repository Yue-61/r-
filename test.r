set.seed(123)  # 设置随机种子以便结果可重现
beta <- c(0.5, 1.0, 1.5, 0.8, 1.2)
h <- c(1, 1, 2, 2, 2)
a <- get_net(beta = beta, h = h)

# 查看结果
print(a)
