# Xác định hệ số hồi quy mẫu bằng phương pháp bình phương cực tiểu

#Lấy dữ liệu từ file excel
setwd("C:/Users/admin/OneDrive - Hanoi University of Science and Technology/
      Tài liệu học tập/Toán ứng dụng/Suy luận thống kê/Code R")
data <- read.csv("bds.csv", header = TRUE)
colnames(data)[1] <- "y"
y <- data[, 2]
x <- data[, 3:6]

#Tính toán trực tiếp
# Tìm hệ số hồi quy
beta <- function(x, y) {
  tmp <- matrix(1, nrow = nrow(x))  # Tạo ma trận toàn 1 có số hàng, cột bằng ma trận X
  x <- as.matrix(x)   #Định nghĩa kiểu dữ liệu ma trận cho X
  x <- cbind(tmp, x)  # Ghép thêm 1 cột toàn số 1 vào X
  y <- as.matrix(y)
  # print(x)
  # print(y)
  beta <- solve(t(x) %*% x) %*% t(x) %*% y    # Tính theo công thức
  return(beta)
}
# Phương trình hồi quy tuyến tính
pthqtt <- function(beta) {
  cat("\nPhương trình hồi quy: ")
  cat("y = ", beta[1])
  for (i in 2:length(beta)) {
    cat(" +", beta[i], ".x", i - 1)
  }
}

beta <- beta(x, y)
pthqtt(beta)


# Sử dụng hàm có sẵn
x1 <- x[,1]
x2 <- x[,2]
x3 <- x[,3]
x4 <- x[,4]

model <- lm(y ~ x1 + x2 + x3 + x4)
coefficients(model)
summary(model)


