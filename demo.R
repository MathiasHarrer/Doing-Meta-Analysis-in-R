library(ggplot2)
set.seed(888)
rnorm(n,mean = 20,sd = 2)
plot(rnorm）



# 假设你的数据框如下：
df <- data.frame(
    category = c("A", "B", "C", "D", "E"),
    value = c(23, 45, 67, 89, 56)
)

# 使用 ggplot2 创建柱状图
ggplot(df, aes(x=category, y=value)) +
    geom_bar(stat="identity", fill="steelblue") +
    labs(x="Category", y="Value") +
    ggtitle("Bar Chart of Value by Category") +
    theme_minimal()

