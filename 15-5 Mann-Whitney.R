library(tidyverse)
library(ggpubr)

##### EXAMPLE 1 #####

ill <- c(640, 160, 1280, 320, 80, 640, 640, 160, 1280, 640, 160)
healthy <- c(10, 320, 160, 160, 320, 320, 10, 320, 320, 80, 640)

i <- tibble(ill)
h <- tibble(healthy)

p1 <- ggplot(data=i, aes(sample=ill)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Ill")+
  theme_minimal()

p2 <- ggplot(data=h, aes(sample=healthy)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Healthy")+
  theme_minimal()

both <- ggarrange(p1, p2, ncol=2, nrow=1)
both

#ggsave("/Users/sseals/Desktop/L71fig1.png")

outcome <- c(640, 160, 1280, 320, 80, 640, 640, 160, 1280, 640, 160,
             10, 320, 160, 160, 320, 320, 10, 320, 320, 80, 640)
healthy <- c(rep(0,11), rep(1,11))

data <- tibble(outcome, healthy)

wilcox.test(data$outcome~data$healthy, exact = FALSE, alternative="greater")
# left side of the ~ is the outcome of interest
# right side of the ~ is the grouping variable

# assign the alternative "greater" because we want to know if the 
# ill group (healthy=0) is greater than the healthy group (healthy=1)
# R will do the comparison in the order that it sees the grouping
# and it sees the 0 group first

##### EXAMPLE 2 #####

conf <- c(9.6, 10.4, 9.7, 10.3, 9.2, 9.3, 9.9, 9.5, 9.0, 10.9)
not <- c(10.7, 10.7, 10.4, 10.9, 10.5, 10.3, 9.6, 11.1, 11.2, 10.4)

c <- tibble(conf)
n <- tibble(not)

p1 <- ggplot(data=c, aes(sample=conf)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Confined")+
  theme_minimal()

p2 <- ggplot(data=n, aes(sample=not)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Not Confined")+
  theme_minimal()

both <- ggarrange(p1, p2, ncol=2, nrow=1)
both

#ggsave("/Users/sseals/Desktop/L71fig2.png")

brain <- c(9.6, 10.4, 9.7, 10.3, 9.2, 9.3, 9.9, 9.5, 9.0, 10.9,
           10.7, 10.7, 10.4, 10.9, 10.5, 10.3, 9.6, 11.1, 11.2, 10.4)

notconf <- c(rep(0,10), rep(1,10))

data <- tibble(brain, notconf)

wilcox.test(data$brain~data$notconf, alternative="two.sided", exact = FALSE)

##### EXAMPLE 3 - LARGE SAMPLE SIZE #####

X <- c(7, 76 , 7 , 33 , 4 , 20 , 4 , 59 , 91 , 5 , 287 , 472 , 52 , 19 , 128 , 28 , 103 , 25 , 68 , 17 , 109 , 3)
Y <- c(115 , 412 , 200 , 55 , 62 , 253 , 219 , 225 , 122 , 245 , 129 , 168 , 239 , 71 , 118 , 130 , 12 )

x <- tibble(X)
y <- tibble(Y)

p1 <- ggplot(data=x, aes(sample=X)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Model X")+
  theme_minimal()

p2 <- ggplot(data=y, aes(sample=Y)) +
  stat_qq(size=3) + 
  stat_qq_line() + 
  xlab("Theoretical") +
  ylab("Sample") +
  ggtitle("Model Y")+
  theme_minimal()

both <- ggarrange(p1, p2, ncol=2, nrow=1)
both

#ggsave("/Users/sseals/Desktop/L72fig1.png")


outcome <- c(7, 76 , 7 , 33 , 4 , 20 , 4 , 59 , 91 , 5 , 287 , 472 , 52 , 19 , 128 , 28 , 103 , 25 , 68 , 17 , 109 , 3,
             115 , 412 , 200 , 55 , 62 , 253 , 219 , 225 , 122 , 245 , 129 , 168 , 239 , 71 , 118 , 130 , 12 )

X <- c(rep(1,22), rep(0,17))

data <- tibble(outcome, X)

data$rank <- rank(data$outcome, ties.method = "average")

data %>% 
  group_by(X) %>%
  summarize_at(vars(rank),
               list(name = sum))
  

wilcox.test(data$outcome~data$X, alternative="greater", exact = FALSE)
