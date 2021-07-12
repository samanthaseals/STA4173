library(tidyverse)

# create data
chill <- c(36, 31, 25, 19, 13, 7, 1, -5, -11, -16, -22, -28, -34, -40, -46, -52, -57, -63,
           34, 27, 21, 15, 9, 3, -4, -10, -16, -22, -28, -35, -41, -47, -53, -59, -66, -72,
           32, 35, 19, 13, 6, 0, -7, -13, -19, -26, -32, -39, -45, -51, -58, -64, -71, -77,
           30, 24, 17, 11, 4, -2, -9, -15, -22, -29, -35, -42, -48, -55, -61, -68, -74, -81,
           29, 23, 16, 9, 3, -4, -11, -17, -24, -31, -37, -44, -51, -58, -64, -71, -78, -84,
           28, 22, 15, 8, 1, -5, -12, -19, -26, -33, -39, -46, -53, -60, -67, -73, -80, -87,
           28, 21, 14, 7, 0, -7, -14, -21, -27, -34, -41, -48, -55, -62, -69, -76, -82, -89,
           27, 20, 13, 6, -1, -8, -15, -22, -29, -36, -43, -50, -57, -64, -71, -78, -84, -91,
           26, 19, 12, 5, -2, -9, -16, -23, -30, -37, -44, -51, -58, -65, -72, -79, -86, -93,
           26, 19, 12, 4, -3, -10, -17, -24, -31, -38, -45, -52, -60, -67, -74, -81, -88, -95,
           25, 18, 11, 4, -3, -11, -18, -25, -32, -39, -46, -54, -61, -68, -75, -82, -89, -97,
           25, 17, 10, 3, -4, -11, -19, -26, -33, -40, -48, -55, -62, -69, -76, -84, -91, -98)

wind <- c(rep(5, 18), rep(10, 18), rep(15, 18), rep(20, 18), rep(25, 18), rep(30, 18), rep(35, 18),
          rep(40, 18), rep(45, 18), rep(50, 18), rep(55, 18), rep(60, 18))

temp <- c(rep(seq(40, -45, -5), 12))

data <- tibble(wind, temp, chill)         

# construct model with interaction
m1 <- lm(chill ~ temp + wind + temp:wind)
summary(m1)

# visualize

# expected chill on y axis
# wind speed on x axis
# lines defined by temperature

# grab coefficients
c1 <- coefficients(m1)

# line for temp of 40
data$t40 <- c1[1] + c1[2]*40 + c1[3]*wind + c1[4]*40*wind

# line for temp of 0
data$t0 <- c1[1] + c1[2]*0 + c1[3]*wind + c1[4]*0*wind

# line for temp of -45
data$tn45 <- c1[1] + c1[2]*(-45) + c1[3]*wind + c1[4]*(-45)*wind

# plot
ggplot(data, aes(x = wind)) +
  geom_line(aes(y = tn45)) +
  geom_line(aes(y = t0)) +
  geom_line(aes(y = t40)) + 
  xlab("Wind Speed") + 
  ylab("Wind Chill") +
  geom_text(aes(x = 63 , y = -101, label = "Temp -45")) +
  geom_text(aes(x = 63 , y = -35, label = "Temp 0")) + 
  geom_text(aes(x = 63 , y = 24, label = "Temp 40")) +
  theme_minimal() +
  theme(text = element_text(size=20)) 

#ggsave("/Users/sseals/Desktop/L95fig5.png")

# expected chill on y axis
# temperature on x axis
# lines defined by wind speed

# line for wind 5
data$w5 <- c1[1] + c1[2]*temp + c1[3]*5 + c1[4]*temp*5

# line for wind 20
data$w20 <- c1[1] + c1[2]*temp + c1[3]*20 + c1[4]*temp*20

# line for wind 50
data$w50 <- c1[1] + c1[2]*temp + c1[3]*50 + c1[4]*temp*50

# plot
ggplot(data, aes(x = temp)) +
  geom_line(aes(y = w5)) +
  geom_line(aes(y = w20)) +
  geom_line(aes(y = w50)) + 
  xlab("Temperature") + 
  ylab("Wind Chill") +
  theme_minimal() +
  geom_text(aes(x = 44.5 , y = 36.5, label = "Wind 5 ")) +
  geom_text(aes(x = 45 , y = 32.5, label = "Wind 20")) + 
  geom_text(aes(x = 45 , y = 25, label = "Wind 50")) +
  theme(text = element_text(size=20)) 

#ggsave("/Users/sseals/Desktop/L95fig6.png")
