height <- c(189, 170, 189, 163, 183, 171, 185, 168, 173, 183, 173, 173, 175, 178,
            183, 193, 178, 173, 174, 183, 183, 168, 170, 178, 182, 180, 183, 178,
            182, 188, 175, 179, 183, 193, 182, 183, 177, 185, 188, 188, 182, 185,
            188)

t.test(height, mu = 176.3, alternative = "greater")

# must specify alternative = "two.sided" for proper CI
t.test(height, mu = 176.3, alternative = "two.sided")
