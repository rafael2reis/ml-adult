require(ggplot2)

ricos <- adult[adult[,15] == ">50K", ]
summary(ricos)

# Graficos ---------------------------------------
ggplot(ricos) +
  geom_histogram(aes(x=age),
                 binwidth=2, fill="gray")

ggplot(adult) + geom_bar(aes(x=education, fill=over50K), position="dodge")

ggplot(adult) + geom_bar(aes(x=occupation, fill=over50K), position="dodge")

ggplot(adult) + geom_bar(aes(x=relationship, fill=over50K), position="dodge")