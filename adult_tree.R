install.packages('tree')
library(tree)

# De treino
tree.adult <- tree(outcome ~. - outcome, adult)
