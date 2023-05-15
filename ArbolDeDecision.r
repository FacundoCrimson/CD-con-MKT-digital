# Instalar paquetes si es necesario
install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("rattle")
install.packages("neuralnet")

# Inicializamos valores Constantes
oferta_can <- 43.8
oferta_no_can <- 66.3
precio_no_can <- 72

# Creamos una funcion para evaluar la precision de los modelos
result <- function(pred, train) {
    # Creamos la matriz de confusion
    confu_matrix <- table(train$TARGET, pred)
    # Calculamos la precision del modelo
    global <- sum(diag(confu_matrix)) / nrow(train)
    cancellers <- ((confu_matrix[1, 1])) / sum(confu_matrix[1, ])
    no_cancellers <- ((confu_matrix[2, 2])) / sum(confu_matrix[2, ])
    # Cantidad respectivas del TARGET
    n_can <- nrow(train[train$TARGET == 1, ])
    n_no_can <- nrow(train[train$TARGET == 0, ])
    # Calculamos la Utilidad al detalle
    util_can <- n_can * cancellers * oferta_can
    util_no_can_n <- n_no_can * no_cancellers * precio_no_can
    util_no_can_o <- n_no_can * (1 - no_cancellers) * oferta_no_can
    predict_util <- util_can + util_no_can_n + util_no_can_o
    total_util <- n_can * oferta_can + n_no_can * precio_no_can
    cat(paste(
        "\nPrecision Global ->", global,
        "\n  Precision Cancellers ->", cancellers,
        "\n  Precision No Cancellers ->", no_cancellers,
        "\n\nPorcentaje de la Utilidad obtenida con el modelo ->",
        predict_util / total_util,
        "\n"
    ))
}

library(readxl)
# Leemos el excel separandolo en train y test cada sheet
train <- read_excel(path = "datos taller4.xls", sheet = "train")
test <- read_excel(path = "datos taller4.xls", sheet = "test")

# Drop filas que tienen valores NA y eliminamos la columna ID
train <- train[complete.cases(train), ]
test <- test[complete.cases(test), ]
train <- train[, -c(1)]
# Vizualizamos train
glimpse(train)
head(train)
summary(train)

#-------------------------ARBOL DE DECISION-------------------------
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot)

# Creamos la formula
vi <- names(train)[1:length(names(train)) - 1] # nolint
target <- "TARGET"
formula <- reformulate(vi, target)

# Creamos un modelo de arbol de decision
set.seed(1234)
tree <- rpart(
    formula,
    data = train,
    method = "class",
    control = rpart.control(
        cp = .002,
        minsplit = 20,
    )
)
summary(tree)
# Analizamos los CPs
printcp(tree)
plotcp(tree)
# Vemos el arbol sin podar
fancyRpartPlot(tree, main = "TARGET")


# Comparamos resultados para podar segun los distintos CP
for (identifier in tree$cptable[, "CP"]) {
    cat(paste("\nUsando el siguiente CP ->", identifier, "\n"))
    tree_fit <- prune(tree, cp = identifier)
    pred <- predict(tree_fit, newdata = train, type = "class")
    result(pred, train)
}
# Realizamos el podado con el mejor CP
tree_fit <- prune(tree, cp = 0.00325251330573625)
pred <- predict(tree_fit, newdata = train, type = "class")
result(pred, train)
# Graficamos el Arbol de Decision Podado
fancyRpartPlot(tree_fit, main = "TARGET")
# Utilizamos el metodo y guardamos la info en la variable test
pred_test <- predict(tree_fit, newdata = test, type = "class")
test <- cbind(test, pred_test)
view(test)
