library(caret)

####Read in MFCC features averaged across all time windows


#assigning numbers to labels

mfcc.train <- read.csv("Filename")
mfcc.train <- mfcc.train[,1:25]
mfcc.train[,1:24] = (mfcc.train[,1:24]- min(mfcc.train[,1:24]))/(max(mfcc.train[,1:24])-min(mfcc.train[,1:24]))


mfcc.test <- read.csv("filename")
mfcc.test <- mfcc.test[,1:25]
mfcc.test[,1:24] = (mfcc.test[,1:24]- min(mfcc.test[,1:24]))/(max(mfcc.test[,1:24])-min(mfcc.test[,1:24]))


library(e1071)
classifier_linear = svm(formula = mfcc.train$loc ~ .,
                 data = mfcc.train,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred_linear = predict(classifier_linear, newdata = mfcc.test[-25])

cm_linear = table(mfcc.test[, 25], y_pred_linear)
print(cm_linear)


cm_linear = confusionMatrix(mfcc.test[, 25], y_pred_linear)
classifier_polynomial = svm(formula = mfcc.train$loc ~ .,
                            data = mfcc.train,
                            type = 'C-classification',
                            kernel = 'polynomial')

# Predicting the Test set results
y_pred_polynomial = predict(classifier_polynomial, newdata = mfcc.test[-25])

cm_polynomial = table(mfcc.test[, 25], y_pred__polynomial)
print(cm_polynomial)

cm_poly = confusionMatrix(mfcc.test[, 25], y_pred_polynomial)
classifier_sigmoid = svm(formula = mfcc.train$loc ~ .,
                         data = mfcc.train,
                         type = 'C-classification',
                         kernel = 'sigmoid')

# Predicting the Test set results
y_pred_sigmoid = predict(classifier_sigmoid, newdata = mfcc.test[-25])

cm_sigmoid = table(mfcc.test[, 25], y_pred__sigmoid )
print(cm_sigmoid)
cm_sigmoid = confusionMatrix(mfcc.test[, 25], y_pred_sigmoid)

classifier_radial = svm(formula = mfcc.train$loc ~ .,
                        data = mfcc.train,
                        type = 'C-classification',
                        kernel = 'radial')

# Predicting the Test set results
y_pred_radial = predict(classifier_radial, newdata = mfcc.test[-25])

model_c= classifier_radial 
cm_radial = table(mfcc.test[, 25], y_pred_radial)

print(cm_radial)

cm_radial1= confusionMatrix(mfcc.test[, 25], y_pred_radial)


