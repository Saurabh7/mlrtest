library(mlr)

# Parameters
num_cluster = 3L
c = 1L
sigma = 1L

# Load the dataset and preprocess.
data(BreastCancer, package = 'mlbench')
d = BreastCancer
d = d[complete.cases(d),]
d$Id =NULL
d_cl=d
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
d_cl=factorsNumeric(d_cl)
d_cl$Class=NULL

# CLUSTERING TRAINING
# Split into train and test subsets.
cluster.task = makeClusterTask(data = d_cl)
size = getTaskSize(cluster.task)
set = seq(1, size)
train = sample(1:size, size=as.integer(0.8*size))
train.set = set[train]
test.set = set[-train]

# Perform K-means clustering.
cluster.lrn = makeLearner("cluster.kmeans", centers = num_cluster)
cluster_mod = train(cluster.lrn, cluster.task, subset = train.set)
model = cluster_mod$learner.model
cluster_list = list()
for (n in 1:num_cluster)
{
  cluster_list[[n]] = seq(length(model$cluster))[model$cluster == n]
}

# CLASSIFICATION TRAINING
class = d$Class
d$Class=NULL
d = factorsNumeric(d)
d$Class = class
classif.task = makeClassifTask(data = d, target = "Class",  positive = "malignant")
classif.lrn = makeLearner("classif.ksvm", C = c, sigma = sigma)


# Generate Proper SVM models for each cluster
model_list = list()
one_class_cluster = list()
for (n in 1:num_cluster)
{ 
  #check for clusters containing only one class.
  temp_data = getTaskData(classif.task, subset = cluster_list[[n]])
  check = length(seq(length(temp_data$Class))[temp_data$Class == temp_data$Class[[1]]])
  model_list[[n]] = NA
  one_class_cluster[[n]] = NA
  if (check == length(temp_data$Class) || check == 0)
  {
    one_class_cluster[[n]] = temp_data$Class[[1]]
  } else {  
    mod = train(classif.lrn, classif.task, subset = cluster_list[[n]])
    model_list[[n]] = mod
  } 
}

# PREDICTION
cluster_pred = predict(cluster_mod, task = cluster.task, subset = test.set)

# Predict using appropriate svm model, RETURN Label if the cluster has only one class.
prediction = list()
accuracy = 0
for (n in 1:num_cluster)
{
  test_data = cluster_pred$data$id[cluster_pred$data$response == n]
  if (is.na(model_list[[n]]))
  {
    prediction[[n]] = one_class_cluster[[n]]
    temp_data = getTaskData(classif.task, subset = test_data)
    accuracy = accuracy + sum(temp_data$Class == prediction[[n]])
  } else {    
    prediction[[n]] = predict(model_list[[n]], task = classif.task, subset = test_data)
    accuracy = accuracy + sum(prediction[[n]]$data$truth == prediction[[n]]$data$response)
  }
}
accuracy = accuracy / length(cluster_pred$data$id)

print (prediction)
print (accuracy)
