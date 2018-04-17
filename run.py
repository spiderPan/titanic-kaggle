import sys

import numpy as np
import pandas as pd
from IPython import display
from matplotlib import cm, gridspec
from matplotlib import pyplot as plt

from helper import tf_basic_model

train_dataframe = pd.read_csv('data/train.csv', sep=",", header=None, skiprows=[0])
test_dataframe = pd.read_csv('data/test.csv', sep=',', header=None, skiprows=[0])
train_dataframe = train_dataframe.reindex(np.random.permutation(train_dataframe.index))
print(train_dataframe.describe())
sys.exit(0)
#train_dataframe = train_dataframe.head(12000)
training_targets, training_examples = tf_basic_model.parse_labels_and_features(train_dataframe[:35000])
validation_targets, validation_examples = tf_basic_model.parse_labels_and_features(train_dataframe[35000:42000])

testing_targets, testing_examples = tf_basic_model.parse_labels_and_features(test_dataframe)


classifier = tf_basic_model.train_nn_regression_model(
    steps=100000,
    batch_size=50,
    hidden_units=[1024,512,256,32],
    training_examples=training_examples,
    training_targets=training_targets,
    validation_examples=validation_examples,
    validation_targets=validation_targets,
    testing_examples=testing_examples,
    testing_targets=testing_targets)
