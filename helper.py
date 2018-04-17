import glob
import io
import math
import os

import numpy as np
import pandas as pd
import seaborn as sns
import tensorflow as tf
from IPython import display
from matplotlib import cm, gridspec
from matplotlib import pyplot as plt
from sklearn import metrics
from tensorflow.python.data import Dataset

tf.logging.set_verbosity(tf.logging.ERROR)
pd.options.display.max_rows = 10
pd.options.display.float_format = '{:.1f}'.format


class tf_basic_model:
    def parse_labels_and_features(dataset):
        print(len(dataset.columns))
        sys.exit(0)
        if len(dataset.columns) > 784:
            labels = dataset[0]
            features = dataset.loc[:, 1:784]
        else:
            labels = dataset[0]
            features = dataset.loc[:, 0:784]
        features = features / 255
        return labels, features

    def construct_feature_columns():
        return set([tf.feature_column.numeric_column('pixels', shape=784)])

    def create_training_input_fn(features, labels, batch_size, num_epochs=None, shuffle=True):
        def _input_fn(num_epochs=None, shuffle=True):
            idx = np.random.permutation(features.index)
            raw_features = {'pixels': features.reindex(idx)}
            raw_targets = np.array(labels[idx])

            ds = Dataset.from_tensor_slices((raw_features, raw_targets))
            ds = ds.batch(batch_size).repeat(num_epochs)

            if shuffle:
                ds = ds.shuffle(10000)

            feature_batch, label_batch = ds.make_one_shot_iterator().get_next()
            return feature_batch, label_batch
        return _input_fn

    def create_perdict_input_fn(features, labels, batch_size):
        def _input_fn():
            raw_features = {'pixels': features.values}
            raw_targets = np.array(labels)

            ds = Dataset.from_tensor_slices((raw_features, raw_targets))
            ds = ds.batch(batch_size)

            feature_batch, label_batch = ds.make_one_shot_iterator().get_next()
            return feature_batch, label_batch
        return _input_fn

    def train_nn_regression_model(
            steps,
            batch_size,
            hidden_units,
            training_examples,
            training_targets,
            validation_examples,
            validation_targets,
            testing_examples,
            testing_targets):
        periods = 10
        steps_per_period = steps / periods

        predict_training_input_fn = tf_basic_model.create_perdict_input_fn(training_examples, training_targets, batch_size)
        predict_validation_input_fn = tf_basic_model.create_perdict_input_fn(validation_examples, validation_targets, batch_size)
        training_input_fn = tf_basic_model.create_training_input_fn(training_examples, training_targets, batch_size)

        my_optimizer = tf.train.AdamOptimizer(1e-4)
        my_optimizer = tf.contrib.estimator.clip_gradients_by_norm(my_optimizer, 5.0)
        classifier = tf.estimator.DNNClassifier(feature_columns=tf_basic_model.construct_feature_columns(),
                                                hidden_units=hidden_units,
                                                n_classes=10,
                                                optimizer=my_optimizer,
                                                dropout=0.5,
                                                config=tf.estimator.RunConfig(keep_checkpoint_max=1))
        print('Training Model...')
        print('LogLoss error (on validation data):')
        training_errors = []
        validation_errors = []
        for period in range(0, periods):
            classifier.train(input_fn=training_input_fn, steps=steps_per_period)
            training_predictions = list(classifier.predict(input_fn=predict_training_input_fn))
            training_probabilities = np.array([item['probabilities'] for item in training_predictions])
            training_pred_class_id = np.array([item['class_ids'][0] for item in training_predictions])
            training_pred_one_hot = tf.keras.utils.to_categorical(training_pred_class_id, 10)

            validation_predictions = list(classifier.predict(input_fn=predict_validation_input_fn))
            validation_probabilities = np.array([item['probabilities'] for item in validation_predictions])
            validation_pred_class_id = np.array([item['class_ids'][0] for item in validation_predictions])
            validation_pred_one_hot = tf.keras.utils.to_categorical(validation_pred_class_id, 10)

            training_log_loss = metrics.log_loss(training_targets, training_pred_one_hot)
            validation_log_loss = metrics.log_loss(validation_targets, validation_pred_one_hot)

            print(" period %02d : %0.2f" % (period, validation_log_loss))

            training_errors.append(training_log_loss)
            validation_errors.append(validation_log_loss)
        print("Model trianing finished")
        _ = map(os.remove, glob.glob(os.path.join(classifier.model_dir, 'events.out.tfevents')))

        final_predictions = classifier.predict(input_fn=predict_validation_input_fn)
        final_predictions = np.array([item['class_ids'][0] for item in final_predictions])
        accuracy = metrics.accuracy_score(validation_targets, final_predictions)
        print("Final accuracy (on validation data): %0.2f" % accuracy)

        plt.ylabel('Logloss')
        plt.xlabel('Periods')
        plt.title("LogLoss vs. Periods")
        plt.plot(training_errors, label='Training')
        plt.plot(validation_errors, label='Validation')
        plt.legend()
        plt.show()

        cm = metrics.confusion_matrix(validation_targets, final_predictions)
        cm_normalized = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]

        ax = sns.heatmap(cm_normalized, cmap="bone_r")
        ax.set_aspect(1)
        plt.title('Confusion matrix')
        plt.ylabel('True Label')
        plt.xlabel('Predicted Label')
        plt.show()

        predict_testing_input_fn = tf_basic_model.create_perdict_input_fn(testing_examples, testing_targets, batch_size)
        tf_basic_model.submit_prediction(classifier, predict_testing_input_fn)

        return classifier

    def submit_prediction(model, predict_testing_input_fn, filename=None):
        if filename is None:
            filename = 'submission'

        submission = pd.DataFrame()
        predictions = list(model.predict(input_fn=predict_testing_input_fn))
        predictions = np.array([item['class_ids'][0] for item in predictions])
        submission['Label'] = predictions
        submission['ImageId'] = submission.index + 1
        submission = submission.reindex(columns=['ImageId', 'Label'])
        display.display(submission)
        submission.head()
        submission.to_csv('./data/' + filename + '.csv', index=False)
