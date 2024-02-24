# Original code by Jacob Schreiber
# Modified by Melody Yin

"""
This module contains a reference implementation of BPNet that can be used
or adapted for your own circumstances. The implementation takes in a
stranded control track and makes predictions for stranded outputs.
"""

import time 
import numpy
import torch

from logging_myin25 import Logger

from tqdm import tqdm

torch.backends.cudnn.benchmark = True


class BPNet_modified(torch.nn.Module):
    """A basic model model with count prediction.

    This model consists of a single Dense layer and an output layer!

    Parameters
    ----------
    n_filters: int, optional
        The number of filters to use per convolution. Default is 64.

    name: str or None, optional
        The name to save the model to during training.
    """

    def __init__(self, name=None):
        super(BPNet_modified, self).__init__()

        self.name = name or "bpnet_modified"

        self.fc1 = torch.nn.Linear(8, 64)
        self.fc2 = torch.nn.Linear(64, 1)
        self.relu = torch.nn.ReLU()

        self.logger = Logger(["Epoch", "Iteration", "Training Time",
            "Training MNLL", "Training Count MSE", 
            "Validation Count Pearson", "Validation Count MSE", 
            "Saved?"], 
            verbose=False)


    def forward(self, x, X_ctl=None):
        # counts prediction
        x = self.fc1(x)
        x = self.relu(x)
        x = self.fc2(x)

        return x


    def predict(self, X, X_ctl=None, batch_size=64, verbose=False):
        """Make predictions for a large number of examples.

        This method will make predictions for a number of examples that exceed
        the batch size. It is similar to the forward method in terms of inputs 
        and outputs, but will run wrapped with `torch.no_grad()` to speed up
        computation and prevent information leakage into the model.


        Parameters
        ----------
        X: torch.tensor, shape=(-1, 4, length)
            The one-hot encoded batch of sequences.

        X_ctl: torch.tensor or None, shape=(-1, n_strands, length)
            A value representing the signal of the control at each position in 
            the sequence. If no controls, pass in None. Default is None.

        batch_size: int, optional
            The number of examples to run at a time. Default is 64.

        verbose: bool
            Whether to print a progress bar during predictions.


        Returns
        -------
        y_profile: torch.tensor, shape=(-1, n_strands, out_length)
            The output predictions for each strand trimmed to the output
            length.
        """


        with torch.no_grad():
            starts = numpy.arange(0, X.shape[0], batch_size)
            ends = starts + batch_size

            y_profiles, y_counts = [], []
            for start, end in tqdm(zip(starts, ends), disable=not verbose):
                X_batch = X[start:end].cuda()
                y_counts_ = self(X_batch)
                y_counts_ = y_counts_.cpu()

                y_counts.append(y_counts_)

            y_counts = torch.cat(y_counts)
            return y_counts

    def fit(self, training_data, optimizer, X_valid=None, X_ctl_valid=None, 
        y_valid=None, max_epochs=100, batch_size=64, validation_iter=100, 
        early_stopping=None, verbose=True):
        """Fit the model to data and validate it periodically.

        This method controls the training of a BPNet model. It will fit the
        model to examples generated by the `training_data` DataLoader object
        and, if validation data is provided, will periodically validate the
        model against it and return those values. The periodicity can be
        controlled using the `validation_iter` parameter.

        Two versions of the model will be saved: the best model found during
        training according to the validation measures, and the final model
        at the end of training. Additionally, a log will be saved of the
        training and validation statistics, e.g. time and performance.


        Parameters
        ----------
        training_data: torch.utils.data.DataLoader
            A generator that produces examples to train on. If n_control_tracks
            is greater than 0, must product two inputs, otherwise must produce
            only one input.

        optimizer: torch.optim.Optimizer
            An optimizer to control the training of the model.

        X_valid: torch.tensor or None, shape=(n, 4, 2114)
            A block of sequences to validate on periodically. If None, do not
            perform validation. Default is None.

        X_ctl_valid: torch.tensor or None, shape=(n, n_control_tracks, 2114)
            A block of control sequences to validate on periodically. If
            n_control_tracks is None, pass in None. Default is None.

        y_valid: torch.tensor or None, shape=(n, n_outputs, 1000)
            A block of signals to validate against. Must be provided if
            X_valid is also provided. Default is None.

        max_epochs: int
            The maximum number of epochs to train for, as measured by the
            number of times that `training_data` is exhausted. Default is 100.

        batch_size: int
            The number of examples to include in each batch. Default is 64.

        validation_iter: int
            The number of batches to train on before validating against the
            entire validation set. When the validation set is large, this
            enables the total validating time to be small compared to the
            training time by only validating periodically. Default is 100.

        early_stopping: int or None
            Whether to stop training early. If None, continue training until
            max_epochs is reached. If an integer, continue training until that
            number of `validation_iter` ticks has been hit without improvement
            in performance. Default is None.

        verbose: bool
            Whether to print out the training and evaluation statistics during
            training. Default is True.
        """

        if X_valid is not None:
            X_valid = X_valid.cuda()
            y_valid_counts = y_valid.sum(dim=2)

        iteration = 0
        early_stop_count = 0
        best_loss = float("inf")
        self.logger.start()

        for epoch in range(max_epochs):
            tic = time.time()

            for data in training_data:
                X, y = data
                X, y = X.cuda(), y.cuda()

                # Clear the optimizer and set the model to training mode
                optimizer.zero_grad()
                self.train()

                print(X)
                print(X.shape)
                # Run forward pass
                y_counts = self(X)

                # Calculate the profile and count losses
                count_loss = log1pMSELoss(y_counts, y.sum(dim=-1).reshape(-1, 1)).mean()
                
                count_loss.backward()
                optimizer.step()
                
                if verbose and iteration % validation_iter == 0:
                    train_time = time.time() - tic

                    with torch.no_grad():
                        self.eval()
                        
                        tic = time.time()
                        y_counts = self.predict(X_valid, X_ctl_valid)
                        
                        # MSE
                        log_true = torch.log(true_counts+1)
                        count_mse = torch.mean(torch.square(log_true - torch.log(y_counts)), dim=-1)
                        
                        # count pearson
                        count_corr = pearson_corr(log_true, y_counts)
                        
                        valid_time = time.time() - tic
                        valid_loss = count_mse

                        self.logger.add([epoch, iteration, train_time, 
                            valid_time, count_loss_, 
                            numpy.nan_to_num(count_corr).mean(), 
                            count_mse.mean().item(),
                            (valid_loss < best_loss).item()])
                        
                        self.logger.save("{}.log".format(self.name))
                        
                        if valid_loss < best_loss:
                            torch.save(self, "{}.torch".format(self.name))
                            best_loss = valid_loss
                            early_stop_count = 0
                        else:
                            early_stop_count += 1

                
                # Extract the profile loss for logging
                loss = count_loss.item()

                if early_stopping is not None and early_stop_count >= early_stopping:
                    break

                iteration += 1

            if early_stopping is not None and early_stop_count >= early_stopping:
                break

        torch.save(self, "{}.final.torch".format(self.name))

def pearson_corr(arr1, arr2):
    """The Pearson correlation between two tensors across the last axis.

    Computes the Pearson correlation in the last dimension of `arr1` and `arr2`.
    `arr1` and `arr2` must be the same shape. For example, if they are both
    A x B x L arrays, then the correlation of corresponding L-arrays will be
    computed and returned in an A x B array.

    Parameters
    ----------
    arr1: torch.tensor
        One of the tensor to correlate.

    arr2: torch.tensor
        The other tensor to correlation.

    Returns
    -------
    correlation: torch.tensor
        The correlation for each element, calculated along the last axis.
    """

    mean1 = torch.mean(arr1, axis=-1).unsqueeze(-1)
    mean2 = torch.mean(arr2, axis=-1).unsqueeze(-1)
    dev1, dev2 = arr1 - mean1, arr2 - mean2

    sqdev1, sqdev2 = torch.square(dev1), torch.square(dev2)
    numer = torch.sum(dev1 * dev2, axis=-1)  # Covariance
    var1, var2 = torch.sum(sqdev1, axis=-1), torch.sum(sqdev2, axis=-1)  # Variances
    denom = torch.sqrt(var1 * var2)
   
    # Divide numerator by denominator, but use 0 where the denominator is 0
    correlation = torch.zeros_like(numer)
    correlation[denom != 0] = numer[denom != 0] / denom[denom != 0]
    return correlation