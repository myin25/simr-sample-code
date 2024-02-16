/*
 * Melody Yin
 * Created February 1 2024
 * 
 * This file implements an A-B-1 network (a three layer network with a variable number of nodes for the
 * input and hidden layers) based on a design document provided by Dr. Eric Nelson in the ATCS: Neural
 * Networks class.
 * 
 * The error function used for training is calculated with gradient descent.
 * 
 * Methods:
 *    main: initializes instance of AB1_Net class and calls functions to either run or train an AB1 network.
 *    parametrize: set parametrization values (the AB1 network parameters are hardcoded inside the function).
 *    echoparams: prints out the network parameters set in the parametrize function (described above) for 
 *       user's reference.
 *    allocate_train: allocates space necessary for arrays used when training the AB1 network (includes 
 *       deriv/gradient arrays that aren't used in allocate_run).
 *    allocate_run: allocates space necessary for arrays used when running the AB1 network.
 *    populateRandom: randomly populates weights in the range (minRand, maxRand) using generate_random_value.
 *    generate_random_value: returns a random value in the range (minRand, maxRand).
 *    populateHardCode: populates weights manually, can be modified by the user.
 *    train: trains the AB1 network using gradient descent.
 *    sigmoid: applies a sigmoid to the input.
 *    derive_sigmoid: applies the derivative of a sigmoid function to the input.
 *    reportTrainingResults: prints out the results from training the model.
 *    run: takes in input and runs the network based on its current weights, uses theta variable.
 *    run_train: takes in input and runs the network based on its current weights, uses theta array. used in
 *       training to report results.
 *    reportRunningResults: reports results from running.
 *    runTestCasesRun: runs network on specified input, returns predictions
 *    runTestCasesTrain: runs network on specified input, returns predictions
 */
public class AB1_Net 
{
   /*
    * Variables for initializing model state and parameters.
    *
    * training: which mode the model is in; if true, it's training. if false, 
    *   it's running.
    *
    * nLayers: number of layers in the model. for the A-B-1 model, it's set to 3.
    * nCases: number of cases for training the model.
    * nInEachInput: number of values in each input case
    * maxIter: maximum number of iterations the model can go through before exiting
    *    training. used only in training and not running.
    *
    * numActs: configuration number of activations in each layer.
    * 
    * Training associated variables.
    * minRand: minimum value when generating random values.
    * maxRand: maximum value when generating random values.
    * lambda: size of 'step' model takes when updating weights in gradient descent.
    * maxError: error limit; when training, model will exit if its error <= maxError.
    *
    * Data:
    * outputs: the expected model outputs.
    * inputs: input data for the network.
    */
   boolean training;
   int nLayers, nCases, nInEachInput, maxIter;
   int[] numActs;
   double minRand, maxRand, lambda, maxError;

   double[] outputs;
   double[][] inputs;

   /*
    * totalError: total model error in an iteration.
    *
    * a: input activations for network.
    * h: hidden activations for network.
    * theta: accumulating function for hidden activations.
    *    used in training and not running. theta is stored in runtheta during running.
    * omegas: omega values for hidden activations.
    *    used in training and not running.
    * psi: psi values for output node.
    *    used in training and not running.
    *
    * weights1: the weight values between input and hidden activations.
    * weights2: the weight values between hidden and output activations.
    *
    * runtheta: used to store theta value exclusively in running
    *
    * The following four arrays are used exclusively in training and not running.
    * gradient1: change in weights1 in each iteration for training.
    * gradient2: change in weights2 in each iteration for training.
    * deriv1: derivatives before lambda is applied for weights1.
    * deriv2: derivatives before lambda is applied for weights2.
    */
   double[] totalError; 
   double[] a, h, theta, omegas, psi;
   double runtheta;
   double[][] weights1, weights2, gradient1, gradient2, deriv1, deriv2;

   /*
    * Error associated variables used during training.
    * iteration: the number of iterations the model has gone through.
    *
    * avgErr: the average model error within an iteration.
    *
    * thetaOut: theta value of the output activation.
    * psiOut: psi value of the output activation.
    * caseError: model's error for a specific test case.
    */
   int iteration;
   double avgErr;
   double thetaOut, psiOut, caseError;

   /*
    * Main method. Depending on if the network is in running or training mode, it will
    *    call the corresponding functions to parametrize the network, allocate space,
    *    populate weights, run/train, and report the results.
    * 
    * @param String[] args -- command line arguments
    */
   public static void main(String[] args)
   {
      AB1_Net net = new AB1_Net();

      net.parametrize();
      net.echoparams();

      if (net.training)
      {
         // will need to allocate different arrays for training in comparison to running.
         net.allocate_train();

         /* 
          * manually populate input/output. randomly populate weights, since they won't be 
          *    hardcoded by the user like in running. optimize performance in train.
          */
         net.populateRandom();
         net.train(net.inputs, net.outputs);

         net.reportTrainingResults(net.inputs, net.outputs, net.runTestCasesTrain(net.inputs));
      }    // if (net.training)

      else // running model based on weights pre-set by the user
      {
         net.allocate_run();

         // populate input/output and weights based on user input
         net.populateHardCode();

         net.reportRunningResults(net.inputs, net.outputs, net.runTestCasesRun(net.inputs));
      }  // else

   }     // public static void main(String[] args)

   /*
    * Defines basic parameters of the model.
    */
   public void parametrize()
   {
      training = false;
      
      // Currently a 3-layer A-B-1 network.
      nLayers = 3;
      numActs = new int[]{2, 1, 1};

      nCases = 4;
      nInEachInput = 2;

      if (training) // define additional necessary parameters if training
      { 
         minRand = -1.5;
         maxRand = 1.5;

         maxIter = 100000;
         lambda = 0.3;
         maxError = 2E-4;
      }  // if (training)

   } // public void parametrize()

   /*
    * Prints out model parameters (number of layers, node configuration, randomization range, etc
    *    state (training or running) for user's reference.
    */
   public void echoparams()
   {
      System.out.println("Network configuration:");
      System.out.print(nLayers + " layer network with the following node configuration: ");

      // Print node configuration in a single line from list
      for (int layer = 0; layer < nLayers - 1; layer++)
      {
         System.out.print(numActs[layer] + "-");
      }
      System.out.println(numActs[nLayers - 1]);

      System.out.println("Weights can be randomized from the range " + minRand + " to " + maxRand);

      if (training) // some additional information available for training
      {
         System.out.println("Model is in training mode.");
         System.out.println("Training on " + nCases + " cases.");

         System.out.println("Lambda: " + lambda);
         System.out.println("Error Threshold: " + maxError);
         System.out.println("Max number of iterations is: " + maxIter);
   
      }     // if training
      else  // running
      {
         System.out.println("Model is in running mode.");
         System.out.println("Running on " + nCases + " cases.");
      }

   } // public void echoparams()

   /*
    * Allocates memory for arrays needed for training.
    */
   public void allocate_train()
   {
      totalError = new double[nCases];

      inputs = new double[nCases][];
      for (int case_ = 0; case_ < nCases; case_++)
      {
         inputs[case_] = new double[nInEachInput];
      }

      outputs = new double[nCases];

      a = new double[numActs[0]];
      h = new double[numActs[1]];
      
      theta = new double[numActs[1]];
      omegas = new double[numActs[1]];
      psi = new double[numActs[1]];

      /* 
       * AB1 networks guaranteed to have exactly 3 layers; since 3D matrices aren't allowed
       *    in the design document, store the weights, gradients, and derivatives in two 2D 
       *    matrices apiece, one for each pair of consecutive layers.
       */

      // in between the input and hidden nodes
      weights1 = new double[numActs[0]][numActs[1]];
      gradient1 = new double[numActs[0]][numActs[1]];
      deriv1 = new double[numActs[0]][numActs[1]];

      for (int k = 0; k < numActs[0]; k++)
      {
         weights1[k] = new double[numActs[1]];
         gradient1[k] = new double[numActs[1]];
         deriv1[k] = new double[numActs[1]];
      }

      // in between the hidden and output nodes
      weights2 = new double[numActs[1]][numActs[2]]; 
      gradient2 = new double[numActs[1]][numActs[2]];
      deriv2 = new double[numActs[1]][numActs[2]];

      for (int j = 0; j < numActs[1]; j++)
      {
         weights2[j] = new double[numActs[2]];
         gradient2[j] = new double[numActs[2]];
         deriv2[j] = new double[numActs[2]];
      }

      totalError = new double[nCases];
   } // public void allocate_train()

   /*
    * Allocates arrays needed for running.
    */
   public void allocate_run()
   {
      inputs = new double[nCases][];
      for (int case_ = 0; case_ < nCases; case_++)
      {
         inputs[case_] = new double[nInEachInput];
      }

      outputs = new double[nCases];

      // arrays corresponding with input activation and hidden activations
      a = new double[numActs[0]]; 
      weights1 = new double[numActs[0]][];

      // arrays corresponding with hidden activations and output activations
      runtheta = 0.0;

      h = new double[numActs[1]]; 
      weights2 = new double[numActs[1]][];
 
      for (int k = 0; k < numActs[0]; k++) 
      { 
         weights1[k] = new double[numActs[1]]; 
      }

      for (int j = 0; j < numActs[1]; j++) 
      { 
         weights2[j] = new double[numActs[2]]; 
      }
   } // allocate_run()

   /*
    * Randomly populates weights array with values in the range (minRand, maxRand).
    */
   public void populateRandom()
   {
      // populate inputs and outputs
      inputs[0][0] = 0.0;
      inputs[0][1] = 0.0;
      inputs[1][0] = 0.0;
      inputs[1][1] = 1.0;
      inputs[2][0] = 1.0;
      inputs[2][1] = 0.0;
      inputs[3][0] = 1.0;
      inputs[3][1] = 1.0;

      outputs[0] = 0.0;
      outputs[1] = 1.0;
      outputs[2] = 1.0;
      outputs[3] = 0.0;

      // populate weights randomly
      for (int currNode = 0; currNode < numActs[0]; currNode++)    // each node in input layer
      {
         for (int nextNode = 0; nextNode < numActs[1]; nextNode++) // each node in hidden layer
         {
            // fill in each weight in weights1 with a random value
            weights1[currNode][nextNode] = generate_random_value();
         }
      } // for (int currNode = 0; currNode < numActs[0]; currNode++)

      for (int currNode = 0; currNode < numActs[1]; currNode++)    // each node in hidden layer
      {
         for (int nextNode = 0; nextNode < numActs[2]; nextNode++) // each node in output layer
         {
            // fill in each weight in weights2 with a random value
            weights2[currNode][nextNode] = generate_random_value();
         }
      } // for (int currNode = 0; currNode < numActs[1]; currNode++)

   } // public void populateRandom()

   /*
    * Generates random value in the range (minRand, maxRand).
    *
    * @return random value
    */
   public double generate_random_value()
   {
      return (maxRand - minRand) * Math.random() + minRand;
   }

   /*
    * Manually enters weights following user input.
    */
   public void populateHardCode()
   {
      // populate inputs and outputs
      inputs[0][0] = 0.0;
      inputs[0][1] = 0.0;
      inputs[1][0] = 0.0;
      inputs[1][1] = 1.0;
      inputs[2][0] = 1.0;
      inputs[2][1] = 0.0;
      inputs[3][0] = 1.0;
      inputs[3][1] = 1.0;

      outputs[0] = 0.0;
      outputs[1] = 1.0;
      outputs[2] = 1.0;
      outputs[3] = 0.0;

      // populate weights
      weights1[0][0] = 0.1;
      weights1[1][0] = 0.2;
      weights1[0][1] = 0.3;
      weights1[1][1] = 0.4;

      weights2[0][0] = 0.5;
      weights2[1][0] = 0.6;
   } // public void populateHardCode()

   /*
    * Trains the network using gradient descent.
    *
    * @param inputs: input data used to train network
    * @param outputs: expected outputs for network
    */
   public void train(double[][] inputs, double[] outputs)
   {
      double predicted, omega;
      
      // reset iteration/errors to 0
      iteration = 0;
      avgErr = 0.0;

      /* 
       * while under max number of iterations allowed or maxError threshold isn't
       *    reached, keep training.
       */
      while ((iteration == 0) || ((iteration < maxIter) && (avgErr > maxError)))
      {
         // iterate through each case in the training data
         for (int case_ = 0; case_ < inputs.length; case_++)
         {
            predicted = run_train(inputs[case_]);
            
            omega = outputs[case_] - predicted;

            psiOut = omega * derive_sigmoid(thetaOut);

            for (int act_j = 0; act_j < numActs[1]; act_j++)    // for each activation in hidden layer
            {
               // calculate derivative + change in weights
               deriv2[act_j][0] = -h[act_j] * psiOut;
               gradient2[act_j][0] = -lambda * deriv2[act_j][0];
               
               omegas[act_j] = psiOut * weights2[act_j][0];

               psi[act_j] = omegas[act_j] * derive_sigmoid(theta[act_j]);

               for (int act_k = 0; act_k < numActs[0]; act_k++) // for each activation in input layer
               {
                  // calculate derivative + change in weights
                  deriv1[act_k][act_j] = -a[act_k] * psi[act_j];
                  gradient1[act_k][act_j] = -lambda * deriv1[act_k][act_j];
               }

            } // for (int act_j = 0; act_j < numActs[1]; act_j++)

            // update weights
            for (int k = 0; k < numActs[0]; k++)
            {
               for (int j = 0; j < numActs[1]; j++)
               {
                  weights1[k][j] += gradient1[k][j];
                  weights2[j][0] += gradient2[j][0];
               }
            } // for (int k = 0; k < numActs[0]; k++)

            caseError = (omega * omega) / 2.0;
            totalError[case_] = caseError;

         } // for (int case_ = 0; case_ < inputs.length; case_++)

         iteration++;

         // calculate average error
         avgErr = 0.0;
         for (int train_case = 0; train_case < nCases; train_case++)
         {
            avgErr += totalError[train_case];
         }

         avgErr /= nCases;

      } // while ((iteration == 0) || ((iteration < maxIter) && (avgErr > maxError)))

      if (iteration >= maxIter)
      {
         System.out.println("Reached max iterations allowed for training.");
      } // if (iteration >= maxIter)

      else if (avgErr <= maxError)
      {
         System.out.println(avgErr);
         System.out.println(maxError);
         System.out.println("Reached desired error value.");
      } // else if (avgErr <= maxError)

   } // public void train(double[][] inputs, double[] outputs)

   /*
    * Calculates sigmoid based on input
    * 
    * @param input: the value that we apply the sigmoid function to
    *
    * @return the sigmoid of input
    */
   public double sigmoid(double input)
   {
      return 1.0 / (1.0 + Math.exp(-input));
   }

   /*
    * Calculates the sigmoid derivative based on input
    *
    * @param input: the value that we apply the sigmoid derivative function to
    *
    * @return the sigmoid derivative of the input
    */
   public double derive_sigmoid(double input)
   {
      double sigmoid = sigmoid(input);
      return sigmoid * (1.0 - sigmoid);
   }

   /*
    * Display model performance to user.
    * 
    * @param inputs: training data
    * @param expected_outputs: expected outputs
    * @param outputs: model's calculated predictions
    */
   public void reportTrainingResults(double[][] inputs, double[] expected_outputs, double[] outputs)
   {
      System.out.println("Total number of iterations: " + iteration);
      System.out.println("Average error: " + avgErr);

      // iterate through each case in input data and print out prediction
      for (int t_case = 0; t_case < expected_outputs.length; t_case++)
      {
         System.out.print("Input: ");
         for (int in = 0; in < inputs[t_case].length; in++)
         {
            System.out.print(inputs[t_case][in] + " ");
         }

         System.out.println();
         System.out.println("Expected Output: " + expected_outputs[t_case]);
         System.out.println("Predicted Output: " + run_train(inputs[t_case])); 

      } // for (int trainCases = 0; trainCases < inputs.length; trainCases++)

   } // public void reportTrainingResults(double[][] inputs, double[] expected_outputs, double[] outputs)

   /*
    * Runs model; makes predictions on input data based on current weights. In comparison to run_train,
    *    uses a single variable (runtheta) to store single theta value.
    * 
    * @param input: data for model to make predictions on
    *
    * @return predictions
    */
   public double run(double[] input)
   {
      // set input activations to input data
      a = input;

       // for each node in the hidden layer
      for (int ind = 0; ind < numActs[1]; ind++)
      {
         // calculate theta (accumulation function)
         runtheta = 0.0;

         // iterate through each node in the input layer
         for (int prevInd = 0; prevInd < numActs[0]; prevInd++)
         {
            runtheta += a[prevInd] * weights1[prevInd][ind];
         }

         // update hidden activations
         h[ind] = sigmoid(runtheta);

      } // ; for (int ind = 0; ind < activationsNum[n]; ind++)

      thetaOut = 0.0;

      // for each node in the hidden layer
       for (int ind = 0; ind < numActs[1]; ind++)
       {
         // calculate the out theta
         thetaOut += h[ind] * weights2[ind][0];
       }

      return sigmoid(thetaOut);

   } // public void run(double[] inputs)

   /*
    * Runs model for training; makes predictions on input data based on current weights.
    * 
    * @param input: data for model to make predictions on
    *
    * @return predictions
    */
    public double run_train(double[] input)
    {
       // set input activations to input data
       a = input;
 
        // for each node in the hidden layer
       for (int ind = 0; ind < numActs[1]; ind++)
       {
          // calculate theta (accumulation function)
          theta[ind] = 0.0;
 
          // iterate through each node in the input layer
          for (int prevInd = 0; prevInd < numActs[0]; prevInd++)
          {
             theta[ind] += a[prevInd] * weights1[prevInd][ind];
          }
 
          // update hidden activations
          h[ind] = sigmoid(theta[ind]);
 
       } // ; for (int ind = 0; ind < activationsNum[n]; ind++)
 
       thetaOut = 0.0;
 
       // for each node in the hidden layer
        for (int ind = 0; ind < numActs[1]; ind++)
        {
          // calculate the out theta
          thetaOut += h[ind] * weights2[ind][0];
        }
 
       return sigmoid(thetaOut);
 
    } // public void run(double[] inputs)

   /*
    * Report the results from running the model
    *
    * @param inputs: input data to make predictions on
    * @param expected_outputs: the expected outputs for the model's predictions
    * @param outputs: the model's predictions
    */
   public void reportRunningResults(double[][] inputs, double[] expected_outputs, double[] outputs)
   {
      // iterate through each case
      for (int case_i = 0; case_i < nCases; case_i++)
      {
         System.out.print("Input: ");

         // iterate through each item in a case and print in a line
         for (int in = 0; in < inputs[case_i].length; in++)
         {
            System.out.print(inputs[case_i][in] + " ");
         }
         
         System.out.print("Output: " + outputs[case_i]);
         System.out.println(" Expected Output: " + expected_outputs[case_i]); 
      } // for (int case_i = 0; case_i < nCases; case_i++)

   }    // public void reportRunningResults(double[][] inputs, double[] expected_outputs, double[] outputs)

   /*
    * Given an input, run model and return predictions. Uses different running function than in training mode
    *    in order to save space; theta does not require more storage than a single value while running.
    *
    * @param input: input data to make predictions
    * 
    * @return model predictions
    */
   public double[] runTestCasesRun(double[][] input)
   {
      double output[] = new double[input.length];

      for (int input_ind = 0; input_ind < input.length; input_ind++)
      {   
         output[input_ind] = run(input[input_ind]);
      }

      return output;
   } // public double[] runTestCases(double[][] input)

   /*
    * Given an input, run model and return predictions.
    *
    * @param input: input data to make predictions
    * 
    * @return model predictions
    */
    public double[] runTestCasesTrain(double[][] input)
    {
      double output[] = new double[input.length];

      for (int input_ind = 0; input_ind < input.length; input_ind++)
      {   
         output[input_ind] = run_train(input[input_ind]);
      }

      return output;
   } // public double[] runTestCases(double[][] input)

} // public class AB1_Net
