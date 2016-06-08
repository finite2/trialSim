# trialSim

I want this package to be a completely flexible, lightweight trial design package for all design types. To do this the simulations are broken down into their respective parts: simulate data, fit a model, and make decisions based on that model.

## Design breakdown

These are the respective parts in the order to analyse them:

1. Simulate next patient. This includes baseline characteristics and arrival time.
2. Fit model
3. Make decisions based on the model and currently accrued data
4. Simulate patient outcomes.

In some cases 1 and 4 can be performed at the same time.

## Implementation

In order to create a lightweight package we use a hierarchy of S4 classes. Individual models will then get their own class at the bottom of the hierarchy. Because modifications are often needed the package itself will concentrate on implementation of the system and finding solutions to problems that arise for specific simulations rather than the implementation of those simulations. We will also provide some simple general methods for outputs.

One point to note is that for parallel computation all functions and methods (?classes) must be loaded on the parallel CPU cores via `parallelTrial` and a call to `clusterEvalQ`. For this reason it may be sensible to use library or source calls for testing or even implementation within the `trialDesign` object.

## Input classes

### Simulate data (simulateBaseline, simualteOutcome, simualteData)
This will consist of a function taking data and parameters. This will return the data. The function will need to create a data frame if data is NULL in order to make the first pass. Depending on implementation of a specific method there will be one or two parts to simulation

### Fit model (experimentalModel)
This will also consist of a function taking data and parameters. In addition the model will have a number of vectors naming the methods 3-6 which are valid for this model. The function can act as a wrapper for models in many other packages.

### make decisions (makeDecisions)

This will also consist of a function taking data and parameters. This will return an object of parent class `decision` which will tell the other functions what to do next.

## Validation of a given method

It will be highly recommended to create and S4 class of the final design object. This S4 object should inherit from a child of trialDesign. The most important thing is to privide a validation method. Validation of list p and the data if provided will be the priority. Both of these objects could pass in anything if you don't do this and it may be hard to trace errors back to these otherwise.

## R file structure

### 00 files:
Parent classes for most functions.

### 01 files:
Generic method definitions and method definitions for parent classes.

### 02 files:
Utility functions and classes.

### 03 files: 
Child classes of the parent classes above. These are not specific designs but implementations of wide ranging designs.

### 04 files:
Specific designs.