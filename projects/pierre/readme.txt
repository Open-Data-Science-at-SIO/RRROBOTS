This code base is meant to accomplish two things
1) an excercise to use RStudio, test_that, ROxygen, ggplot, git, github.
2) an excercise to replicate the implementation of an ecological model.

The ecological model chosen is FORET-SORTIE put forward in Pacala 93 and expanded on in Pacala 95.

The conceptual structure of the model uses 'submodels', (ie units of code that calculate and perform various aspects of the model). The authors implement this using a procedural paradigm, with units of code operate on a shared data structure that resembles a table with a row per individual being modeled.
