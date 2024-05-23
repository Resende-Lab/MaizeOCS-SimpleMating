# MaizeOCS-SimpleMating
Scripts from the paper describing SimpleMating package.

### Simulations
The main script is the RUNME.R. It is suppose to call all other scripts. Please, make sure that all scripts are in the same folder than the main one (RUNME.R) or give the path to them on the main file.

For the OCSped scenario, make sure to have BLUPF90 softwere is installed in your local computer or in the server. Be aware that this is the scenario that takes the most memory to run.

If you want to run the simulations in a server, you should use the scripts as they are, otherwise, create a loop in RUNME.R to run all repetitions sequentially. Otherwise, you can take a look into the results, they are at *4.Outputs* folder together with a file A_plot.R. With this file, you can load the data/results and plot the *Figure 6* of the paper.
