# RESPOND Simulation

**R**esearching **E**ffective **S**trategies to **P**revent **O**pioid **D**eath is a Compartmental Monte Carlo simulation used to better understand the impacts of treatment (or lack thereof) of Opioid Use Disorder (OUD) both on a population (i.e. overdose, mortality, recovery) basis and a medical cost basis.

## Necessary and Recommended Software

Strictly, the only required software required to run this simulation is the [R programming language](https://www.r-project.org/). R is freely available and is open-source. [This mirror from Carnegie Mellon University](http://lib.stat.cmu.edu/R/CRAN/) contains a copy of the Comprehensive R Archive Network (CRAN), with individualized links depending on the operating system you use. If you are using Linux, it is recommended that you first check your built-in package manager, as it is likely that a version of R already exists there.

While R is the only required software, it is recommended that users also download [`git`](https://git-scm.com/downloads), for [version control](https://www.atlassian.com/git/tutorials/what-is-version-control), as well as a programming-oriented text editor (e.g. [emacs](https://www.gnu.org/software/emacs/) or [vim](https://www.vim.org)) or an integrated development environment (IDE) such as [RStudio](https://www.rstudio.com), as changes will need to be made to some files to study the specific situations of interest to you.

## Obtaining the Source Code

The recommended method of downloading the source code is through `git` because it makes the process of updating your local copy of the repository as simple as using the command `git pull` (in most cases). You are, however, able to directly download this code from Bitbucket, but this will make updating much more difficult as changes are made to the model.

### Cloning the Git Repository

Find your way to your terminal if you are on MacOS or Linux or open the `git` executable if you are on Windows. In this environment, you will enter the following command (also able to be copied via the "Clone" button near the top of the repository page).

```sh
git clone https://bitbucket.org/respond_projects/respond.git
```
After doing this, a prompt will appear requesting you enter your Bitbucket username. Another prompt will subsequently request your password (NOTE: Bitbucket's system now requires you [register for an app password they provide you](https://bitbucket.org/account/settings/app-passwords/) in order to successfully retrieve files from the repository.

### Downloading the Repository as a `.zip`

In the additional menu options denoted by three dots on the repository page, there is a "Download repository" option. Selecting this will either commence the download or bring you to a menu to choose how to proceed, depending on your browser and settings. After extracting all the contents of the downloaded `.zip`, you are at a functionally equivalent state to the above `git` method (though, once again, when changes are made you will have to repeat this entire process.

## Installation and Use

Once you've obtained the source code through one of the above methods, you'll need to configure some things before you can actually run the code.

### Required R Packages

While the code "intelligently" handles installing the packages you need in order to run it, the location they get stored on your device may need to be manually specified (it depends on your settings and personal preference), to avoid needing to give the program administrative permissions unnecessarily. Configuration for this differs based on your platform--follow the instructions for the system you'll be using to run your simulations.

#### MacOS and Linux
Open a terminal and navigate to the folder where you have your source code--if you're unfamiliar, this is done with the `cd` (**c**hange **d**irectory) command, e.g.
```sh
cd $HOME/respond
```
if the `$HOME` (on Linux, this is generally `/home/<username>/`. On MacOS, this is `/Users/<username>/`) directory is where you cloned the code.

Here, you will need to create a `.Rprofile` file. From the terminal, this may be done with the `touch` command, i.e.
```sh
touch .Rprofile
```

Once this file has been created, the line necessary to define where the necessary packages will be stored is
```R
.libPaths("/path/to/store/packages")
```
where, of course, you replace `/path/to/store/packages` with the absolute path to the folder you want to keep them, e.g. `/home/user/rpackages` if you were a Linux user whose username is `user` and the place you wanted to store your packages was a folder called `rpackages`. If you haven't yet decided on a text editor, this may be done purely from the terminal using the following command:
```sh
echo '.libPaths("/path/to/store/packages")' >> .Rprofile
```

#### Windows
On Windows, **RStudio**, mentioned [above](#necessary-and-recommended-software), installs packages without any additional configuration.

If not using RStudio, configuration can be done in almost the same way as on MacOS and Linux--a variable named `.libPaths` must be set in your `.Rprofile` file in the working directory (a.k.a. the project directory, wherever you have the repository stored). If no `.Rprofile` file exists in this folder, create it and add the line
```R
.libPaths("/path/to/store/packages")
```
via whatever text editor you're using.

### Running the Simulation
**Note:** This section is tentative to change, as there is intent to lower the level of complexity of running the simulation.

*Quoted from Golnaz' `run_instructions.docx`:*

* Create a folder called `inputs` in the root directory of RESPOND, copying `user_inputs.R` from `input1` into `inputs` and update the parameters in the new copy of `user_inputs.R` based on your needs.
* Run `src/generate_shell_tables.R` to create the `.csv` files you'll need.
* If there were no errors, there will now be tables in the `inputs` folder. If there were errors, there will be details in the `generate_deterministic_shell_tables_errors.txt` in the top-level RESPOND folder. Add values as is appropriate for your study in these new tables.
* Actually running the simulation is done from the top-level folder of the project using the command
```sh
Rscript src/respond_main.R X Y
```
where `X` is the number on the input folder to be used and `Y` is the number of the current run, used in the case where you're running several iterations of the simulation.
