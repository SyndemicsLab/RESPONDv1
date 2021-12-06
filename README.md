# RESPOND Simulation

**R**esearching **E**ffective **S**trategies to **P**revent **O**pioid **D**eath is a Compartmental Monte Carlo simulation used to better understand the impacts of treatment (or lack thereof) of Opioid Use Disorder (OUD) both on a population (i.e. overdose, mortality, recovery) basis and a medical cost basis.

## Necessary and Recommended Software

Strictly, the only required software required to run this simulation is the [R programming language](https://www.r-project.org/). R is freely available and is open-source. [This mirror from Carnegie Mellon University](http://lib.stat.cmu.edu/R/CRAN/) contains a copy of the Comprehensive R Archive Network (CRAN), with individualized links depending on the operating system you use. If you are using Linux, it is recommended that you first check your built-in package manager, as it is likely that a version of R already exists there.

While R is the only required software, it is recommended that users also download [`git`](https://git-scm.com/downloads), for [version control](https://www.atlassian.com/git/tutorials/what-is-version-control), as well as a programming-oriented text editor (e.g. [emacs](https://www.gnu.org/software/emacs/) or [vim](https://www.vim.org)) or an integrated development environment (IDE) such as [Rstudio](https://www.rstudio.com), as changes will need to be made to some files to study the specific situations of interest to you.

## Obtaining the Source Code

The recommended method of downloading the source code is through `git` because it makes the process of updating your local copy of the repository as simple as using the command `git pull` (in most cases). You are, however, able to directly download this code from Bitbucket, but this will make updating much more difficult as changes are made to the model.

### Cloning the Git Repository

Find your way to your terminal if you are on MacOS or Linux or open the `git` executable if you are on Windows. In this environment, you will enter the following command (also able to be copied via the "Clone" button near the top of the repository page).

```
git clone https://bitbucket.org/respond_projects/respond.git
```
After doing this, a prompt will appear requesting you enter your Bitbucket username. Another prompt will subsequently request your password (NOTE: Bitbucket's system now requires you [register for an app password they provide you](https://bitbucket.org/account/settings/app-passwords/) in order to successfully retrieve files from the repository.

### Downloading the Repository as a `.zip`

In the additional menu options denoted by three dots on the repository page, there is a "Download repository" option. Selecting this will either commence the download or bring you to a menu to choose how to proceed, depending on your browser and settings. After extracting all the contents of the downloaded `.zip`, you are at a functionally equivalent state to the above `git` method (though, once again, when changes are made you will have to repeat this entire process.

## Installation and Use

Once you've obtained the source code through one of the above methods, you'll need to configure some things before you can actually run the code.

### Required Packages

While the code "intelligently" handles installing the packages you need in order to run it, the location they get stored on your device may need to be manually specified (it depends on your settings and personal preference), to avoid needing to give the program administrative permissions unnecessarily. Configuration for this differs based on your platform--follow the instructions for the system you'll be using to run the simulations.

#### MacOS and Linux
Open a terminal and navigate to the folder where you have your source code--if you're unfamiliar, this is done with the `cd` (**c**hange **d**irectory) command, e.g.
```
cd $HOME/respond
```
if the `$HOME` (on Linux, this is generally `/home/<username>/`. On MacOS, this is `/Users/<username>/`) directory is where you cloned the code.

Here, you will need to create a `.Rprofile` file. From the terminal, this may be done with the `touch` command, i.e.
```
touch .Rprofile
```

Once this file has been created, the line necessary to define where the necessary packages will be stored is
```
.libPaths("/path/to/store/packages")
```
where, of course, you replace `/path/to/store/packages` with the absolute path to the folder you want to keep them, e.g. `/home/user/rpackages` if you were a Linux user whose username is `user` and the place you wanted to store your packages was a folder called `rpackages`.

#### Windows
This section is a stub.

### Running the Simulation
