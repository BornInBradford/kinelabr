# README #

A set of functions for processing raw Kinelab output files in R.

### Install as R package ###

Can now be installed as a R package using the devtools package to load kinelabr directly from bitbucket. User authentication is still required at this stage.

```R

# install devtools if not already installed
install.packages("devtools")

# install kinelabr package
library(devtools)
install_bitbucket("dan-m/kinelabr", 
                  auth_user = "<your_username>", 
                  password = "<your_password>")

# load kinelabr functions
library(kinelabr)

```

### Directory structure ###

You need to provide the path to the Kinelab folder to the functions that take the `kinelab.path` parameter. Within this folder, the following structure is expected. The hierarchy from `BiB Kinelab Output` downwards matches the structure of the default Kinelab output.


```
<kinelab.path>
  |
  \__<Daily directory 1>
  |    |
  |    \__<Tablet directory 1>
  |    |    |
  |    |    \__BiB Kinelab Output
  |    |        |
  |    |        \__BiB data
  |    |        |   |
  |    |        |   \__Database data
  |    |        |   |
  |    |        |   \__Feedback reports
  |    |        |       \_<SchoolName 1>
  |    |        |       \_<SchoolName 2>
  |    |        |       \_...
  |    |        | 
  |    |        \__Processed data
  |    |        |   \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |        |   \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |        |   \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |        |   \__...
  |    |        |
  |    |        \__recorded_data
  |    |        |   \__dd_mm_yyyy
  |    |        |        \__session 001
  |    |        |        \__session 002
  |    |        |        \__session 003
  |    |        |        \__...
  |    |        |
  |    |        \__WM
  |    |            \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |            \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |            \__<Tablet name>_<UPN>_ddmmyyyy_hhmmss
  |    |            \__...
  |    |
  |    \__<Tablet directory 2>
  |    |    |
  |    |    \__BiB Kinelab Output
  |    |
  |    \__<Tablet directory n>
  |         |
  |         \__BiB Kinelab Output
  |           
  \__<Daily directory 2>
  |
  \__<Daily directory n>
```

### Data files ###

The following files are expected:

* `Database data.txt` contains the summary data and is in the `BiB data/Database data` folder. This file is required to find feedback reports on the basis of UPN, as we get child name for each UPN from here.

* `Battery_DVs.csv` contains the CKAT raw scores and is in the one-per-session folders called `Processed data/<Tablet name>_<UPN>_ddmmyyyy_hhmmss`

* `WM scores.csv` contains the working memory (WM) raw scores and is in the one-per-session folders called `WM/<Tablet name>_<UPN>_ddmmyyyy_hhmmss`

* `Subject_Info.csv` contains the session information for the raw data in the `session nnn` directories under the `recorded_data` directory

### HOW TO ###

Please see `scripts/kinelabr_examples.R` for usage examples.

### TO DO ###

* Trailing / in kinelab.path causes all sorts of problems, needs to be dealt with.

* Functions to index and extract other files across the `BiB Kinelab Output` hierarchy.

* Delete functions need to log directly to file and need more informative messages when UPNs not found.

* Not sure what I'm calling raw data and summary data is quite right, some relabelling might be required.