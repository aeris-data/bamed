# BAMED

BAMED - Boundary Layer Pressurized Balloons trajectory simulation

BAMED is a Fortran based numeric tool that allows to simulate Boundary Layer Pressurized Balloons (BLPB) trajectories based on the given meteorological conditions. The model allows simulating balloon path from a given launch site (geographical coordinates) on the given date and time of the launch. The tool needs ECMWF meteorological data (wind, temperature, humidity, precipitation and surface pressure) as input data.

## Requirements
The BAMED tool is containerized into a Singularity container so one must have Singularity installed on the host system intended for simulations.

## Installation
1. `git clone https://github.com/aeris-data/bamed.git`
2. `sudo singularity build ./bamed.sif ./bamed-container.def`
The `singularity build` command will build the container `bamed.sif` from its definition file, using the source files got from the git repo; so for the build it is important to call the command from the git repo directory that one has made. ⚠️ ***The build requires sudo rights.*** Afterwards, the sif image can be placed anywhere (even on another system) independently of the source files. To run the image no sudo rights are required.

## Usage
The main script is bamed.py which needs the input configuration file user-config.xml (which can be renamed, the name is not important). The Python script handles the launch combinations, writes input files for the Fortran executable and post-process simulation results. The main usage is `python3 bamed.py --config user-config.xml [--shell-log]`. The script must be launched inside the Singularity container. The outputs of the simulation are : baloons estimated positions' coordinates in ASCII, netCDF and KML format + PNG map plot of the estimated trajectories. More details about input/output and folder structure are in the manual `SEDOO-AERIS-DT-003-MAG_BAMED_ATBD.pdf`.

## Input meteorological data extraction
The input data for the simulations is meteorological data : wind, temperature, humidity, precipitation and logarithm of surface pressure, coming from the ECMWF database. To extract and prepare the data in the correct format, the script `bamed-extract-ecmwf.sh` should be used. The script must be launched on the ECMWF MARS server (ecs, hpc or other). The data extraction was tested with a member-state user account. Other more public accounts might customize the script based on the MARS services or APIs available for their type of user.

The bash script can either extract the data and prepare it for the simulation, or it can extract the data, transfer it to the one's distant working server and automatically launch simulations on this server when the data extraction is finished. One must of course has the Singularity container and BAMED tool scripts on this working server. The input for this script is the configuration file ecmwf-user-parameters.conf, where the simulation and data parameters are indicated by the user. The main usage is `bamed-extract-ecmwf.sh --config ecmwf-user-parameters.conf`. In a case of a member-state user account, it is strongly advised to launch extractions as a SLURM batch job via `sbatch --wrap="bamed-extract-ecmwf.sh --config ecmwf-user-parameters.conf"` syntax.
