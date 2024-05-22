#!/bin/bash

#####################################################################################                                           
###
###    ╱▔▔▔▔▔╲        _                              _                        
###   ▕╋╋╋╋╋╋╋▏      | |                            | |                       
###    ╲╳╳╳╳╳╱       | |__   __ _ _ __ ___   ___  __| |   ___ _ __ ___  _ __  
###     ╲╋╋╋╱        | '_ \ / _` | '_ ` _ \ / _ \/ _` |  / __| '__/ _ \| '_ \ 
###  ~~  ╲▂╱  ~~~~~  | |_) | (_| | | | | | |  __/ (_| | | (__| | | (_) | | | |
### ~~~~~▕▅▏~~~~~~~  |_.__/ \__,_|_| |_| |_|\___|\__,_|  \___|_|  \___/|_| |_|
###  ~~~~~~~~~~~~  
###
### This script allows to configure a cron for regular operational 
### BAMED simulations. The user must set parameters present in section 1.
### Everything else is taken care of by the script.
#####################################################################################

# +---------------------------------------------------------------------------+
# | 1.                                                                        |
# | Set all of the parameters needed for the data extraction and the          |
# | simulation. More details about each parameter can be found in the BAMED   |
# | manual in the tool git repo.                                              |
# +---------------------------------------------------------------------------+

mars_src_dir="/home/as2/git/bamed"

id_name="campaign"

simu_date="$(date +%Y%m%d)"
simu_duration_days=5
launch_time="06/09/12/15"
simulation_timestep="360"
launch_site_name=""
launch_lat="38.8441815"
launch_lon="0.1152626"
baloons_density="0.9/0.95/1.0"
grid_resolution="0.1"
lat_min="38.078357255727184"
lat_max="44.30062824415589"
lon_min="-1.1529551468129284"
lon_max="15.843682960823116"
launch_simulation=true

mars_working_dir="/home/as2/BAMED/cron_test"
mars_data_dir="/ec/res4/hpcperm/as2/BAMED"
remote_server_user="resos"
remote_server_address="nuwa.aero.obs-mip.fr"
remote_server_working_dir="/home/resos/BAMED/wdir/cron_test"
remote_server_data_dir="/sedoo/resos/bamed/mars_data"
remote_server_singularity_container_path="/home/resos/git/bamed/bamed.sif"
remote_server_python_path="/home/resos/git/bamed/bamed.py"

# +---------------------------------------------------------------------------+
# |                         ! ! ! DO NOT CHANGE ! ! !                         |
# +---------------------------------------------------------------------------+

conf_filepath="${mars_working_dir}/${id_name}_${simu_date}.conf"
cat > ${conf_filepath} <<EOF
ID_NAME="${id_name}"
START_DATE="${simu_date}"
N_DAYS=${simu_duration_days}
GRID_RESOLUTION="${grid_resolution}"
LAT_MIN="${lat_min}"
LAT_MAX="${lat_max}"
LON_MIN="${lon_min}"
LON_MAX="${lon_max}"
WORKING_DIR="${mars_working_dir}"
DATA_DIR="${mars_data_dir}"
SERVER_USER="${remote_server_user}"
SERVER_ADDRESS="${remote_server_address}"
SERVER_DATA_DIR="${remote_server_data_dir}"
LAUNCH_SIMULATION=${launch_simulation}
LAUNCH_DATE="${simu_date}"
LAUNCH_TIME="${launch_time}"
LAUNCH_SITE_NAME="${launch_site_name}"
LAUNCH_LAT="${launch_lat}"
LAUNCH_LON="${launch_lon}"
BALOONS_DENSITY="${baloons_density}"
SIMULATION_DURATION=${simu_duration_days}
SIMULATION_TIMESTEP=${simulation_timestep}
SERVER_WORKING_DIR="${remote_server_working_dir}"
SINGULARITY_CONTAINER_REMOTE_PATH="${remote_server_singularity_container_path}"
PYTHON_REMOTE_PATH="${remote_server_python_path}"
EOF

echo "Submitting job :\
    --job-name=\"bamed_${id_name}_${simu_date}\" \
    --output=\"${mars_working_dir}/${id_name}_${simu_date}.out\" \
    --error=\"${mars_working_dir}/${id_name}_${simu_date}.out\" \
    --wrap=\"${mars_src_dir}/bamed-extract-ecmwf.sh \
    --config ${conf_filepath}\""

sbatch \
    --job-name="bamed_${id_name}_${simu_date}" \
    --output="${mars_working_dir}/${id_name}_${simu_date}.out" \
    --error="${mars_working_dir}/${id_name}_${simu_date}.out" \
    --wrap="${mars_src_dir}/bamed-extract-ecmwf.sh --config ${conf_filepath}"
