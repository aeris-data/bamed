#!/bin/bash

#simu_date="202009$(date +%H)"
#simu_date="20230107"
simu_date="$(date +%Y%m%d)"

ID_NAME="Dénia"
MARS_WDIR="/home/as2/BAMED/cron_test"
MARS_DATA_DIR="/ec/res4/hpcperm/as2/BAMED"
REMOTE_WDIR="/home/resos/BAMED/wdir/cron_test"
REMOTE_DATA_DIR="/sedoo/resos/bamed/mars_data"
REMOTE_PY="/home/resos/git/bamed/bamed.py"
REMOTE_SIF="/home/resos/git/bamed/bamed.sif"

conf_filepath="/home/as2/BAMED/cron_test/${ID_NAME}_${simu_date}.conf"
cat > ${conf_filepath} <<EOF
ID_NAME="${ID_NAME}"
START_DATE="${simu_date}"
N_DAYS=2
GRID_RESOLUTION="0.1"
LAT_MIN="38.078357255727184"
LAT_MAX="44.30062824415589"
LON_MIN="-1.1529551468129284"
LON_MAX="15.843682960823116"
WORKING_DIR="${MARS_WDIR}"
DATA_DIR="${MARS_DATA_DIR}"
SERVER_USER="resos"
SERVER_ADDRESS="nuwa.aero.obs-mip.fr"
SERVER_DATA_DIR="${REMOTE_DATA_DIR}"
LAUNCH_SIMULATION=true
LAUNCH_DATE="${simu_date}"
LAUNCH_TIME="06/09/12/15"
LAUNCH_SITE_NAME="${ID_NAME}"
LAUNCH_LAT="38.8441815"
LAUNCH_LON="0.1152626"
BALOONS_DENSITY="0.9/0.95/1.0"
SIMULATION_DURATION="2"
SIMULATION_TIMESTEP="360"
SERVER_WORKING_DIR="${REMOTE_WDIR}"
SINGULARITY_CONTAINER_REMOTE_PATH="${REMOTE_SIF}"
PYTHON_REMOTE_PATH="${REMOTE_PY}"
EOF

echo "Submitting job --job-name=\"bamed_${ID_NAME}_${simu_date}\" --output=\"${MARS_WDIR}/${ID_NAME}_${simu_date}.out\" --error=\"${MARS_WDIR}/${ID_NAME}_${simu_date}.out\" --wrap=\"/home/as2/BAMED/cron_test/bamed-extract-ecmwf.sh --config ${conf_filepath}\""

sbatch \
    --job-name="bamed_${ID_NAME}_${simu_date}" \
    --output="${MARS_WDIR}/${ID_NAME}_${simu_date}.out" \
    --error="${MARS_WDIR}/${ID_NAME}_${simu_date}.out" \
    --wrap="/home/as2/git/bamed/bamed-extract-ecmwf.sh --config ${conf_filepath}"

# srun /home/as2/BAMED/cron_test/bamed-extract-ecmwf.sh --config ${conf_filepath}
