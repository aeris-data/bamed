#!/bin/bash

########################################
#  SCRIPT FOR              __   _      #
#   THE ECMWF            _(  )_( )_    #
#    FORECAST DATA      (_   _    _)   #
#      EXTRACTION      / /(_) (__)     #
#                     / / / / / /      #
#                    / / / / / /       #
########################################

####### HELP
# ====================================================================================================
# Parameters below are to be defined by the user:
# 
# ID_NAME               "id" name of the cron/job/launch for the user to differentiate 
#                       this data from other data (f.ex. campaign name).
# 
# START_DATE            start date of the campaign in the "YYYYMMDD" format (with double quotes);
#                       replace with "" if you want the script to take the current date (date of
#                       when the script is launched), e.g. in a case of a cron job.
# 
# N_DAYS                number of days for your data J+N_DAYS (forecasts can go up to 10 days from the today)
# 
# GRID_RESOLUTION       spatial resolution of the lat/lon grid of the data (in degrees)
# 
# LAT_MIN               south latitude of the desired area (-90°/+90°)
# LAT_MAX               north latitude of the desired area (-90°/+90°)
# LON_MIN               west longitude of the desired area (-180°/+180° or 0°/+360°)
# LON_MAX               east longitude of the desired area (-180°/+180° or 0°/+360°)
# 
# WORKING_DIR           path to the working directory on the MARS server (for temporary and local files)
# 
# DATA_DIR              path to the directory on the MARS server for GRIB files storage
# 
# SERVER_USER           username of your account on the ssh server where the data will be sent
# 
# SERVER_ADDRESS        ssh server address where the data will be sent
# 
# SERVER_DATA_DIR       path to the directory on the ssh server where the data will be sent (make sure
#                       to verify the existence of this path as there will be no distant directory creation,
#                       and your data could just not be sent and you will have to copy it yourself from
#                       the DATA_DIR of the MARS server)
# 
# LAUNCH_DATE           date of the baloon launch in YYYYMMDD format
# 
# LAUNCH_TIME           time of the baloon launch in HHMM format (must be the time for which there is a grib file);
#                       one can indicate multiple launch times separated by the slash, p.ex. 0800/0900/1000 means
#                       that 3 launch times will be simulated for the given date
# 
# LAUNCH_SITE_NAME      name of the launch site
# 
# LAUNCH_LAT            latitude of the launch site
# 
# LAUNCH_LON            longitude of the launch site
# 
# BALOONS_DENSITY       density of the launches baloons
# 
# SIMULATION_DURATION   duration of the simulation in integer number of days
# 
# SIMULATION_TIMESTEP   timestep of the simulation in seconds (360 seconds maximum)
# 
# SERVER_WORKING_DIR    directory where the simulation will be launched on the remote server
# ====================================================================================================
####### END OF HELP
####### DO NOT CHANGE ANYTHING BELOW

set -e

SCRIPT_NAME=$(basename "$0")

SING_CONTAINER_PATH="/home/user/BAMED/src/bamed.sif"
PYTHON_PATH="/home/user/BAMED/src/bamed.py"

# +----------------------------------+
# | Aux functions                    |
# +----------------------------------+

function help() {
    bold=$(tput bold)
    normal=$(tput sgr0)
    echo "# ########################################"
    echo "# #                          __   _      #"
    echo "# #  SCRIPT FOR            _(  )_( )_    #"
    echo "# #   THE ECMWF           (_   _    _)   #"
    echo "# #    DATA EXTRACTION   / /(_) (__)     #"
    echo "# #                     / / / / / /      #"
    echo "# #                    / / / / / /       #"
    echo "# ########################################"
    echo "#"
    echo "# ${bold}${SCRIPT_NAME}${normal} script extracts and formats the ECMWF Grib data into"
    echo "# files that are accepted by the simulation BAMED tool."
    echo "#"
    echo "# The script knows which meterological data and variables are needed; the user input"
    echo "# must only define the dates, the spatial coverage and additional logistical information."
    echo "# The extraction is performed on the MARS server, and when it's finished the data is sent"
    echo "# to the user defined distant server (which is optional, if the server nor username"
    echo "# were not defined)."
    echo "#"
    echo "# Usage: ${SCRIPT_NAME} [options] arguments"
    echo "# Options:"
    echo "#   ${bold}-h, --help${normal}     Show this help message and exit"
    echo "# Arguments:"
    echo "#   ${bold}--config conf_fielpath${normal}  This argument must correspond to the configuration"
    echo "# file where the user defines input parameters needed for the extraction (like dates,"
    echo "# minimum and maximum latitude/longitude, directory path for the data etc). See an example"
    echo "# of a configuration file below."
    echo "#"
    echo "# +--------------------------------------------------------------------------------+"
    echo "# | Example of the content in the configuration file (all lines are mandatory,     |"
    echo "# | except the server_user, server_address and server_data_dir if you do not want  |"
    echo "# | to send the extracted data to a distant server).                               |"
    echo "# |                                                                                |"
    echo "# | Example filename : ${bold}my_parameters.conf${normal}                                          |"
    echo "# | Example content below :                                                        |"
    echo "# |                                                                                |"
    echo "# | ID_NAME='simulation_n1'                                                        |"
    echo "# | START_DATE='20230801                                                           |"
    echo "# | N_DAYS=5                                                                       |"
    echo "# | GRID_RESOLUTION='0.5'                                                          |"
    echo "# | LAT_MIN='30'                                                                   |"
    echo "# | LAT_MAX='70'                                                                   |"
    echo "# | LON_MIN='-15'                                                                  |"
    echo "# | LON_MAX='42.5'                                                                 |"
    echo "# | WORKING_DIR='/home/my_user/simulation_n1'                                      |"
    echo "# | DATA_DIR='/ec/res4/hpcperm/my_user'                                            |"
    echo "# | SERVER_USER='distant_server_username'                                          |"
    echo "# | SERVER_ADDRESS='my.server.for.data.com'                                        |"
    echo "# | SERVER_DATA_DIR='/my_data_server_root/data/simulation_n1'                      |"
    echo "# | LAUNCH_DATE='20230801'                                                         |"
    echo "# | LAUNCH_TIME='0300/0600/0900'                                                   |"
    echo "# | LAUNCH_SITE_NAME='site-1/site-2'                                               |"
    echo "# | LAUNCH_LAT='lat-of-site-1/lat-of-site-2'                                       |"
    echo "# | LAUNCH_LON='lon-of-site-1/lon-of-site-2'                                       |"
    echo "# | BALOONS_DENSITY='0.90/0.95'                                                    |"
    echo "# | SIMULATION_DURATION='3'                                                        |"
    echo "# | SIMULATION_TIMESTEP='360'                                                      |"
    echo "# | SERVER_WORKING_DIR='/root/bamed/simulation_n1_wdir/'                           |"
    echo "# +--------------------------------------------------------------------------------+"
    echo "#"
    echo "#"
    echo "# ${bold}Input parameters description :${normal}"
    echo "#"
    echo '# ID_NAME               "id" name of the cron/job/launch for the user to differentiate '
    echo '#                       this data from other data (f.ex. campaign name).'
    echo '# '
    echo '# START_DATE            start date of the campaign in the "YYYYMMDD" format (with double quotes);'
    echo '#                       replace with "" if you want the script to take the current date (date of'
    echo '#                       when the script is launched), e.g. in a case of a cron job.'
    echo '# '
    echo '# N_DAYS                number of days for your data J+N_DAYS (forecasts can go up to 10 days from the today)'
    echo '# '
    echo '# GRID_RESOLUTION       spatial resolution of the lat/lon grid of the data (in degrees)'
    echo '# '
    echo '# LAT_MIN               south latitude of the desired area (-90°/+90°)'
    echo '# LAT_MAX               north latitude of the desired area (-90°/+90°)'
    echo '# LON_MIN               west longitude of the desired area (-180°/+180°)'
    echo '# LON_MAX               east longitude of the desired area (-180°/+180°)'
    echo '# '
    echo '# WORKING_DIR           path to the working directory on the MARS server (for temporary and local files)'
    echo '# '
    echo '# DATA_DIR              path to the directory on the MARS server for GRIB files storage'
    echo '# '
    echo '# SERVER_USER           username of your account on the ssh server where the data will be sent'
    echo '# '
    echo '# SERVER_ADDRESS        ssh server address where the data will be sent'
    echo '# '
    echo '# SERVER_DATA_DIR       path to the directory on the ssh server where the data will be sent (make sure'
    echo '#                       to verify the existence of this path as there will be no distant directory creation,'
    echo '#                       and your data could just not be sent and you will have to copy it yourself from'
    echo '#                       the DATA_DIR of the MARS server)'
    echo '# LAUNCH_DATE           date of the baloon launch in YYYYMMDD format'
    echo '# '
    echo '# LAUNCH_TIME           time of the baloon launch in HHMM format (must be the time for which there is a grib file);'
    echo '#                       one can indicate multiple launch times separated by the slash, p.ex. 0800/0900/1000 means'
    echo '#                       that 3 launch times will be simulated for the given date'
    echo '# '
    echo '# LAUNCH_SITE_NAME      name of the launch site (multiple site names must be separated by /)'
    echo '# '
    echo '# LAUNCH_LAT            latitude of the launch site (corresponding to the names, in the same order, if multiple sites, separated by the /)'
    echo '# '
    echo '# LAUNCH_LON            longitude of the launch site (corresponding to the names, in the same order, if multiple sites, separated by the /)'
    echo '# '
    echo '# BALOONS_DENSITY       density of the launched baloons (multiple values must be separated by the /)'
    echo '# '
    echo '# SIMULATION_DURATION   duration of the simulation in integer number of days'
    echo '# '
    echo '# SIMULATION_TIMESTEP   timestep of the simulation in seconds (360 seconds maximum)'
    echo '# '
    echo '# SERVER_WORKING_DIR    directory where the simulation will be launched on the remote server'
}

# ====================================================================================================

function check_args(){
    local exit_status=0
    # ====================================================================================================================================================================
    if [ -z ${ID_NAME} ]; then printf "%s - ERROR : ID_NAME variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${N_DAYS} ]; then printf "%s - ERROR : N_DAYS variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${GRID_RESOLUTION} ]; then printf "%s - ERROR : GRID_RESOLUTION variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${LAT_MIN} ]; then printf "%s - ERROR : LAT_MIN variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${LAT_MAX} ]; then printf "%s - ERROR : LAT_MAX variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${LON_MIN} ]; then printf "%s - ERROR : LON_MIN variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${LON_MAX} ]; then printf "%s - ERROR : LON_MAX variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${WORKING_DIR} ]; then printf "%s - ERROR : WORKING_DIR variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${DATA_DIR} ]; then printf "%s - ERROR : MARS_DATA_DIR variable is either empty or not set at all\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    if [ -z ${SERVER_USER} ] || [ -z ${SERVER_ADDRESS} ] || [ -z ${SERVER_DATA_DIR} ]; then printf "%s - WARNING : information about destination server for the extracted data is not set or empty, there will be no data transfer, you will find it directly in the ${DATA_DIR} on the MARS server\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; fi
    # ====================================================================================================================================================================
    if [[ ! -z ${START_DATE} && $(date -d ${START_DATE} +%s) -gt $(date +%s) ]]; then printf "%s - ERROR : START_DATE can not exceed the today date, verify your \
        value\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    # ====================================================================================================================================================================
    if [[ ! -z ${N_DAYS} && ${N_DAYS} -lt 1 ]]; then printf "%s - ERROR : N_DAYS should be positive >= 1\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    # ====================================================================================================================================================================
    if [[ ! -z ${LAT_MIN} && ! -z ${LAT_MAX} && ! -z ${LON_MIN} && ! -z ${LON_MAX} && ! -z ${GRID_RESOLUTION} ]]; then
        if (( $(bc <<< "${LAT_MIN} < -90") || $(bc <<< "${LAT_MAX} > 90") )); then printf "%s - ERROR : latitude has to be in [-90°; +90°] interval\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "${LON_MIN} < -180") )); then printf "%s - ERROR : longitude has to be either in [-180°; +180°] or [0°; +360°] interval\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "${LON_MIN} > -180") && $(bc <<< "${LON_MIN} < 0") && $(bc <<< "${LON_MAX} > 180") )); then printf "%s - ERROR : longitude has to be either in [-180°; +180°] or [0°; +360°] interval\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "${LON_MIN} > 0") && $(bc <<< "${LON_MAX} > 360") )); then printf "%s - ERROR : longitude has to be either in [-180°; +180°] or [0°; +360°] interval\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "${LAT_MIN} >= ${LAT_MAX}") )); then printf "%s - ERROR : LAT_MIN has to be less than LAT_MAX\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "${LON_MIN} >= ${LON_MAX}") )); then printf "%s - ERROR : LON_MIN has to be less than LON_MAX\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
        if (( $(bc <<< "scale=2; (${LAT_MAX}-${LAT_MIN})/${GRID_RESOLUTION} < 2") )); then printf "%s - ERROR : GRID_RESOLUTION is greater or equal to the coverage of your window\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit_status=1; fi
    fi
    return ${exit_status}
}

# ====================================================================================================

function count_fc_hours(){
    local _N_DAYS=$1
    local _N_HOURS=$((${_N_DAYS}*24))
    local result=""
    if (( ${_N_HOURS} <= 90 )); then result=$(seq -s '/' 0 ${_N_HOURS}); fi
    if (( ${_N_HOURS} > 90 && ${_N_HOURS} <= 144 )); then result=$(seq -s '/' 0 3 ${_N_HOURS}); fi
    if (( ${_N_HOURS} > 144 )); then result=$(seq -s '/' 0 6 ${_N_HOURS}); fi
    echo ${result}
}

# ====================================================================================================

function check_the_date(){
    # the date is not set, TODAY date will be taken
    if [[ ${START_DATE} == "" ]]; then
        START_DATE=$(date +'%Y%m%d')
        if [[ ${N_DAYS} -gt 10 ]]; then
            printf "%s - WARNING : Your number of days is > 10, but the data can only be \
            extracted up to 10 days from TODAY. Extracting data up to your start date + 10 days.\n" \
            "$(date +'%d/%m/%Y - %H:%M:%S')"
            N_DAYS=10
        fi
        DATA_TYPE="fc"
        END_DATE=$(date -d "${START_DATE}+${N_DAYS} days" +%Y%m%d)
    # if date is set and is TODAY
    elif [[ ${START_DATE} == $(date +%Y%m%d) ]]; then
        if [[ ${N_DAYS} -gt 10 ]]; then
            printf "%s - WARNING : Your number of days is > 10, but the data can only be \
                extracted up to 10 days from TODAY. Extracting data up to your start date + 10 days.\n" \
                "$(date +'%d/%m/%Y - %H:%M:%S')"
            N_DAYS=10
        fi
        DATA_TYPE="fc"
        END_DATE=$(date -d "${START_DATE}+${N_DAYS} days" +%Y%m%d)
    # if date is set and is < TODAY (the case > TODAY is handled earlier in arguments check)
    elif [[ $(date -d ${START_DATE} +%s) -lt $(date +%s) ]]; then
        local today_date=$(date +%s)
        local end_date=$(date -d "${START_DATE}+${N_DAYS} days" +%s)
        local end_fc_date=$(date -d "$(date +'%Y%m%d')+10 days" +%s)
        if [[ ${end_date} -lt ${today_date} ]]; then
            DATA_TYPE="an"
            n_days=$((${N_DAYS}-1))
            END_DATE=$(date -d "${START_DATE}+${n_days} days" +%Y%m%d)
        fi
        if [[ ${end_date} -ge ${today_date} && ${end_date} -le ${end_fc_date} ]]; then
            DATA_TYPE="an+fc"
            n_days=$((${N_DAYS}-1))
            END_DATE=$(date -d "${START_DATE}+${n_days} days" +%Y%m%d)
        fi
        if [[ ${end_date} -gt ${end_fc_date} ]]; then
            printf "%s - WARNING : Your end date exceeds TODAY+10 days, the data can only be \
                extracted up to 10 days from TODAY. Extracting data from your start date and up to 10 days from TODAY.\n" \
                "$(date +'%d/%m/%Y - %H:%M:%S')"
            DATA_TYPE="an+fc"
            END_DATE=$(date -d "$(date +'%Y%m%d')+10 days" +%Y%m%d)
        fi
    fi
}

# ====================================================================================================

function write_simulaiton_config_file(){
    GRIB_TIMESTEP=$1
    if [[ ${LAUNCH_DATE} == "" ]]; then
        LAUNCH_DATE=$(date +'%Y%m%d')
    fi

    XML_FILEPATH="${LAUNCH_WORKING_DIR}/bamed-config_${ID_NAME}.xml"
    IFS="/" read -a LAUNCH_SITE_NAMES <<< "${LAUNCH_SITE_NAME}"
    IFS="/" read -a LAUNCH_SITE_LATS <<< "${LAUNCH_LAT}"
    IFS="/" read -a LAUNCH_SITE_LONS <<< "${LAUNCH_LON}"
    N_SITES=${#LAUNCH_SITE_NAMES[@]}

    cat > ${XML_FILEPATH} <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<config>
    <bamed>
        <!-- Current BAMED version (DO NOT CHANGE)-->
        <version>2.0</version>

        <launch name="launch-1">
            <launch_datetime>
                <!-- launch date (YYYYMMDD) -->
                <date>${LAUNCH_DATE}</date>
                <!-- launch time(s) HHMM, separated by a slash in case of multiple times -->
                <time>${LAUNCH_TIME}</time>
            </launch_datetime>
            <launch_site>
                <!-- launch site latitude, longitude in "lat/lon" format for each desired site -->
                <!-- names matter only for the output/input folders distinction, they should be different -->
EOF

    for ((i = 0; i < N_SITES; i++)); do
        cat >> ${XML_FILEPATH} <<EOF
                <site name='${LAUNCH_SITE_NAMES[i]}'> ${LAUNCH_SITE_LATS[i]} / ${LAUNCH_SITE_LONS[i]} </site>
EOF
    done

    cat >> ${XML_FILEPATH} <<EOF
            </launch_site>
            <baloons>
                <!-- baloon densities -->
                <density>${BALOONS_DENSITY}</density>
            </baloons>
        </launch>

        <simulation>
            <!-- Latitude/longitude simulation extent -->
            <lat_min>${LAT_MIN}</lat_min>
            <lat_max>${LAT_MAX}</lat_max>
            <lon_min>${LON_MIN}</lon_min>
            <lon_max>${LON_MAX}</lon_max>
            <!-- Simulation duration (in days) -->
            <duration>${SIMULATION_DURATION}</duration>
            <!-- simulation timestep in seconds (maximum 360 secs) -->
            <time_step>${SIMULATION_TIMESTEP}</time_step>
            <!-- timestep of the grib data in hours -->
            <grib_time_step>${GRIB_TIMESTEP}</grib_time_step>
        </simulation>

        <paths>
            <!-- path to the directory where grib files are stored -->
            <grib_path>${SERVER_DATA_DIR}</grib_path>
            <!-- path to the directory where working files for the simulation will be stored -->
            <working_path>${SERVER_WORKING_DIR}</working_path>
        </paths>
    </bamed>
</config>
EOF
    echo ${XML_FILEPATH}
}

# ====================================================================================================

function write_simu_job_script(){
    XML_FILEPATH=$1
    JOB_FILEPATH="${LAUNCH_WORKING_DIR}/job_${ID_NAME}.sh"
    cat > ${JOB_FILEPATH} <<EOF
#!/bin/bash
# The job name
#SBATCH --job-name=${ID_NAME}
#SBATCH --output=${ID_NAME}.out
#SBATCH --error=${ID_NAME}.out
#SBATCH --chdir=${SERVER_WORKING_DIR}
#SBATCH --time=1:0:0
module load singularity/3.10.2
singularity exec --bind ${SERVER_DATA_DIR} ${SING_CONTAINER_PATH} python3 ${PYTHON_PATH} -bc ${SERVER_WORKING_DIR}/$(basename ${XML_FILEPATH}) --shell-log
EOF
    echo ${JOB_FILEPATH}
}

# ====================================================================================================

# +----------------------------------+
# | Main function                    |
# +----------------------------------+

function main(){

    ssh-copy-id -i ~/.ssh/id_rsa.pub ${SERVER_USER}@${SERVER_ADDRESS}

    check_the_date
    COPY_START_DATE=${START_DATE}
    COPY_END_DATE=${END_DATE}
    COPY_N_DAYS=${N_DAYS}
    START_DATE=$(date -d "$(date -d "${START_DATE}" +%Y%m%d) - 1 day" +%Y%m%d)
    END_DATE=$(date -d "$(date -d "${END_DATE}" +%Y%m%d) + 1 day" +%Y%m%d)
    N_DAYS=$((${N_DAYS}+2))
    check_the_date
    DATA_START_DATE=${START_DATE}
    DATA_END_DATE=${END_DATE}
    DATA_N_DAYS=${N_DAYS}
    local LAUNCH_WORKING_DIR="${WORKING_DIR}/${ID_NAME}/${COPY_START_DATE}_${COPY_END_DATE}"
    if [ ! -d ${LAUNCH_WORKING_DIR} ]; then
        mkdir -p ${LAUNCH_WORKING_DIR}
    fi
    DATA_DIR="${DATA_DIR}/${ID_NAME}/${COPY_START_DATE}_${COPY_END_DATE}"
    if [ ! -d ${DATA_DIR} ]; then
        mkdir -p ${DATA_DIR}
    fi

    ID_NAME="${ID_NAME}"
    GRID_RES="${GRID_RESOLUTION}"
    AREA="${LAT_MAX}/${LON_MIN}/${LAT_MIN}/${LON_MAX}"

    if [[ ${DATA_TYPE} == "an" ]]; then
        SIMU_CONFIG_FILEPATH=$(write_simulaiton_config_file 3)
        FC_HOURS=""
        TODAY_DATE=""
        END_AN_DATE=""
        AN_N_DAYS=""
        FC_N_DAYS=""
    fi
    if [[ ${DATA_TYPE} == "fc" ]]; then
        FC_HOURS=$(count_fc_hours ${DATA_N_DAYS})
        IFS="/" read -ra NUMBERS <<< "${FC_HOURS}"
        FC_DELTA=$(( NUMBERS[1] - NUMBERS[0] ))
        SIMU_CONFIG_FILEPATH=$(write_simulaiton_config_file  ${FC_DELTA})
        TODAY_DATE=""
        END_AN_DATE=""
        AN_N_DAYS=""
        FC_N_DAYS=""
    fi
    if [[ ${DATA_TYPE} == "an+fc" ]]; then
        TODAY_DATE=$(date +'%Y%m%d')
        END_AN_DATE=$(date -d "${TODAY_DATE} - 1 day" "+%Y%m%d")
        AN_N_DAYS=$(( ($(date -d ${TODAY_DATE} +%s) - $(date -d ${DATA_START_DATE} +%s)) / 86400 ))
        FC_N_DAYS=$(( ($(date -d ${DATA_END_DATE} +%s) - $(date -d ${TODAY_DATE} +%s) + 86400) / 86400 ))
        FC_HOURS=$((${FC_N_DAYS}*24))
        SIMU_CONFIG_FILEPATH=$(write_simulaiton_config_file 3)
    fi

    REMOTE_JOB_FILEPATH=$(write_simu_job_script ${SIMU_CONFIG_FILEPATH})

    export MARS_MULTITARGET_STRICT_FORMAT=1
    module load ecmwf-toolbox
    printf "%s - Requesting data on mars\n" "$(date +'%d/%m/%Y - %H:%M:%S')"

    REQUEST_FILEPATH="${LAUNCH_WORKING_DIR}/data.req"
    if [[ ${DATA_TYPE} == "an" ]]; then
        cat > ${REQUEST_FILEPATH} <<EOF
retrieve,
    class=OD,
    expver=1,
    stream=OPER,
    type=AN,
    packing=simple,
    levtype=ML,
    levelist=90/to/137/by/1,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    date=${DATA_START_DATE}/to/${DATA_END_DATE},
    time=00/06/12/18,
    param=130/131/132/133/135,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    class=OD,
    stream=OPER,
    expver=1,
    levtype=ML,
    levelist=90/to/137/by/1,
    type=FC,
    param=130/131/132/133/135,
    date=${DATA_START_DATE}/to/${DATA_END_DATE},
    time=00/12,
    step=3/9,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
EOF
    fi
    if [[ ${DATA_TYPE} == "fc" ]]; then
        cat > ${REQUEST_FILEPATH} <<EOF
retrieve,
    class=OD,
    expver=1,
    stream=OPER,
    type=FC,
    levtype=ML,
    levelist=90/to/137/by/1,
    packing=simple,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    date=${DATA_START_DATE},
    time=00,
    step=${FC_HOURS},
    param=130/131/132/133/135,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
EOF
    fi
    if [[ ${DATA_TYPE} == "an+fc" ]]; then
        cat > ${REQUEST_FILEPATH} <<EOF
retrieve,
    class=OD,
    expver=1,
    stream=OPER,
    type=AN,
    levtype=ML,
    levelist=90/to/137/by/1,
    packing=simple,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    date=${DATA_START_DATE}/to/${END_AN_DATE},
    time=00/06/12/18,
    param=130/131/132/133/135,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    class=OD,
    stream=OPER,
    expver=1,
    levtype=ML,
    levelist=90/to/137/by/1,
    type=FC,
    packing=simple,
    param=130/131/132/133/135,
    date=${DATA_START_DATE}/to/${END_AN_DATE},
    time=00/12,
    step=3/9,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    class=OD,
    expver=1,
    stream=OPER,
    type=FC,
    levtype=ML,
    packing=simple,
    levelist=90/to/137/by/1,
    grid=${GRID_RES}/${GRID_RES},
    area=${AREA},
    date=${TODAY_DATE},
    time=00,
    step=0/to/${FC_HOURS}/by/3,
    param=130/131/132/133/135,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${DATA_DIR}/[date]_[time]_[step].grib"
EOF
    fi

    mars ${REQUEST_FILEPATH}
    mars_status=$?
    if [ ${mars_status} == 0 ]; then
        printf "%s - Getting data from mars - DONE\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
    else
        printf "%s - Error while retrieving data from MARS : exit status ${mars_status}\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
        exit ${mars_status}
    fi

    for RESULT_GRIB_FILE in ${DATA_DIR}/????????_????_*.grib; do
        FILENAME=$(basename ${RESULT_GRIB_FILE} ".grib")
        date_part=$(echo "${FILENAME}" | cut -d'_' -f1)
        time_part=$(echo "${FILENAME}" | cut -d'_' -f2)
        fcstep_part=$(echo "${FILENAME}" | cut -d'_' -f3)
        new_name=$(date --date "${date_part} ${time_part} + ${fcstep_part} hours" +"%y%m%d%H")
        mv ${RESULT_GRIB_FILE} ${DATA_DIR}/${new_name}.grib
    done

    if [ -z ${SERVER_USER} ] || [ -z ${SERVER_ADDRESS} ] || [ -z ${SERVER_DATA_DIR} ]; then
        printf "%s - No distant server info was configured, thus no file transfer nor simulation were performed, you can find your data in ${DATA_DIR}\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
        printf "%s - END OF JOB\n" "$(date +'%d/%m/%Y - %H:%M:%S')" 
        exit 0
    else
        cmd="mkdir -p ${SERVER_DATA_DIR}"
        ssh -o ServerAliveInterval=10 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${cmd}
        TOTAL_FILES=$(ls ${DATA_DIR}/*.grib | wc -l)
        TRANSFERRED_FILES=0
        printf "%s - Copying data to %s\n" "$(date +'%d/%m/%Y - %H:%M:%S')" "${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_DATA_DIR}/"
        for RESULT_GRIB_FILE in ${DATA_DIR}/*.grib; do
            SRC_PATH="${RESULT_GRIB_FILE}"
            DST_PATH="${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_DATA_DIR}/"
            printf "%s - Transferring %s ---> %s\n" "$(date +'%d/%m/%Y - %H:%M:%S')" "$(basename ${SRC_PATH})" "${DST_PATH}"
            for try_ind in {1..10}; do
                scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${SRC_PATH}" "${DST_PATH}"
                status=$?
                if (( ${status} == 0 )); then
                    TRANSFERRED_FILES=$((${TRANSFERRED_FILES}+1))
                    break
                fi
            done
            if (( ${try_ind} == 10 )); then
                printf "%s - ERROR with transferring %s\n" "$(date +'%d/%m/%Y - %H:%M:%S')" "$(basename ${SRC_PATH})"
            fi
        done
        printf "%s - Transferred %s/%s files\n" "$(date +'%d/%m/%Y - %H:%M:%S')" "${TRANSFERRED_FILES}" "${TOTAL_FILES}"
        NOT_TRANSFERRED_FILES=$((${TOTAL_FILES}-${TRANSFERRED_FILES}))
        if (( ${NOT_TRANSFERRED_FILES} != 0 )); then 
            printf "%s - Error with transferring %s files\n" "$(date +'%d/%m/%Y - %H:%M:%S')" "$((${TOTAL_FILES}-${TRANSFERRED_FILES}))"
            if [ ${LAUNCH_SIMULATION} == true ]; then
                printf "%s - Simulation was not launched due to a possible lack of the non-transferred ECMWF data" "$(date +'%d/%m/%Y - %H:%M:%S')"
            fi
            printf "%s - END OF JOB\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
            exit 1
        else
            echo "$(date +'%d/%m/%Y - %H:%M:%S') - Data extraction done, data copied to the ${DST_PATH}"
            DST_PATH="${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_WORKING_DIR}"
            if [ ${LAUNCH_SIMULATION} == true ]; then
                cmd="mkdir -p ${SERVER_WORKING_DIR}"
                ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${cmd}
                scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${SIMU_CONFIG_FILEPATH}" "${DST_PATH}/"
                scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${REMOTE_JOB_FILEPATH}" "${DST_PATH}/"
                cmd="chmod +x ${SERVER_WORKING_DIR}/$(basename ${REMOTE_JOB_FILEPATH})"
                ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${cmd}
                echo "$(date +'%d/%m/%Y - %H:%M:%S') - Submitting the simulation script ${SERVER_WORKING_DIR}/$(basename ${REMOTE_JOB_FILEPATH}) as a job on ${SERVER_ADDRESS}"
                cmd="sbatch ${SERVER_WORKING_DIR}/$(basename ${REMOTE_JOB_FILEPATH})"
                jobID=$(ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${cmd})
                echo "$(date +'%d/%m/%Y - %H:%M:%S') - ${jobID}"
                jobID="${jobID//[!0-9]/}"
                while true; do
                    cmd="ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 ${SERVER_USER}@${SERVER_ADDRESS} squeue -u ${SERVER_USER} -j ${jobID} 2>/dev/null"
                    if ${cmd}; then
                        echo "$(date +'%d/%m/%Y - %H:%M:%S') - Simulation is running..."
                        sleep 15  # Adjust the sleep interval as needed
                    else
                        echo "$(date +'%d/%m/%Y - %H:%M:%S') - Remote job has finished, check the ${SERVER_WORKING_DIR} on the remote server for the log output to see if the simulation was successful"
                        printf "%s - END OF JOB\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
                        exit 0
                    fi
                done
            fi
        fi
    fi
}

# +----------------------------------+
# | BASH SCRIPT                      |
# +----------------------------------+

opts=$(getopt --longoptions "config:,help" --name "$(basename "$0")" --options "h" -- "$@")
eval set --$opts

while [[ $# -gt 0 ]]; do
	case "$1" in
		--config) shift; CONFIG_FILE=$1; shift;;
        -h|--help) help; exit 0; shift;;
		\?) shift; printf "%s - ERROR: unrecognized options\n" "$(date +'%d/%m/%Y - %H:%M:%S')"; exit 1; shift;;
		--) break;;
	esac
done

if [[ -z ${CONFIG_FILE} ]]; then
    printf "%s - ERROR: no configuration file was passed. Exiting script...\n" "$(date +'%d/%m/%Y - %H:%M:%S')";
    exit 1
fi

source ${CONFIG_FILE}
check_args
ARGS_STATUS=$?
if [[ ${ARGS_STATUS} == 1 ]]; then
    printf "%s - WARNING : Errors in input parameters detected, job was not launched\n" "$(date +'%d/%m/%Y - %H:%M:%S')"
    exit 1
else
    main
fi
