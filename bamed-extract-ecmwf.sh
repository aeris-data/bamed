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

# set -e

SCRIPT_NAME=$(basename "$0")

# +----------------------------------+
# | Aux functions                    |
# +----------------------------------+

function info() {
    msg=$1
    line_number=${BASH_LINENO[0]}
    calling_func=${FUNCNAME[1]}
    echo "$(date +'%d/%m/%Y %H:%M:%S') - [INFO]       (${calling_func}:${line_number}) ${msg}"
}

function warning() {
    msg=$1
    line_number=${BASH_LINENO[0]}
    calling_func=${FUNCNAME[1]}
    echo "$(date +'%d/%m/%Y %H:%M:%S') - [WARNING]    (${calling_func}:${line_number}) ${msg}"
}

function error() {
    msg=$1
    line_number=${BASH_LINENO[0]}
    calling_func=${FUNCNAME[1]}
    echo "$(date +'%d/%m/%Y %H:%M:%S') - [ERROR]      (${calling_func}:${line_number}) ${msg}"
}

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
    if [ -z ${ID_NAME} ]; then error "ID_NAME variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${N_DAYS} ]; then error "N_DAYS variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${GRID_RESOLUTION} ]; then error "GRID_RESOLUTION variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${LAT_MIN} ]; then error "LAT_MIN variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${LAT_MAX} ]; then error "LAT_MAX variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${LON_MIN} ]; then error "LON_MIN variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${LON_MAX} ]; then error "LON_MAX variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${WORKING_DIR} ]; then error "WORKING_DIR variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${DATA_DIR} ]; then error "MARS_DATA_DIR variable is either empty or not set at all"; exit_status=1; fi
    if [ -z ${SERVER_USER} ] || [ -z ${SERVER_ADDRESS} ] || [ -z ${SERVER_DATA_DIR} ]; then warning "Information about destination server for the extracted data is not set or empty, there will be no data transfer, you will find it directly in the ${DATA_DIR} on the MARS server"; fi
    # ====================================================================================================================================================================
    if [[ ! -z ${START_DATE} && $(date -d ${START_DATE} +%s) -gt $(date +%s) ]]; then
        error "START_DATE can not exceed the today date, verify your value"
        exit_status=1
    fi
    # ====================================================================================================================================================================
    if [[ ! -z ${N_DAYS} && ${N_DAYS} -lt 1 ]]; then error "N_DAYS should be positive >= 1"; exit_status=1; fi
    # ====================================================================================================================================================================
    if [[ ! -z ${LAT_MIN} && ! -z ${LAT_MAX} && ! -z ${LON_MIN} && ! -z ${LON_MAX} && ! -z ${GRID_RESOLUTION} ]]; then
        if (( $(bc <<< "${LAT_MIN} < -90") || $(bc <<< "${LAT_MAX} > 90") )); then
            error "Latitude has to be in [-90°; +90°] interval"
            exit_status=1
        fi
        if (( $(bc <<< "${LON_MIN} < -180") )); then
            error "Longitude has to be either in [-180°; +180°] or [0°; +360°] interval"
            exit_status=1
        fi
        if (( $(bc <<< "${LON_MIN} > -180") && $(bc <<< "${LON_MIN} < 0") && $(bc <<< "${LON_MAX} > 180") )); then
            error "Longitude has to be either in [-180°; +180°] or [0°; +360°] interval"
            exit_status=1
        fi
        if (( $(bc <<< "${LON_MIN} > 0") && $(bc <<< "${LON_MAX} > 360") )); then
            error "Longitude has to be either in [-180°; +180°] or [0°; +360°] interval"
            exit_status=1
        fi
        if (( $(bc <<< "${LAT_MIN} >= ${LAT_MAX}") )); then
            error "LAT_MIN has to be less than LAT_MAX"
            exit_status=1
        fi
        if (( $(bc <<< "${LON_MIN} >= ${LON_MAX}") )); then
            error "LON_MIN has to be less than LON_MAX"
            exit_status=1
        fi
        if (( $(bc <<< "scale=2; (${LAT_MAX}-${LAT_MIN})/${GRID_RESOLUTION} < 2") )); then
            error "GRID_RESOLUTION is greater or equal to the coverage of your window"
            exit_status=1
        fi
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
            warning "Your number of days is > 10, but the data can only be \
            extracted up to 10 days from TODAY. Extracting data up to your start date + 10 days."
            N_DAYS=10
        fi
        DATA_TYPE="fc"
        END_DATE=$(date -d "${START_DATE}+${N_DAYS} days" +%Y%m%d)
    # if date is set and is TODAY
    elif [[ ${START_DATE} == $(date +%Y%m%d) ]]; then
        if [[ ${N_DAYS} -gt 10 ]]; then
            warning "Your number of days is > 10, but the data can only be \
                extracted up to 10 days from TODAY. Extracting data up to your start date + 10 days."
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
            warning "Your end date exceeds TODAY+10 days, the data can only be \
                extracted up to 10 days from TODAY. Extracting data from your start date and up to 10 days from TODAY."
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
singularity exec --bind ${SERVER_DATA_DIR} ${SINGULARITY_CONTAINER_REMOTE_PATH} python3 ${PYTHON_REMOTE_PATH} -bc ${SERVER_WORKING_DIR}/$(basename ${XML_FILEPATH})
EOF
    chmod +x ${JOB_FILEPATH}
    echo ${JOB_FILEPATH}
}

function prepare_dates(){
    # --------------------------------------------------------
    # | Check_the_date function determines whether analysis  |
    # | or forecast data should be requested based on the    |
    # | time period chosen by the user. This period is then  |
    # | extended by 1 day in the beginning and in the end    |
    # | in order to allow a more precise inteprolation by    |
    # | the BAMED simulation model.                          |
    # --------------------------------------------------------
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
}

function prepare_dirs(){
    # ----------------------------------------------------------------------
    # | This function prepares working and data directories                |
    # | on the MARS server. The current simulation working                 |
    # | directory will be:                                                 |
    # | root_wdir_from_the_configuration_file/id_name/start_date_end_date  |
    # | The data directory for the current simulation remains the same,    |
    # | as indicated in the configuration file.                            |
    # ----------------------------------------------------------------------
    LAUNCH_WORKING_DIR="${WORKING_DIR}/${ID_NAME}/${COPY_START_DATE}_${COPY_END_DATE}"
    SERVER_WORKING_DIR="${SERVER_WORKING_DIR}/${ID_NAME}/${COPY_START_DATE}_${COPY_END_DATE}"
    if [ ! -d ${LAUNCH_WORKING_DIR} ]; then
        mkdir -p ${LAUNCH_WORKING_DIR}
    fi
    DATA_DIR="${DATA_DIR}/${ID_NAME}"
    if [ ! -d ${DATA_DIR} ]; then
        mkdir -p ${DATA_DIR}
    fi
}

function prepare_bamed_simulation_config(){
    # ---------------------------------------------------------
    # | This function prepares the xml configuration file for |
    # | the current BAMED simulation and defines certain      |
    # | parameters needed for the data extraction and MARS    |
    # | requests.                                             |
    # ---------------------------------------------------------
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
        # TODAY_DATE=$(date +'%Y%m%d')
        # END_AN_DATE=$(date -d "${TODAY_DATE} - 1 day" "+%Y%m%d")
        # AN_N_DAYS=$(( ($(date -d ${TODAY_DATE} +%s) - $(date -d ${DATA_START_DATE} +%s)) / 86400 ))
        # FC_N_DAYS=$(( ($(date -d ${DATA_END_DATE} +%s) - $(date -d ${TODAY_DATE} +%s) + 86400) / 86400 ))
        # FC_HOURS=$((${FC_N_DAYS}*24))
        # SIMU_CONFIG_FILEPATH=$(write_simulaiton_config_file 3)
        TODAY_DATE=$(date +'%Y%m%d')
        END_AN_DATE=$(date -d "${TODAY_DATE} - 2 days" "+%Y%m%d")
        START_FC_DATE=$(date -d "${TODAY_DATE} - 1 day" "+%Y%m%d")
        AN_N_DAYS=$(( ($(date -d ${END_AN_DATE} +%s) - $(date -d ${DATA_START_DATE} +%s)) / 86400 ))
        FC_N_DAYS=$(( ($(date -d ${DATA_END_DATE} +%s) - $(date -d ${START_FC_DATE} +%s) + 86400) / 86400 ))
        FC_HOURS=$((${FC_N_DAYS}*24))
        SIMU_CONFIG_FILEPATH=$(write_simulaiton_config_file 3)
    fi
}

function prepare_remote_job_script(){
    # ----------------------------------------------------------------
    # This function prepares the script that will be launched        |
    # with sbatch on the remote server to run the simulation itself. |
    # ----------------------------------------------------------------
    REMOTE_JOB_FILEPATH=$(write_simu_job_script ${SIMU_CONFIG_FILEPATH})
}

# function check_for_existing_grib_files(){
# }

function extract_ecmwf_data(){
    _tmp_data_dir="${DATA_DIR}/${START_DATE}_${END_DATE}"
    mkdir -p ${_tmp_data_dir}
    export MARS_MULTITARGET_STRICT_FORMAT=1
    module load ecmwf-toolbox
    info "Requesting data on mars"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
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
    date=${START_FC_DATE},
    time=00,
    step=0/to/21/by/3,
    param=130/131/132/133/135,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levelist=1,
    param=152,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
retrieve,
    levtype=SFC,
    param=228,
    target="${_tmp_data_dir}/[date]_[time]_[step].grib"
EOF
    fi

    mars ${REQUEST_FILEPATH}
    mars_status=$?
    # mars_status=0
    if [ ${mars_status} == 0 ]; then
        info "Getting data from mars - DONE"
    else
        error "Error while retrieving data from MARS : exit status ${mars_status}"
        exit ${mars_status}
    fi
}

function rename_grib_files(){
    _files_to_rename=($(find ${_tmp_data_dir} -name "????????_????_*.grib"))
    if [ -z ${_files_to_rename} ]; then
        warning "No files to rename, continuing with the script"
    else
        for RESULT_GRIB_FILE in ${_files_to_rename[@]}; do
            FILENAME=$(basename ${RESULT_GRIB_FILE} ".grib")
            date_part=$(echo "${FILENAME}" | cut -d'_' -f1)
            time_part=$(echo "${FILENAME}" | cut -d'_' -f2)
            fcstep_part=$(echo "${FILENAME}" | cut -d'_' -f3)
            new_name=$(date --date "${date_part} ${time_part} + ${fcstep_part} hours" +"%y%m%d%H")
            mv ${RESULT_GRIB_FILE} ${_tmp_data_dir}/${new_name}.grib
        done
    fi
}

function transfer_files_to_remote_server(){
    if [ -z ${SERVER_USER} ] || [ -z ${SERVER_ADDRESS} ] || [ -z ${SERVER_DATA_DIR} ]; then
        info "No distant server info was configured, thus no file transfer nor simulation were performed, you can find your data in ${DATA_DIR}"
        info "END OF JOB"
        exit 0
    else
        _max_retries=5

        _attempt=1
        cmd="mkdir -p ${SERVER_DATA_DIR}"
        info "Creating remote directory for the extracted data if it does not exist..."
        while [ ${_attempt} -le ${max_retries} ]; do
            ssh -o ServerAliveInterval=10 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${cmd}
            if [ $? -eq 0 ]; then
                info "${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_DATA_DIR} is ready"
                break  # Break out of the loop if SSH command succeeds
            else
                info "SSH connection failed. Retrying..."
                _attempt=$((_attempt + 1))
                sleep 60  # Add a delay before retrying
            fi
        done

        TOTAL_FILES=$(ls ${_tmp_data_dir}/*.grib | wc -l)
        TRANSFERRED_FILES=0
        DST_PATH="${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_DATA_DIR}/"
        info "Copying data to ${DST_PATH}/"
        files_to_copy=($(find ${_tmp_data_dir} -iname "*.grib"))
        for RESULT_GRIB_FILE in ${files_to_copy[@]}; do
            SRC_PATH="${RESULT_GRIB_FILE}"
            info "Transferring $(basename ${SRC_PATH}) ---> ${DST_PATH}"
            for _attempt in {1..10}; do
                scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${SRC_PATH}" "${DST_PATH}"
                status=$?
                if (( ${status} == 0 )); then
                    TRANSFERRED_FILES=$((${TRANSFERRED_FILES}+1))
                    break
                else
                    info "SSH connection failed. Retrying..."
                    sleep 60
                fi
            done
            if (( ${_attempt} == 10 )); then
                error "Problem with transferring $(basename ${SRC_PATH}) file"
            fi
        done
        info "Transferred ${TRANSFERRED_FILES}/${TOTAL_FILES} files"
        NOT_TRANSFERRED_FILES=$((${TOTAL_FILES}-${TRANSFERRED_FILES}))
    fi
    if (( ${NOT_TRANSFERRED_FILES} != 0 )); then
        if [ ${LAUNCH_SIMULATION} == true ]; then
            error "Error with transferring ${NOT_TRANSFERRED_FILES} files, can't proceed with the simulation"
        else
            error "Error with transferring ${NOT_TRANSFERRED_FILES} files. Extracted data remains in the ${_tmp_data_dir} directory"
        fi
        info "END OF JOB"
        exit 1
    else
        info "All GRIB files were succesfully transferred to the remote server"
        if [ ${LAUNCH_SIMULATION} == true ]; then
            info "Proceeding with the BAMED simulation"
        fi
    fi
}

function launch_simulation_remotely(){
    _max_retries=5
    if (( ${NOT_TRANSFERRED_FILES} == 0 )) && [ ${LAUNCH_SIMULATION} == true ]; then 
        DST_PATH="${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_WORKING_DIR}"

        info "Creating remote working directory if it does not exist..."
        _cmd="mkdir -p ${SERVER_WORKING_DIR}"
        _attempt=1
        while [ ${_attempt} -le ${_max_retries} ]; do
            ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${_cmd}
            if [ $? -eq 0 ]; then
                info "${SERVER_USER}@${SERVER_ADDRESS}:${SERVER_WORKING_DIR} is ready"
                break  # Break out of the loop if SSH command succeeds
            else
                info "SSH connection failed. Retrying..."
                _attempt=$((_attempt + 1))
                sleep 60  # Add a delay before retrying
            fi
        done

        info "Copying BAMED configuration file to the remote server..."
        _attempt=1
        while [ ${_attempt} -le ${_max_retries} ]; do
            scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${SIMU_CONFIG_FILEPATH}" "${DST_PATH}/"
            if [ $? -eq 0 ]; then
                info "${DST_PATH}/$(basename ${SIMU_CONFIG_FILEPATH}) is ready"
                break  # Break out of the loop if SSH command succeeds
            else
                info "SSH connection failed. Retrying..."
                _attempt=$((_attempt + 1))
                sleep 60  # Add a delay before retrying
            fi
        done

        info "Copying SLURM job script for simulation to the remote server..."
        _attempt=1
        while [ ${_attempt} -le ${_max_retries} ]; do
            scp -o ServerAliveInterval=30 -o ServerAliveCountMax=5 -q "${REMOTE_JOB_FILEPATH}" "${DST_PATH}/"
            if [ $? -eq 0 ]; then
                info "${DST_PATH}/$(basename ${REMOTE_JOB_FILEPATH}) is ready"
                break  # Break out of the loop if SSH command succeeds
            else
                info "SSH connection failed. Retrying..."
                _attempt=$((_attempt + 1))
                sleep 60  # Add a delay before retrying
            fi
        done

        _attempt=1
        info "Submitting the simulation script ${SERVER_WORKING_DIR}/$(basename ${REMOTE_JOB_FILEPATH}) as a job on ${SERVER_ADDRESS}"
        while [ ${_attempt} -le ${_max_retries} ]; do
            _cmd="sbatch ${SERVER_WORKING_DIR}/$(basename ${REMOTE_JOB_FILEPATH})"
            _jobID=$(ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 "${SERVER_USER}@${SERVER_ADDRESS}" ${_cmd})
            if [ $? -eq 0 ]; then
                info "${_jobID}"
                _jobID="${_jobID//[!0-9]/}"
                break  # Break out of the loop if SSH command succeeds
            else
                info "SSH connection failed. Retrying..."
                _attempt=$((_attempt + 1))
                sleep 60  # Add a delay before retrying
            fi
        done

        while true; do
            _cmd="ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=5 ${SERVER_USER}@${SERVER_ADDRESS} sacct -p -j ${_jobID} --noheader -X --format JobName,State"
            _attempt=1
            while [ ${_attempt} -le ${_max_retries} ]; do
                _sacct_res=$(${_cmd})
                if [ $? -eq 0 ]; then
                    _job_name=$(echo ${_sacct_res} | cut -d'|' -f1)
                    _job_state=$(echo ${_sacct_res} | cut -d'|' -f2)
                    if [[ "${_job_state}" == "COMPLETED" ]]; then
                        info "Remote job has been completed, check the ${REMOTE_WORKING_DIR} on the remote server for simulation results"
                        exit 0
                    elif [[ "${_job_state}" == "RUNNING" ]]; then
                        info "Simulation is still running..."
                        sleep 180
                    elif [[ "${_job_state}" == "FAILED" ]]; then
                        error "Remote job has failed, check the ${REMOTE_WORKING_DIR}/girafe-simulation.out on the remote server for more information"
                        exit 1
                    fi
                    break  # Break out of the loop if SSH command succeeds
                else
                    info "SSH connection failed. Retrying..."
                    attempt=$((attempt + 1))
                    sleep 60  # Add a delay before retrying
                fi
            done
        done
    fi
}

# ====================================================================================================

# +----------------------------------+
# | Main function                    |
# +----------------------------------+

function main(){
    ssh-copy-id -i ~/.ssh/id_rsa.pub ${SERVER_USER}@${SERVER_ADDRESS}
    NOT_TRANSFERRED_FILES=0
    prepare_dates
    prepare_dirs
    prepare_bamed_simulation_config
    prepare_remote_job_script
    extract_ecmwf_data
    rename_grib_files
    transfer_files_to_remote_server
    launch_simulation_remotely
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
