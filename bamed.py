# #####################################################################
#                                                                     #
#                    BAMED Python User interface                      #
#                 =================================                   #
#                                                                     #
#   This Python interface handles user configuration for a BAMED      #
#   simulation : from the reading of the configuration file, to the   #
#   writing of the input files for the tool's executable.             #
#                                                                     #
#   Usage : python bamed.py -bc user-config-file.xml [--shell-log]    #
#                                                                     #
#   Created by Daria MALIK (Magellium, 2023)                          #
# #####################################################################

import os
import logging
import datetime
import xml.etree.ElementTree as ET
import glob
import subprocess
import numpy as np
import shutil
import matplotlib.pyplot as plt
import cartopy.crs as crs
import cartopy.feature as cf
import pygrib
import sys
import netCDF4 as nc

BAMED_EXE     = "/usr/local/bamed/src/traj_blpb"
BALOON_COLORS = ["#332288", "#AA4499", "#88CCEE", "#AA4499", "#44AA99", "#882255""#117733", "#CC6677", "#999933", "#DDCC77", "#AA4499"]

# ###############################################################################
# #    BAMED simulation object with information from xml confugration file      #
# ###############################################################################

class bamed_simulation:
    """
    bamed_simulation class allows to hold all the configuration information
    from the xml configuration file
    """
    # ------------------------------------------
    # Launch date and time configuration
    launch_start_datetime = {}
    launch_coordinates    = {}
    # ------------------------------------------
    # Baloons configuration
    density        = []
    # ------------------------------------------
    # Simulation configuration
    lat_min             = -1.0
    lat_max             = 1.0
    lon_min             = -1.0
    lon_max             = 1.0
    simulation_duration = 1
    simulation_timestep = 360
    # ------------------------------------------
    # Input ECMWF data configuration
    grib_grid_res   = 1.0
    grib_time_step  = 1
    level_min       = 90
    level_max       = 137
    # ------------------------------------------
    # Input ECMWF data configuration
    grib_dir = "/root"
    work_dir = "/root"

    def __init__(self, config_xml_filepath) -> int:
        """
        Creates and inits the object with information
        from the configuration file

        Args:
            config_xml_filepath (str): full filepath to the xml confuguration file
        """
        xml = ET.parse(config_xml_filepath)
        xml_root = xml.getroot()
        # ------------------------------------------
        # Launch date and time configuration
        for launch_i in xml_root.findall("bamed/launch/launch_datetime"):
            dates = launch_i.find("date").text.split("/")
            time = launch_i.find("time").text.split("/")
            for date in dates:
                self.launch_start_datetime[date] = time
        sites = xml_root.findall("bamed/launch/launch_site/site")
        self.launch_coordinates = [(site.attrib['name'], float(site.text.split("/")[0].strip()), float(site.text.split("/")[1].strip())) for site in sites]
        # ------------------------------------------
        # Baloons configuration
        self.density = [float(elem) for elem in xml_root.find("bamed/launch/baloons/density").text.split("/")]
        # ------------------------------------------
        # Simulation configuration
        self.lat_min             = float(xml_root.find("bamed/simulation/lat_min").text)
        self.lat_max             = float(xml_root.find("bamed/simulation/lat_max").text)
        self.lon_min             = float(xml_root.find("bamed/simulation/lon_min").text)
        self.lon_max             = float(xml_root.find("bamed/simulation/lon_max").text)
        self.simulation_timestep = round(float(xml_root.find("bamed/simulation/time_step").text))
        self.simulation_duration = round(float(xml_root.find("bamed/simulation/duration").text))
        self.grib_time_step      = int(xml_root.find("bamed/simulation/grib_time_step").text)
        # ------------------------------------------
        # Input ECMWF data configuration
        self.grib_dir = xml_root.find("bamed/paths/grib_path").text
        self.work_dir = xml_root.find("bamed/paths/working_path").text

    def update_grib_fields(self):
        """
        Updates grib related fields in the object based on the grib files
        in the indicated grib directory
        """
        grib_filepath = glob.glob(f"{self.grib_dir}/*.grib")[0]
        ds = pygrib.open(grib_filepath)
        # ------------------------------------------
        var = ds.select(name="Specific humidity")
        levels = [elem.level for elem in var]
        lats, _ = var[0].latlons()
        # ------------------------------------------
        self.grib_grid_res   = round(abs(lats[0,0]-lats[1,0]),3)
        self.level_min       = min(levels)
        self.level_max       = max(levels)
        ds.close()

    def print(self):
        """
        Prints object's information
        """
        print(f"launch_start_datetime  --->  {self.launch_start_datetime}")
        print(f"launch_coordinates     --->  {self.launch_coordinates   }")
        print(f"density                --->  {self.density              }")
        print(f"lat_min                --->  {self.lat_min              }")
        print(f"lat_max                --->  {self.lat_max              }")
        print(f"lon_min                --->  {self.lon_min              }")
        print(f"lon_max                --->  {self.lon_max              }")
        print(f"simulation_duration    --->  {self.simulation_duration  }")
        print(f"simulation_timestep    --->  {self.simulation_timestep  }")
        print(f"grib_grid_res          --->  {self.grib_grid_res        }")
        print(f"level_min              --->  {self.level_min            }")
        print(f"level_max              --->  {self.level_max            }")
        print(f"grib_path              --->  {self.grib_dir             }")
        print(f"working_path           --->  {self.work_dir             }")
    
    def verify_workdir(self):
        """
        Verifies the existence and the path of the working directory
        """
        self.work_dir = os.path.abspath(self.work_dir)
        if not os.path.exists(self.work_dir):
            os.mkdir(self.work_dir)
    
    def verify(self):
        """
        Verifies information in the configuration file (date format, coordinates etc) 
        """
        exit_status = 0
        for date in self.launch_start_datetime.keys():
            for time in self.launch_start_datetime[date]:
                try:
                    dateObject = datetime.datetime.strptime(date+"-"+time, "%Y%m%d-%H%M")
                except:
                    LOGGER.error("Launch date and/or time are in incorrect format, please check your configuration file.")
                    exit_status = 1
                    break
            else:
                continue
            break
        site_names = [site[0] for site in self.launch_coordinates]
        if len(list(set(site_names)))!=len(site_names):
            LOGGER.error("Some of your launch sites have the same name, please check your configuration file.")
            exit_status = 1
        site_lats = np.array([site[1] for site in self.launch_coordinates])
        site_lons = np.array([site[2] for site in self.launch_coordinates])
        # ---------------------------------------------------------------------------
        # Check of the latitude
        # ---------------------------------------------------------------------------
        if np.any(site_lats<-90.0) or np.any(site_lats>90.0):
           LOGGER.error("Launch latitude is outside of the valid domain [-90;+90], please check your configuration file.")
           exit_status = 1
        if self.lat_min<-90.0 or self.lat_max>90.0:
           LOGGER.error("Latitude of the simulation domain is out of valid range [-90;+90], please check your configuration file.")
           exit_status = 1
        if self.lat_min>=self.lat_max:
           LOGGER.error("Minimum latitude for your simulation domain should be less than the maximum latitude, please check your configuration file.")
           exit_status = 1
        if np.any(site_lats<self.lat_min) or np.any(site_lats>self.lat_max):
            LOGGER.error("Your launch latitude coordinates are outside of the simulation domain, please check your configuration file.")
            exit_status = 1
        # ---------------------------------------------------------------------------
        # Check of the longitude
        # ---------------------------------------------------------------------------
        if self.lon_min>=self.lon_max:
           LOGGER.error("Minimum longitude for your simulation domain should be less than the maximum longitude, please check your configuration file.")
           exit_status = 1
        # [-180;+180] convention
        if self.lon_min>=-180.0 and self.lon_min<180.0 and self.lon_max>-180.0 and self.lon_max<=180.0:
            if np.any(site_lons<-180.0) or np.any(site_lons>180.0):
                LOGGER.error("Launch longitude is outside of the valid domain [-180;+180], please check your configuration file.")
                exit_status = 1
        # [0;+360] convention
        if (self.lon_min>=180.0 or self.lon_max>180.0) and self.lon_max<=360.0:
            if np.any(site_lons<0.0) or np.any(site_lons>360.0):
                LOGGER.error("Launch longitude is outside of the valid domain [0;+360], please check your configuration file.")
                exit_status = 1
        # [-180;+360] convention which should give ERROR
        if self.lon_min<0.0 and (self.lon_max>180.0 and self.lon_max<=360.0):
            LOGGER.error("Your minimum and maximum longitude values does not respect the [-180;+180], nor [0;+360] convention, please check your configuration file.")
            exit_status = 1
        if np.any(site_lons<self.lon_min) or np.any(site_lons>self.lon_max):
            LOGGER.error("Your launch longtiude coordinates are outside of the simulation domain, please check your configuration file.")
            exit_status = 1
        # ---------------------------------------------------------------------------
        if self.simulation_duration<1:
            LOGGER.error("Simulation duration should be minimum 1 day, please check your configuration file.")
            exit_status = 1
        if self.simulation_timestep>360:
            LOGGER.error("Simulation timestep should be maximum 360 secs, please check your configuration file.")
            exit_status = 1
        if not os.path.exists(self.grib_dir):
            LOGGER.error(f"Indicated grib path ({self.grib_dir}) does not exist, please check your configuration file.")
            exit_status = 1
        return exit_status
    
    def plot_results(self):
        """
        Plot results once the simulation is done.
        """
        for date in self.launch_start_datetime.keys():
            for time in self.launch_start_datetime[date]:
                for site_coords in self.launch_coordinates:
                    LOGGER.info(f"Plot date {date}, time {time}, site {site_coords}")
                    out_dir = f"{self.work_dir}/{site_coords[0]}"
                    res_files = glob.glob(f"{out_dir}/*{date}_{time}*.dat")
                    # ------------------------------------------
                    # Read data
                    if len(res_files)>0:
                        data = {}
                        for ii,filepath in enumerate(res_files):
                            id = ii
                            data[f"{id}"] = {}
                            with open(filepath,"r") as file:
                                datalines = file.readlines()
                            data[f"{id}"]["launch_date"] = os.path.basename(filepath).split("_")[1]
                            data[f"{id}"]["launch_time"] = os.path.basename(filepath).split("_")[2]
                            data[f"{id}"]["launch_datetime"] = data[f"{id}"]["launch_date"]+"_"+data[f"{id}"]["launch_time"]
                            data[f"{id}"]["density"] = float(datalines[4].split(";")[-1].strip())
                            data[f"{id}"]["time"]    = [float(line.split(";")[1]) for line in datalines[7:-1]]
                            data[f"{id}"]["lon"]     = [float(line.split(";")[2]) for line in datalines[7:-1]]
                            data[f"{id}"]["lat"]     = [float(line.split(";")[3]) for line in datalines[7:-1]]
                            data[f"{id}"]["alt"]     = [float(line.split(";")[4]) for line in datalines[7:-1]]
                        # ------------------------------------------
                        # Plot data
                        fig = plt.figure()
                        if self.lon_min>=180.0 or self.lon_max>=180.0:
                            ax = fig.add_axes(plt.axes(projection=crs.PlateCarree(central_longitude=180)))
                            ax.set_xlim([self.lon_min-180, self.lon_max-180])
                            ax.set_ylim([self.lat_min, self.lat_max])
                            flag_lon = 1
                        else:
                            ax = fig.add_axes(plt.axes(projection=crs.PlateCarree(central_longitude=0)))
                            ax.set_xlim([self.lon_min, self.lon_max])
                            ax.set_ylim([self.lat_min, self.lat_max])
                            flag_lon = -1
                        title_date = datetime.datetime.strftime(datetime.datetime.strptime(date, "%Y%m%d"),"%d/%m/%Y")
                        title_time = datetime.datetime.strftime(datetime.datetime.strptime(time, "%H%M%S"),"%H:%M")
                        ax.set_title(f"Launch on {title_date} at {title_time}\nSite {site_coords[0]} ({site_coords[2]}°, {site_coords[1]}°)",fontsize=15)
                        for ii,key in enumerate(data.keys()):
                            if flag_lon==1:
                                ax.plot(np.array(data[key]["lon"])-180,
                                        data[key]["lat"],
                                        linestyle="--",
                                        marker=".",
                                        # color=colors(ii),
                                        color=BALOON_COLORS[ii%len(BALOON_COLORS)],
                                        linewidth=1.0,
                                        markersize=3,
                                        markevery=15,
                                        label=f"d={data[key]['density']}")
                                ax.plot(site_coords[2]-180, site_coords[1], "rx")
                            else:
                                ax.plot(data[key]["lon"],
                                        data[key]["lat"],
                                        linestyle="--",
                                        marker=".",
                                        # color=colors(ii),
                                        color=BALOON_COLORS[ii%len(BALOON_COLORS)],
                                        linewidth=1.0,
                                        markersize=3,
                                        markevery=15,
                                        label=f"d={data[key]['density']}")
                                ax.plot(site_coords[2], site_coords[1], "rx")
                        ax.add_feature(cf.COASTLINE, linewidth=0.3)
                        ax.add_feature(cf.BORDERS, linewidth=0.3, linestyle=":")
                        ax.add_feature(cf.LAND, alpha=0.2)
                        ax.add_feature(cf.OCEAN, alpha=0.2)
                        grd = ax.gridlines(draw_labels=["x","y","bottom","left"], linestyle="--", ylabel_style={"fontsize":12}, xlabel_style={"fontsize":12})
                        ax.legend()
                        LOGGER.info("Saving fig")
                        fig.savefig(f"{out_dir}/{date}_{time}.png", dpi=300, bbox_inches="tight")
                    else:
                        LOGGER.warning("No simulation results were found for this launch datetime!")

    def write_output_in_kml(self):
        """
        Transcript the simulation ASCII output to the KML format.
        The user have then the output in the ASCII and the KML format.
        """
        out_files = glob.glob(f"{self.work_dir}/**/blpb*.dat", recursive=True)
        for dat_file in out_files:
            with open(dat_file,"r") as file:
                data = file.readlines()
            time_secs = [float(line.split(";")[1]) for line in data[7:-1]]
            lon       = [float(line.split(";")[2]) for line in data[7:-1]]
            lat       = [float(line.split(";")[3]) for line in data[7:-1]]
            alt       = [float(line.split(";")[4]) for line in data[7:-1]]
            start_datetime = datetime.datetime.strptime("T".join(os.path.basename(dat_file).split("_")[1:3]),"%Y%m%dT%H%M%S")
            with open(f"{dat_file[:-4]}.kml","w") as file:
                file.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                file.write("<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n")
                file.write("<Document>\n")
                for ii in range(len(lon)):
                    position_datetime_str = datetime.datetime.strftime(start_datetime + datetime.timedelta(seconds=time_secs[ii]), "%d-%m-%YT%H:%M:%S") 
                    file.write("\t<Placemark>\n")
                    file.write(f"\t\t<name>{position_datetime_str}</name>\n")
                    file.write("\t\t<Point>\n")
                    file.write("\t\t\t<extrude>1</extrude>\n")
                    file.write("\t\t\t<altitudeMode>absolute</altitudeMode>\n")
                    file.write(f"\t\t\t<coordinates>{lon[ii]},{lat[ii]},{alt[ii]}</coordinates>\n")
                    # file.write(f"\t\t\t<coordinates>{lon[ii]},{lat[ii]}</coordinates>\n")
                    file.write("\t\t</Point>\n")
                    file.write("\t</Placemark>\n")
                file.write("</Document>\n")
                file.write("</kml>\n")
    
    def write_output_in_netcdf(self):
        """
        Transcript the simulation ASCII output to the netCDF format.
        """
        out_files = glob.glob(f"{self.work_dir}/**/blpb*.dat", recursive=True)
        for dat_file in out_files:
            with open(dat_file,"r") as file:
                data = file.readlines()
            time_secs = [float(line.split(";")[1]) for line in data[7:-1]] 
            lon       = [float(line.split(";")[2]) for line in data[7:-1]]
            lat       = [float(line.split(";")[3]) for line in data[7:-1]]
            alt       = [float(line.split(";")[4]) for line in data[7:-1]]
            w         = [float(line.split(";")[5]) for line in data[7:-1]]
            tp        = [float(line.split(";")[6]) for line in data[7:-1]]
            start_datetime = datetime.datetime.strptime("T".join(os.path.basename(dat_file).split("_")[1:3]),"%Y%m%dT%H%M%S")
            with nc.Dataset(f"{dat_file[:-4]}.nc",mode="w",format='NETCDF4') as ncfile:
                ncfile.conventions              = "CF-1.0"
                ncfile.netcdf_version_id        = nc.__netcdf4libversion__
                ncfile.standard_name_vocabulary = "NetCDF Standard"
                ncfile.title                    = "BAMED estimated baloon trajectory"
                ncfile.summary                  = "This file contains baloon estimated trajectories computed by the tool BAMED"
                ncfile.institution              = "OMP / Magellium"
                if self.lon_min>=180.0 or self.lon_max>=180.0:
                    ncfile.westernmost_longitude    = 0
                    ncfile.easternmost_longitude    = 360
                else:
                    ncfile.westernmost_longitude    = -180
                    ncfile.easternmost_longitude    = 180
                ncfile.southernmost_latitude    = -90
                ncfile.northernmost_latitude    = 90

                YYYMMDD = os.path.basename(dat_file).split("_")[1]
                HHMMSS  = os.path.basename(dat_file).split("_")[2]
                ncfile.site_name = os.path.basename(os.path.dirname(dat_file))
                ncfile.launch_lat  = data[0].split(";")[1].strip()
                ncfile.launch_lon  = data[1].split(";")[1].strip()
                ncfile.launch_date = f"{YYYMMDD[:4]}-{YYYMMDD[4:6]}-{YYYMMDD[6:8]}"
                ncfile.launch_time = f"{HHMMSS[:2]}:{HHMMSS[2:4]}:{HHMMSS[4:6]}"

                ncfile.creation_time            = str(datetime.datetime.now()).split(".")[0]
                ncfile.modification_time        = str(datetime.datetime.now()).split(".")[0]
                # ncfile.createDimension("lat",len(lon)+1)
                # ncfile.createDimension("lon",len(lon)+1)
                ncfile.createDimension("time",len(lon)+1)
                # ------------------------------------------
                var = ncfile.createVariable("launch_lat", np.float32)
                var.units = 'degrees_north'
                var.standard_name = 'latitude'
                var.long_name = 'Latitude of the baloon launch'
                var[:] = float(data[0].split(";")[1].strip())
                # ------------------------------------------
                var = ncfile.createVariable("launch_lon", np.float32)
                var.units = 'degrees_east'
                var.standard_name = 'longitude'
                var.long_name = 'Longitude of the baloon launch'
                var[:] = float(data[1].split(";")[1].strip())
                # ------------------------------------------
                var = ncfile.createVariable("density", np.float32)
                var.units = '-'
                var.standard_name = 'density'
                var.long_name = 'Density of the baloon'
                var[:] = float(data[4].split(";")[1].strip())
                # ------------------------------------------
                var = ncfile.createVariable("lat", np.float32, ("time",))
                var.units = 'degrees_north'
                var.standard_name = 'latitude'
                var.long_name = 'Latitude of the baloon position'
                var[:] = [float(data[0].split(";")[1].strip())] + lat
                # ------------------------------------------
                var = ncfile.createVariable("lon", np.float32, ("time",))
                var.units = 'degrees_east'
                var.standard_name = 'longitude'
                var.long_name = 'Longitude of the baloon position'
                var[:] = [float(data[1].split(";")[1].strip())] + lon
                # ------------------------------------------
                var = ncfile.createVariable("alt", np.float32, ("time",))
                var.units = 'meters'
                var.standard_name = 'altitude'
                var.long_name = 'Altitude of the baloon position'
                var[:] = [0] + alt
                # ------------------------------------------
                var = ncfile.createVariable("w", np.float32, ("time",))
                var.units = 'Pa/s'
                var.standard_name = 'vertical_velocity'
                var.long_name = 'Vertical velocity at simulation timestamps'
                var[:] = [0] + w
                # ------------------------------------------
                var = ncfile.createVariable("tp", np.float32, ("time",))
                var.units = 'meters'
                var.standard_name = 'total_precipitation'
                var.long_name = 'Total precipitations at simulation timestamps'
                var[:] = [0] + tp
                # ------------------------------------------
                YYYMMDD = os.path.basename(dat_file).split("_")[1]
                HHMMSS  = os.path.basename(dat_file).split("_")[2]
                var = ncfile.createVariable("time", np.float32, ("time",))
                var.units = 'seconds since ' + f"{YYYMMDD[:4]}-{YYYMMDD[4:6]}-{YYYMMDD[6:8]}" + " " + f"{HHMMSS[:2]}:{HHMMSS[2:4]}:{HHMMSS[4:6]}"
                var.standard_name = 'time'
                var.long_name = 'Time stamp of the baloon position'
                var.calendar = "standard"
                var[:] = [0] + time_secs

# ###############################################################################
# #           Configuration object for the .dat configuration file              #
# ###############################################################################

class file_config:
    """
    file_config class allows an easier manipulation of the data needed for the .dat
    input file of the simulation
    """
    lon_min     = 0.0
    lon_max     = 1.0
    lat_min     = 0.0
    lat_max     = 1.0
    grid_res    = 1.0
    time_res    = 1
    n_levels    = 1
    launch_date = ""
    launch_time = ""
    n_days      = 3
    timestep    = 360
    site_lon    = 0.5
    site_lat    = 0.5
    density     = 0.9
    grib_path   = ""

    def write_config_file(self, file_dir: str) -> None:
        """
        Creates an input .dat file needed for the BAMED executable 

        Args:
            file_dir (str): path to the directory where to create the file; this argument
                            is handled by the main function based on the user's configuration
        """
        with open(f"{file_dir}/blbp.dat","w") as file:
            file.write("********************************************\n")
            file.write("* Input file for the BLBP trajectory model *\n")
            file.write("********************************************\n")
            
            file.write("* COMPUTATIONNAL DOMAIN\n")
            file.write(f"{self.lon_min : <25}WEST LONGITUDE (° EAST)\n")
            file.write(f"{self.lon_max : <25}EAST LONGITUDE (° EAST)\n")
            file.write(f"{self.lat_min : <25}SOUTH LATITUDE (° NORTH)\n")
            file.write(f"{self.lat_max : <25}NORTH LATITUDE (° NORTH)\n")
            file.write(f"{self.grid_res : <25}GRID SIZE IN DEGREE\n")
            file.write(f"{self.n_levels : <25}NUMBER OF VERTICAL LEVEL\n")
            file.write(f"{137 : <25}MAX NUMBER OF LEVEL\n")
            file.write("*\n")

            file.write("* TIME MANAGEMENT\n")
            file.write(f"{self.launch_date : <25}FIRST DATE (YYYMMDD)\n")
            file.write(f"{self.launch_time : <25}TIME (HHMMSS)\n")
            file.write(f"{self.n_days : <25}NUMBER OF DAYS\n")
            file.write(f"{self.time_res : <25}TIMESTEP OF DATA\n")
            file.write(f"{self.timestep/3600 : <25}TIMESTEP OF SIMULATION\n")
            file.write("*\n")

            file.write("* GRIB DIRECTORY\n")
            if self.grib_path[-1]!="/":
                file.write(f"{self.grib_path}/ GRIB DIRECTORY\n")
            else:
                file.write(f"{self.grib_path} GRIB DIRECTORY\n")
            file.write("*\n")

            file.write("* BALOON INFO\n")
            file.write(f"{1 : <25}NUMBER OF LAUNCH SITES\n")
            file.write("*\n")

            file.write("* LAUNCH SITE\n")
            file.write(f"{self.site_lon : <25}SITE LONGITUDE\n")
            file.write(f"{self.site_lat: <25}SITE LATITUDE\n")
            file.write(f"{1 : <25}NUMBER OF BALOONS\n")
            file.write(f"{self.density: <25}FIRST BALOON DENSITY\n")
            file.write(f"{0.00: <25}DENSITY RANGE BETWEEN BALOONS\n")
            file.write("*\n")

    def update(self, config_obj: bamed_simulation) -> None:
        """
        Updates .dat configuration fields that remains the same for all launches
        in current simulation

        Args:
            config_obj (bamed_simulation): main configuration object of the bamed_simulation class
        """
        self.lon_min   = config_obj.lon_min
        self.lon_max   = config_obj.lon_max
        self.lat_min   = config_obj.lat_min
        self.lat_max   = config_obj.lat_max
        self.grid_res  = config_obj.grib_grid_res
        self.n_levels  = config_obj.level_max - simulation_obj.level_min + 1
        self.timestep  = config_obj.simulation_timestep
        self.grib_path = config_obj.grib_dir
        self.n_days    = config_obj.simulation_duration
        self.time_res  = config_obj.grib_time_step

# ###############################################################################
# #                                                                             #
# ###############################################################################

def print_header_in_terminal() -> None:
    print("╔════════════════════════════════════════════════╗")
    print("║   ╱▔▔▔▔▔╲                                      ║")
    print("║  ▕╋╋╋╋╋╋╋▏    WELCOME                          ║")
    print("║   ╲╳╳╳╳╳╱            TO                        ║")
    print("║    ╲╋╋╋╱               THE                     ║")
    print("║ ~~  ╲▂╱  ~~~~~~~~~~        BAMED               ║")
    print("║~~~~~▕▅▏~~~~~~~~~               SIMULATION      ║")
    print("║ ~~~~~~~~~~~~                                   ║")
    print("╚════════════════════════════════════════════════╝")


def write_header_in_file(filepath: str) -> None:
    with open(filepath,"w") as file:
        file.write("╔════════════════════════════════════════════════╗\n")
        file.write("║   ╱▔▔▔▔▔╲                                      ║\n")
        file.write("║  ▕╋╋╋╋╋╋╋▏    WELCOME                          ║\n")
        file.write("║   ╲╳╳╳╳╳╱            TO                        ║\n")
        file.write("║    ╲╋╋╋╱               THE                     ║\n")
        file.write("║ ~~  ╲▂╱  ~~~~~~~~~~        BAMED               ║\n")
        file.write("║~~~~~▕▅▏~~~~~~~~~               SIMULATION      ║\n")
        file.write("║ ~~~~~~~~~~~~                                   ║\n")
        file.write("╚════════════════════════════════════════════════╝\n")


def start_log(log_filepath: str, shell_option: bool=False) -> logging.Logger:
    """
    Create and initiates a Python logger object for an easy handling of the
    log information and printing

    Args:
        log_filepath (str): filepath to the log file, handled by the main function.

        shell_option (bool, optional): if True, displays all the log information in the shell
                                       in addition to the txt file. Defaults to False.

    Returns:
        logging.Logger: logger object
    """
    log_handlers = []
    if shell_option==True:
        log_handlers.append(logging.StreamHandler())
        print_header_in_terminal()
    log_handlers.append(logging.FileHandler(log_filepath))
    write_header_in_file(log_filepath)
    logging.basicConfig(format="%(asctime)s   [%(levelname)s]   %(message)s",
                        datefmt="%d/%m/%Y %H:%M:%S",
                        handlers=log_handlers)
    logger = logging.getLogger('my_log')
    logger.setLevel(logging.DEBUG)
    return logger


def make_list_dat(grib_dir: str, output_dir: str) -> None:
    """
    Creates a txt file list.dat with the list of the available
    grib data. This file is one of the BAMED executable's inputs.

    Args:
        grib_dir (str): path to the directory with grib files, indicated
                        by the user in the configuration file
        output_dir (str): path to the directory where to create list.dat
    """
    files = glob.glob(f"{grib_dir}/????????.grib")
    files.sort()
    with open(f"{output_dir}/list.dat","w") as file:
        for grib_file in files:
            filename          = os.path.basename(grib_file)
            filename_datetime = datetime.datetime.strptime(filename[:8],"%y%m%d%H")
            YYYYMMDD          = datetime.datetime.strftime(filename_datetime,"%Y%m%d")
            HHMMSS            = datetime.datetime.strftime(filename_datetime,"%H%M%S")
            file.write(f"{YYYYMMDD}  {HHMMSS}  {filename}\n")


def run_bash_command(command_string: str, working_directory: str) -> None:
    """
    Executes bash commands and logs its output simultaneously

    Args:
        command_string (str): bash command to execute
    """
    process = subprocess.Popen(command_string, cwd=working_directory, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    while True:
        output = process.stdout.readline()
        if process.poll() is not None:
            break
        if output:
            LOGGER.info(output.strip().decode('utf-8'))
    return_code = process.poll()
    return return_code

# ###############################################################################
# #                               Main function                                 #
# ###############################################################################

if __name__=="__main__":

    """
    Main function which takes care of the simulation in the singularity container
    based on the user's configuration file.
    This Python script have to be executed in the BAMED container.

    Args:
        -bc         : Filepath to the xml configuration file (mandatory)
        --shell-log : Provide this argument if you want to display log messages on the screen in addition to the log file
    """

    import argparse
    
    parser = argparse.ArgumentParser(description="This Python script allows to manage multiple BAMED simulations based on the user configuration XML file",
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("-bc","--config", type=str, help="Filepath to the xml configuration file (mandatory)")
    parser.add_argument("--shell-log", action="store_true", help="Provide this argument if you want to display log messages on the screen in addition to the log file")
    args = parser.parse_args()
    config_xmlpath = args.config

    simulation_obj = bamed_simulation(config_xmlpath)

    simulation_obj.verify_workdir()

    global LOGGER, LOG_FILEPATH
    LOG_FILEPATH = simulation_obj.work_dir+"/bamed_"+datetime.datetime.now().strftime("%Y%m%d_%H%M%S")+".log"
    LOGGER = start_log(LOG_FILEPATH, args.shell_log)

    status = simulation_obj.verify()
    if status!=0:
        sys.exit(1)
    
    simulation_obj.update_grib_fields()

    make_list_dat(simulation_obj.grib_dir, simulation_obj.work_dir)

    simulation_config = file_config()
    simulation_config.update(simulation_obj)

    # ------------------------------------------
    # For each launch date
    for date in simulation_obj.launch_start_datetime.keys():
        # ------------------------------------------
        # For each launch time of the current launch date
        for time in simulation_obj.launch_start_datetime[date]:
            simulation_config.launch_date = date
            if len(time)==4:
                simulation_config.launch_time = time+"00"
            if len(time)==6:
                simulation_config.launch_time = time
                # ------------------------------------------
            # For each launch site defined by the user
            for site_coords in simulation_obj.launch_coordinates:
                res_dir = f"{simulation_obj.work_dir}/{site_coords[0]}"
                if not os.path.exists(res_dir):
                    os.mkdir(res_dir)
                shutil.copy(f"{simulation_obj.work_dir}/list.dat", res_dir)
                simulation_config.site_lat = site_coords[1]
                simulation_config.site_lon = site_coords[2]
                # ------------------------------------------
                # For each baloon density defined by the user
                for density_val in simulation_obj.density:
                    LOGGER.info("+-------------------------------------------------------------------------+")
                    LOGGER.info(f"|  Launch date    : {date : <54}|")
                    LOGGER.info(f"|  Launch time    : {time : <54}|")
                    coords_string = f"|  Launch site    : (lat={simulation_config.site_lat}°, lon={simulation_config.site_lon}°)"
                    LOGGER.info(f"{coords_string : <74}|")
                    LOGGER.info(f"|  Baloon density : {density_val : <54}|")
                    LOGGER.info("+-------------------------------------------------------------------------+")
                    simulation_config.density = density_val
                    # ------------------------------------------
                    # Write input .dat file for the BAMED executable
                    simulation_config.write_config_file(res_dir)
                    # ------------------------------------------
                    # Run simulation
                    ret_code = run_bash_command(BAMED_EXE, res_dir)
    
    LOGGER.info("+-------------------------------------------------------------------------+")
    LOGGER.info("|  Plotting results...                                                    |")
    LOGGER.info("+-------------------------------------------------------------------------+")
    simulation_obj.plot_results()
    
    LOGGER.info("+-------------------------------------------------------------------------+")
    LOGGER.info("|  Adding results in KML format...                                        |")
    LOGGER.info("+-------------------------------------------------------------------------+")
    simulation_obj.write_output_in_kml()

    LOGGER.info("+-------------------------------------------------------------------------+")
    LOGGER.info("|  Adding results in netCDF format...                                     |")
    LOGGER.info("+-------------------------------------------------------------------------+")
    simulation_obj.write_output_in_netcdf()

    LOGGER.info("+-------------------------------------------------------------------------+")
    LOGGER.info("|  SIMULATION DONE !                                                      |")
    LOGGER.info("+-------------------------------------------------------------------------+")
