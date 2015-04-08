# Landuse System Archetypes Analysis as a WPS Process

Contact: d.nuest@52north.org

## Files

* `wps_config.xml`: WPS configuration file, configures an LocalRAlgorithmRepository including a Spring wrapper repository
* `full-process.R` and `advanced-plots.R`: The R script files that must be deployed in the WPS script directory
* `system-archetypes-request-example-01.xml`: Execute request example


## Development Notes

To run the land use archteypes process locally on a development machine follow these steps:

* Check out the latest development version of WPS4R from https://github.com/nuest/WPS/tree/feature/spring-new-wps4R
* Install and configure R 3.x, see also https://wiki.52north.org/bin/view/Geostatistics/WPS4R#Installation
  * Important: The script uses the kohonen package, but the latest version 2.0.18 does not work. Install version 2.0.15 instead (see [here](https://github.com/52North/glues-wps/blob/master/WPS4R/system-archetypes-original/full-process.R#L33))
* Configure the work directory setting (placeholder in example config is `SET_PROPERTY_ON_SERVER`), e.g.
```
<Property name="R_resourceDirectory" active="true">R/resources;C:/Users/<username>/System-Archetypes/inputData</Property>
<Property name="R_scriptDirectory" active="true">R/scripts;C:\git\system-archetypes-original</Property>
<Property name="R_wdName" active="true">C:/Users/<username>/System-Archetypes/workspace-basedir</Property>
```
* Save the input data in the resource directory
* Save the process script files in the script directory
