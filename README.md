_Last updated: April 6th, 2025_

# Bee Diversity in Costa Rica: A National Survey of Coffee Agroecosystems

Authors: Christopher Donovan, Adina Chain-Guadarrama, Alejandra Martínez-Salinas, Natalia Aristazábal, and Taylor H. Ricketts

## About

This repository supports the reproduction of analyses for a survey of bee pollinators on 140 farms across five coffee regions of Costa Rica. A pre-print of the paper is pending at OSF, and will be linked here when approved. Data used in the analysis is also available at the [Dataverse Project](https://dataverse.org/), DOI: https://doi.org/10.7910/DVN/67X9CS. 

Note that to run the analysis, you will have to download a directory of spatial files from Google Drive and unpack it in the `1_raw/` folder, as these files were too large to push to GitHub. More details in the "Navigating the Project" section below.


## Ownership and Licensing

<div style="display: flex; align-items: center;">
  <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.en.html#license-text">
    <img alt="GPLv3 License" style="border-width:0; margin-right: 10px;" src="https://www.gnu.org/graphics/gplv3-or-later-sm.png" />
  </a>
  <span>
    The code in this project is licensed under the 
    <a rel="license" href="https://www.gnu.org/licenses/gpl-3.0.en.html#license-text">GNU General Public License v3</a>.
  </span>
</div>
<br>

<div style="display: flex; align-items: center;">
  <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.en">
    <img alt="CC BY-NC-SA License" style="border-width:0; width: 100px; margin-right: 10px;" src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by-nc-sa.png" />
  </a>
  <span>
    The data in this project is licensed under the 
    <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.en">CC BY-NC-SA 4.0 License</a>.
  </span>
</div>
<br>


## Navigating the Project

Start by opening `cr_bees.Rproj` to load the project. Dependencies are managed with `renv`. To restore the packages and versions used in the project, install the `renv` package if necessary, then run:

``` r
# install.packages('renv')
renv::restore()
```

Once packages are restored, navigate to the `table_of_contents.R` file. This will list all the relevant scripts in the order they should be run, usually with a little bit of description. Use the definition function (`F2`) or `ctrl/cmd + left click` on the text of a script in the table of contents will open that script in a new tab. Each script starts by loading the necessary objects and ends by saving them and removing all data objects in the environment. This way, the entire project can be run by running the table of contents. Alternatively, you can just open up a single script and have it work without running any others. 

Note that spatial files are quite large, so I have not pushed them to GitHub. For now, they can be downloaded from a public [Google Drive directory](https://drive.google.com/file/d/1ukJwSo42vSAidgO-fc-HgZcdTnyX7bwn/view?usp=drive_link). The zipped file should be unpacked into the `1_raw/spatial/` folder. From there, relative file paths from the project should be able to find the necessary spatial files and all the scripts should run. 
