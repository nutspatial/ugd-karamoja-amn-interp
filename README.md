

# Spatial interpolation of acute malnutrition: a quest using probability proportional to the size of the population-based survey data

To effectively combat this public health issue, enhancing the ability to
tell *where* acute malnutrition is of great concern over other areas is
paramount. Possessing this information leverages the ability of
programmers to prioritize limited resources - that are now getting even
more shrunk amid the modern resource-constraint era.

This repository is an R implementation of a quest applying spatial
interpolation modelling techniques. The quest aims to explore the
usability of probability proportional to the size of the
population-based survey sampling data to predict the prevalence of acute
malnutrition from the observed sampling points to unobserved locations
across a given survey area of interest.

## Research (quest) question:

- Does spatial interpolation produce reliable (precise and accurate)
  estimates when applied to a small-scale PPS-based survey?

If yes:

- How comparable are the predicted estimates against the observed (from
  the original survey)

## Study area

The analysis covered the Karamoja region in Uganda. This region has an
area of 27,514 square kilometres and is subdivided into 9 districts
(admin level 2), namely Abim, Amudat, Kaabong, Karenga, Kotido, Moroto,
Nabilatuk, Nakapiripirit and Napak. The immediate lower administrative
units after districts are counties (admin level 4). There are 64
counties in Karamoja.

The quest used derived from a Food Security and Livelihood Assessment
(FSLA) conducted in February 2021. The FSLA was designed to represent
each district independently. It employed the SMART methodology..

<img src="karamoja_region.png"
data-fig-alt="Map of the Karamoja region, Uganda, showing district and within district boundaries"
data-fig-align="center" width="300" />

## Repository Structure

- `raw-data/`: a data frame of the input data. This is encrypted.
- `R/`: some handy user-defined functions.
- `scripts/`: a set of `R` scripts. These are split into different
  files, based on the specific task that they execute.

The following workflow is recommended:

``` mermaid
flowchart LR
A(Retrieve secret key for decryption)
B(Load project-specific functions.R)
C(Run read-in-data.R)
D(Run wrangle-aspatial-attributes.R)
E(Run data-quality-check.R)
F(Run wrangle-spatial-attributes.R)
G(Run krige-interpolate-wfhz.R OR krige-interpolate-wfhz-automap)

  A --> B --> C --> D --> E --> F --> G
```

The above flowchart can be implemented simply by running the `scrip.R`
file found in the root directory.

## Reproducibility information

The repository was created in `R` version 4.4.2. This project uses the
`{renv}` framework to record `R` package dependencies and versions.
Packages and versions used are recorded in `renv.lock` and code used to
manage dependencies is in `renv/` and other files in the root project
directory. On starting an `R` session in the working directory, run
`renv::restore()` to install R package dependencies.

## Data encryption

This project uses `{cyphr}` to encrypt the raw data that lives in
`data-raw/` directory. In order to be able to access and decrypt the
encrypted data, the user will need to have created their own personal
SSH key and make a request to be added to the project. An easy-to-grasp
guide on how to make a request will be found
[here](https://github.com/OxfordIHTM/cyphr-encryption-demonstration#)

## License

This repository is licensed under a GNU General Public License 3
(GPL-3).

## Feedback

If you wish to give feedback, file an issue or seek support, kindly do
so [here](https://github.com/nutspatial/ugd-karamoja-amn-interp/issues).

## Author

Tomás Zaba
