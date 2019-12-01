Cassini ISS CALibration (CISSCAL)
=================================

[![CICLOPS](https://img.shields.io/badge/Website-CICLOPS-blue.svg)][ciclops]
[![Data](https://img.shields.io/badge/Data-PDS-blue.svg)][pds]
[![User Guide](https://img.shields.io/badge/User%20guide-PDF-green.svg)][user_guide]
[![Last release](https://img.shields.io/github/release/seignovert/cisscal.svg)][last-release]

[ciclops]: ciclops.org/sci/cisscal.php
[pds]: https://pds-imaging.jpl.nasa.gov/data/cassini/cassini_orbiter/coiss_0011_v4.2/
[user_guide]: docs/iss_data_user_guide_180916.pdf
[last-release]: https://github.com/seignovert/cisscal/releases/latest

> This repository is a clone of the latest version of [CISSCAL][ciclops] published on the [PDS][pds].
> It is provided _as is_ without any warranty.

---

CISSCAL is the Cassini ISS image calibration software package. For more
information, including detailed setup instructions, please see Section 4.4
of the ISS Data User's Guide, available in the `document/` directory of the
ISS Calibration Volume ([COISS_0011 version 4.2][pds]) or at the following [URL][user_guide_url]:

[user_guide]: http://pds-imaging.jpl.nasa.gov/documentation/ISS_Data_User_Guide_180916.pdf

```
http://pds-imaging.jpl.nasa.gov/documentation/ISS_Data_User_Guide_180916.pdf
```

Additionally, see `src/make_polar_image_readme.txt` for information regarding
polarization calibration using the standalone code `make_polar_image.pro`.

Note that when downloaded from the PDS, the CISSCAL software will be found in
the `extras/cisscal/` directory, and the calibration support files will be
found in the `calib/` directory.

CISSCAL is written in Interactive Data Language (`IDL`), and must be run on a
computer on which IDL has been installed. As of this writing, CISSCAL has
been tested for compatibility with IDL versions `5.5` through `8.2`. In addition,
the following system variables must be set (typically in user's `.cshrc` file
or equivalent) for CISSCAL to work properly:

- `CisscalDir`, directory containing CISSCAL code:
```
setenv CisscalDir /home/username/cisscal3_9_1
```

- `CalibrationBaseDir`, directory containing calibration support files:
```
setenv CalibrationBaseDir /home/username/support3_9
```

- `ImageBaseDir`, default image directory:
```
setenv ImageBaseDir /home/username/images
```

The default calibration options can be set by the user by editing the text
file `src/cisscal_default_options.txt`, located in the `CisscalDir` defined above.

CISSCAL was developed in stages, beginning in August 1998, under the auspices
of the Cassini Imaging Central Laboratory for Operations (CICLOPS) directed
by Dr. Carolyn Porco, ISS Team Leader. The theoretical basis and pipeline
design for Cassini image calibration was developed by ISS team member Dr.
Robert West of JPL. The systems design for the CISSCAL software, and its
original implementation and development, was the work of Dr. Kevin Beurle at
Queen Mary, University of London from August 1998 through March 2002.
Beginning in April 2002, major design and algorithmic modifications, software
additions, debugging and maintenance have been performed by Ben Knowles of
CICLOPS/Space Science Institute, Boulder, CO under the direct supervision of
West.  Significant contributions (algorithms, software, evaluation and
validation, etc.) have been made by West, Vance Haemmerle of JPL (while at
CICLOPS), Daren Wilson of CICLOPS, and other members of the Cassini ISS Team.

Direct all questions to [Ben Knowles][Knowles] at Space Science Institute, Boulder, CO.

[Knowles]: mailto:ben.cisscal@gmail.com