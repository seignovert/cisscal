Calibration data
=================

[![PDS archive](https://img.shields.io/badge/PDS-calib.tar.gz-blue.svg)][calib-archive]
[![Last release](https://img.shields.io/github/release/seignovert/cisscal.svg)][last-release]

[cisscal]: https://pds-imaging.jpl.nasa.gov/data/cassini/cassini_orbiter/coiss_0011_v4.2/extras/cisscal/
[pds]: https://pds-imaging.jpl.nasa.gov/data/cassini/cassini_orbiter/coiss_0011_v4.2/calib/
[last-release]: https://github.com/seignovert/cisscal/releases/latest

> This repository is a clone of the latest version of [CISSCAL][cisscal] calibration data published on the [PDS][pds].
> It is provided _as is_ without any warranty. The `.lbl`, `.img`, `.xdr` files are ignored by `git` but
> can be downloaded from [`calib.tar.gz`][calib-archive] archive.

---

The CALIB directory contains Cassini ISS calibration data files derived from
ground and in-flight calibration analyses. Some of these files may not
explicitly conform to PDS format.

The directory and filenaming structure of the CALIB directory is set up to
be fully compatible with the CISSCAL software package contained in the
EXTRAS directory on this volume. In order to ensure that filename cases
remain the same, regardless of the DVD filesystem being used, the entire
contents of the CALIB subdirectories have also been provided in a g-zipped
TAR archive, [`calib.tar.gz`][calib-archive].

[calib-archive]: https://pds-imaging.jpl.nasa.gov/data/cassini/cassini_orbiter/coiss_0011_v4.2/calib/calib.tar.gz

For a brief tutorial on Cassini image calibration, see section 4.4 of the
ISS Data User's Guide, which can be found in the document subdirectory of the
Calbration archive volume.

`antibloom/`
------------
Files containing masks of the anti-blooming mode pixel pairs
in the NAC and WAC, as seen in ground calibration data.

`bitweight/`
------------
Files containing bitweight corrections as a function of camera,
optics temperature (-10, +5, +25 degrees C) and gain state, as
derived during ground calibration.

`correction/`
-------------
Files containing normalized wavelength-dependent correction
function to be applied to the quantum efficiencies of the NAC
and WAC during absolute calibration, as well as files
containing filter-specific correction factors to be applied to
each filter combination individually.

`darkcurrent/`
--------------
Image files containing the dark parameters for the
interpolation dark model used in CISSCAL.

`distortion/`
-------------
Files containning a lists of NAC and WAC focal lengths for use
in correcting geometric distortion.

`dustring/`
-----------
This directory contains VICAR-image files used to correct for
dustrings that are not found in the slope files created during
ground calibration.

`efficiency/`
-------------
This directory contains various files necessary for calculating
the transmission of the ISS cameras as a function of
wavelength.

`efficiency/systrans/`
---------------------
This subdirectory contains the
non-wavelength-integrated system transmission values
(`QE * optics * filter1 * filter2`) for each filter combination.

`lut/`
-------
This directory contains the inverse-lookup table used to
convert table-encoded Cassini ISS images from an 8-bit range
back 12-bit.

`offset/`
-------
The offset directory contains shutter offset information for
each camera as a function of temperature.

`slope/`
-------
The slope directory contains the VICAR-format "slope files"
derived during ground calibration, and used by the calibration
software as flatfields. Database files are also included which
specify the recommended slope files to use for each filter
combination at a given optics temperature.

`xpsf/`
-------
The xpsf directory contains VICAR-format images representing
the point spread function for each filter combination. There
are both _core_ PSFs which extend to a 100-pixel radius for
the WAC, or 200 pixels for the NAC, at 0.1-pixel resolution
(`2000 x 2000` and `4000 x 4000` arrays, respectively); as well as
"extended" PSFs which extend out to 512 pixels at 1-pixel
resolution (`1024 x 1024`).