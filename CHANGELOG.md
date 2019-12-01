Version history
===============

New in version 4.0:
-------------------
- Improved logic in option file reader and `cisscal_cl.pro`
- Changed formatting of bitweight files to ease parsing
- Added logic to handle bias/2-hz noise removal for TABLE images of various gain states
- Improved CISSCAL_Log formatting
- Updates to polarized filter algorithm in `src/make_polar_image.pro`
    (see: `src/make_polar_image_readme.txt`).


New in version 3.9.1
--------------------
- Fixed non-working _hotpix_ keyword in command line code
- Removed obsolete text from batch GUI


New in version 3.9
------------------
- Significant update to absolute correction for NAC and WAC:
    * New correction factors
    * Updated quantum efficiency correction
    * Added sensitivity vs- time correction
    * Incorporated solid angle/effective area values with more traceable provenance
    * Removed Jupiter absolute calibration option (no longer needed)
- Updated command line code to match changes to calibration options file
- Fixed mkdir path bug in subtractdark (now compatible with _space_ character)
- Incorporated hot pixel data since 2012
- Added hot pixel on/off toggle to subtractdark and associated GUI changes
- Added saturated/missing pixel value dialog to GUI
- Replaced binary options file with text-based version (e.g.
  cisscal_default_options.txt)
- Incorporated changes for possible future GDL compatibility (CassLabels class attributes)
- Fixed negative values in BIAS_STRIP_MEAN bug
- Improved plot range in _plot overclocks_ code
- Converted mottle file from TIFF to VICAR format
- Cleaned up and improved batch mode text entry and dialogs
- Extended Jupiter and Saturn solar distance files through end of mission
- Added functionality for and improved handling of shutter-inhibited images
- General code and comment cleanup


New in version 3.8
------------------
- Fixed bugs in command-line mode.
- Fixed negative array index bug in label reading code.
- Fixed polarized filter bug in dust ring code.
- Added NAC dust ring corrections derived from Titan atmosphere images.
- Added correction to preexisting bright ring for images with anti-blooming mode = OFF.
- Changed flatfield algorithm to ignore NaN pixels in slope files.
- Added WAC filters to Jupiter correction.
- Switched default CL1/CL2 slope files from gain 3 to gain 2.


New in version 3.7
------------------
- Improved and cleaned up log output throughout code.
- Modified VICAR label behavior, added _show/hide label_ menu option.
- Various GUI enhancements/adjustments to make suitable for smaller screen
  resolutions.
- Added Jupiter correction factors and corresponding GUI option.
- Added saturated and missing pixel output values to options file.
- Improved handling of polarized images.
- Removed old IDL code version tags.
- Fixed various display and GUI bugs.
- Moved plot overclocks button to Tools menu.
- Fixed error handling for invalid image files.
- Added bias subtraction and de-LUT to image mean mask preview.
- IDL 8 compatibility changes.
- New dark label format; old dark repositories will be ignored.
- Consolidated GUI and command line option file reader (`cisscal_readoptfile`).
- Simplified GUI and improved logic for bias/two hz noise removal.
- Added binary header to output VICAR files, cleaned up VICAR write code.
- Updated shutter offset values.
- Re-designed output VICAR label format.
- Improved bias subtraction and 2 Hz noise removal: better filtering, better
    handling of missing lines and slow-readout images.
- Removed horizontal ringing artifacts and residual hot pixel streaks from
    dark current parameter files, and corrected saturated pixels.
- Added hot pixel files and updated dark subtraction algorithm accordingly.
- Added intermediate options for DN-to-flux conversion.
- Updated format of system transmission files, included transmission for
    both parallel and perpendicular orientations of polarized filters.
- Added `make_polar_image.pro` for processing polarized images; modified
    absolute flux correction for use with new version of polarizer code.


New in version 3.6 (changes since version 3.3)
----------------------------------------------
- Improved dark simulation code to increase speed.
- Modifications to handling of summed and lossy-compressed images by 2 hz removal routine.
- Incorporated latest NAC and WAC absolute correction factors.
- Combined, where possible, system transmission files (located in `support/efficiency/systrans/`)
    with _blocking_ filter transmission from ground calibration.
- Added cisscal_cl, the CISSCAL command-line mode, enabling use of
    CISSCAL without the GUI.
- Added compatibility for several operating systems, including Mac.
- Removed 2-parameter dark current model (now defunct).
- Added default options file, cisscal_default_options.txt, allowing user
    to specify defaults for all calibration parameters-
- Updates to `linetime.pro` (readout time calculation).
- Various bug fixes and clean-up of GUI toggle and window behavior.
- Changed histogram code to allow large DNs-
- Implemented a slightly wider low-pass filter for 2-Hz removal to deal
    better with cosmic rays in overclocked pixels.
- Disabled 2-Hz noise removal for summed images.
- Updated user manual.
- Data file clean-up, reformatting.
- Extended Sun-Saturn distance calculator to be useful for images taken after 2009.
- Added output filename suffix to calibration options file.
- Added new central/effective wavelength files.
- Added omega0.tab (in place of `effic_db*`).
- Fixed flight software version bug in debias code.
- Cleaned up rebinning behavior in flatfield code-