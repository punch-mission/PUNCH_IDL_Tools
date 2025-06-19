# PUNCH_IDL_Tools
Tools for accessing PUNCH data in [IDL](https://www.nv5geospatialsoftware.com/docs/using_idl_home.html) and/or [GDL](https://github.com/gnudatalanguage/gdl).

These tools require the `gen` and `astrom` packages from
[SolarSoft](https://www.lmsal.com/solarsoft/), and optionally the
`vso/ontology` package.

The intent is that they will allow SolarSoft users to ingest and
interpret any level of PUNCH data from Level 0 to 3. 

They should work equally well in recent versions of IDL and the current
GIT of GDL.

## Status

These routines have as yet had limited testing. In particular, as yet
no images with non-null distortion tables have been available for
testing.

All testing by the Author has been on Linux systems ([OpenSuSE](https://www.opensuse.org/) with IDL
and [Manjaro](https://manjaro.org/) with GDL)

## Tools provided

### READ_PUNCH:

Reads a PUNCH FITS file into IDL. This should handle the
Rice-compressed files either with the shared library in the appropriate
subdirectory of `/soft/rsi/ssw/vobs/ontology/binaries/` and
`fitsio_read_image` or via the [`imcopy`](####imcopy) command, and the
regular `readfits` procedure.

In general, it is probably better to use the shared library by adding
the `/use_shared_lib` keyword. However the use of DLL's is not (yet)
supported by GDL so `imcopy` must be used in that case.

**N.B.** The layout of a Rice-compressed file is assumed to be:

* A dummy header for the primary data block.
* The actual primary data compressed into a BINTABLE in the first
extension.
* Uncertainty information in the second extension.
* Distortion lookup tables in later extensions (`fits_info` is used to
  identify them).


### PUNCH_STACK

Reads the data using `read_punch.pro` from a list of PUNCH FITS files into 
data cubes of total brightness and polarized brightness. Also returns 
supporting data (uncertainties and distortions) as available.

### PUNCH_XY2AD and PUNCH_AD2XY


- punch_xy2ad: Converts a given pixel position to Right Ascension (R.A.) 
                      and Declination (Dec).
- punch_ad2xy: Performs the inverse operation, converting R.A. and Dec. 
                      back to pixel coordinates.

These functions also correct for optical distortion (if distortion table is available) 
in the input PUNCH file.

Why These Functions?

In the current SolarSoft astrometry package, accessing distortion tables requires opening 
the file with fits_open. However, this approach does not work for Rice-compressed FITS files.
To overcome this limitation, these routines integrate the relevant functionalities of:

- fits_xy2ad and xy2ad (for pixel-to-R.A./Dec. conversion)
- fits_ad2xy and ad2xy (for R.A./Dec.-to-pixel conversion)

By combining these components, punch_xy2ad and punch_ad2xy ensure accurate coordinate 
transformations even for compressed files.


### PUNCH_WCS_GET_COORD and PUNCH_WCS_GET_PIXEL

These routines support the conversion of WCS structure from data coordinates to IDL 
pixel position and back. These allow the use of distortion tables alongside the 
WCS family of SSWIDL astrometry codes.
- punch_wcs_get_coord: This procedure takes a WCS structure, and calculates the 
                            data coordinates at each IDL pixel position.
- punch_wcs_get_pixel: This procedure is inverse of punch_wcs_get_coord and takes 
                            a WCS structure, and converts coordinates back into IDL pixel positions.


### FIX_Z_HEAD

Modifies header keys starting with "Z", in Rice-compressed files read
by `fitsio_read_image` to be the actual image parameters
(e.g. `ZNAXIS1` becomes `NAXIS1`), tries to follow the pattern of
`imcopy`. This is only intended to be used internally within
`read_punch`.


## Building imcopy

is an ancillary program that is part of the cfitsio
library. However many Linux distributions do not build it by
default. It can be simply built by downloading the [cfitsio
source](https://heasarc.gsfc.nasa.gov/fitsio/) and going to the
`utilities` subdirectory and compiling with (assuming the cfitsio
library is installed):

    gcc -o imcopy imcopy.c $(pkg-config -libs -cflags cfitsio$)

and copying the executable to somewhere in your path
(e.g. `/usr/local/bin` or `~/bin` if you don't have root access).


## Using latest SSW package
We recommend using the latest SSW package to run these routines smoothly
across various platforms (Windows, Linux, MacOS etc.).

## Getting help

All routines contain a documentation header that can be read via
`doc_library`.

For further assistance, please open an issue or create a discussion
here on GitHub.

Note that at present the distortion tables are not tested as we do not
yet have data with non-null tables.

## Contributing
We welcome all contributions. Please open a pull request to contribute.
