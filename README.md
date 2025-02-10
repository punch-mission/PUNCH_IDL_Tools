# PUNCH_IDL_Tools
Tools for accessing PUNCH data in IDL and/or GDL.

These tools require the `gen` and `astrom` packages from
[SolarSoft](https://www.lmsal.com/solarsoft/).

The intent is that they will allow SolarSoft users to ingest and
interpret any level of PUNCH data from Level 0 to 3. 

They should work equally well in recent versions of IDL and the current
GIT of GDL.

## Status

These routines have as yet had limited testing. In particular, as yet
no images with non-null distortion tables have been available for testing.

## Tools provided.

### READ_PUNCH:

Reads a PUNCH FITS file into IDL. This should handle the
Rice-compressed files either with the shared library and
`fitsio_read_image` or via the [`imcopy`](####imcopy) command, and the
regular `readfits` procedure.

**N.B.** The layout of a Rice-compressed file is assumed to be:

* A dummy header for the primary data block.
* The actual primary data compressed into a BINTABLE in the first
extension.
* Uncertainty information in the second extension.
* Distortion lookup tables in the third and fourth extensions.


### PUNCH_STACK

Reads the data from a list of PUNCH FITS files into a data cube. Also
returns supporting data (uncertainties and distortions) as available.

### PUNCH_XY2AD and PUNCH_AD2XY

In the current SolarSoft astrometry package, the only way to access
distortion tables is by opening the file with `fits_open`, but this is
not possible for RIce-compressed files. These routines therefore
combine the relevant parts of `fits_xy2ad` and `xy2ad`, and
`fits_ad2xy` and `ad2xy` respectively.

### PUNCH_WCS_GET_COORD and PUNCH_WCS_GET_PIXEL

These allow the use of distortion tables alongside the WCS family of
SSWIDL astrometry codes.

### FIX_Z_HEAD

Modifies header keys starting with "Z", in Rice-compressed files read
by `fitsio_read_image` to be the actual image parameters
(e.g. `ZNAXIS1` becomes `NAXIS1`), tries to follow the pattern of
`imcopy`. This is only intended to be used internally within
`read_punch`.


## Getting help

All routines contain a documentation header that can be read via
`doc_library`.

For further assistance, please open an issue or create a discussion
here on GitHub.

Note that at present the distortion tables are not tested as we do not
yet have data with non-null tables.

## Contributing
We welcome all contributions. Please open a pull request to contribute.


#### imcopy

is an anciliary program that is part of the cfitsio
library. However many Linux distributions do not build it by
default. It can be simply built by downloading the [cfitsio
source](https://heasarc.gsfc.nasa.gov/fitsio/) and going to the
`utilities` subdirectory and compiling with (assuming the cfitsio
library is installed):

    gcc -o imcopy imcopy.c $(pkg-config -libs -cflags cfitsio$)

and copying the executable to somewhere in your path (e.g. `~/bin`).
