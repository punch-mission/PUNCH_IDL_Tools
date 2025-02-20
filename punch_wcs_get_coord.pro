;+
; Project     :	STEREO
;
; Name        :	PUNCH_WCS_GET_COORD()
;
; Purpose     :	Get coordinate values for WCS structures
;
; Category    :	FITS, Coordinates, WCS
;
; Explanation :	This procedure takes a WCS structure, and calculates the data
;               coordinates at each IDL pixel position.
;
; Syntax      :	Coordinates = PUNCH_WCS_GET_COORD( WCS  [, PIXELS] )
;
; Examples    :	Suppose that WCS.NAXIS = [10,20].  The result of the command
;
;                       COORD = PUNCH_WCS_GET_COORD(WCS)
;
;               would be an array with dimensions [2,10,20], where
;               X=COORD[0,*,*] and Y=COORD[1,*,*].
;
;               To get the coordinates of a specific pixel, use e.g.
;
;                       COORD = WCS_GET_COORD(WCS, [5,5])
;
;               If IX and IY contain N pixel coordinates along the X and Y
;               directions respectively, use
;
;                       IXY = LONARR(2, N_ELEMENTS(IX))
;                       IXY[0,*] = IX
;                       IXY[1,*] = IY
;                       COORD = PUNCH_WCS_GET_COORD(WCS, IXY)
;
; Inputs      :	WCS     = Structure from FITSHEAD2WCS
;
; Opt. Inputs :	PIXELS  = If passed, then contains an array of IDL pixel
;                         locations to return coordinates from.  Otherwise,
;                         coordinates are returned for all the pixels in the
;                         array.
;
;                         The first dimension must be the number of axes
;                         (except for pixel lists).
;
;                         Note that IDL pixel locations differ by 1 from FITS
;                         pixels, e.g. IDL pixel 0 is FITS pixel 1, etc.
;
;                         However, pixel lists are an exception.  For pixel
;                         lists, the PIXELS array contains row numbers in the
;                         FITS binary table, ranging from 1 to N to match the
;                         calling sequence of FXBREAD.  Normally, PIXELS is a
;                         one dimensional array when used with pixel lists.
;                         The output array will have an additional axis
;                         prepended for the coordinate axes.
;
; Outputs     :	The result of the function is the array of coordinates, where
;               the first dimension steps through each coordinate axis.
;
; Opt. Outputs:	None.
;
; Keywords    :	XHDR, YHDR = FITS headers for X & Y distortion tables.
; 		XTABLE, YTABLE = X & Y distortion tables.
; 		RELATIVE   = If set, then only intermediate positions
;                            relative to the reference value are returned,
;                            i.e. CRVALi is not added.  This is mainly for
;                            internal use by some of the routines.
;
;               QUICK      = Depending on the projection, using /QUICK selects
;                            a quick approximation, rather than the full
;                            computationally expensive spherical projection.
;
;               FORCE_PROJ = Some projection routines, such as WCS_PROJ_TAN,
;                            contain logic which automatically selects the
;                            /QUICK option under certain conditions.  Using
;                            /FORCE_PROJ forces the full spherical coordinate
;                            transformation to be calculated.
;
;               MISSING    = Value to fill missing values with.  If not passed,
;                            then missing values are filled with IEEE
;                            Not-A-Number (NaN) values.  This is for
;                            coordinate systems, such as heliographic, that
;                            might not be defined for all pixels.
;
;               NOWRAP     = If set, don't wrap the longitude values to be
;                            between +/-180 degrees.  Only used for cylindrical
;                            projections (CAR,CEA,CYP,MER).
;
;               POS_LONG   = If set, then force the output longitude to be
;                            positive, i.e. between 0 and 360 degrees.  The
;                            default is to return values between +/- 180
;                            degrees.
;
;               TOLERANCE  = Convergence tolerance for reiterative technique
;                            used by some projections.
;
;               MAX_ITER   = Maximum number of iterations.  Default is 1000.
;
;               NODISTORTION = If set, then don't apply any distortion keywords.
;
; Calls       :	VALID_WCS, PRODUCT, TAG_EXIST, WCS_PROJ_*, WCS_APPLY_DISTORTION,
;               WCS_APPLY_DIST_TABLE
;
; Common      :	None.
;
; Restrictions:	SOLARNET distortions are only handled for binary tables.
;               APPLY=5 and APPLY=6 are treated identically.  ASSOCIATE=5 is
;               ignored.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 19-Apr-2005, William Thompson, GSFC
;               Version 2, 26-Apr-2005, William Thompson, GSFC
;                       Added AZP and SIN projections, MISSING keyword
;		Version 3, 29-Apr-2005, William Thompson, GSFC
;			Added projections AIT,CAR,CEA,CYP,MER,MOL,PAR,SFL
;               Version 4, 03-Jun-2005, William Thompson, GSFC
;                       Added projections ARC,BON,COD,COE,COO,COP,STG,SZP,ZEA
;               Version 5, 08-Jun-2005, William Thompson, GSFC
;                       Add support for spectral projections
;               Version 6, 21-Dec-2005, William Thompson, GSFC
;                       Added projections AIR, CSC, PCO, QSC, TSC, ZPN
;                       Added keywords TOLERANCE, MAX_ITER
;               Version 7, 12-Oct-2006, William Thompson, GSFC
;                       Added support for pixel lists.
;               Version 8, 28-Mar-2013, WTT, Added keywords NOWRAP, POS_LONG
;               Version 9, 28-Jun-2019, WTT, Handle distortion keywords
;               Version 10,  9-May-2023, WTT, add calls to WCS_APPLY_DIST_TABLE
;               Version 11, 11-May-2023, WTT, apply SOLARNET distortion for
;                       binary tables.
;               PUNCH_WCS... 30/10/24, incorporate distortion table
;               	handling for PUNCH (SJT)
;
; Contact     :	WTHOMPSON
;-
;
function punch_wcs_get_coord, wcs, pixels, xhdr = xhdr, yhdr = yhdr, $
                              xtable = table, ytable = ytable, $
                              quick = quick, relative = relative, $ 
                              force_proj = force_proj, missing = $
                              missing, $
                              tolerance = tolerance, max_iter = max_iter, $
                              nowrap = nowrap, $
                              pos_long = pos_long, nodistortion = $
                              nodistortion 
  on_error, 2
;
  if not valid_wcs(wcs) then message, 'Input not recognized as WCS structure'
;
;  Determine whether or not the distortion correction should be
;  applied. N.B. If a distortion lookup table is provided, then other 
;  distortion corrections will be ignored
;
  apply_dist = tag_exist(wcs, 'DISTORTION') && ~keyword_set(nodistortion) $
     && ~keyword_set(xdistort) && ~keyword_set(ydistort)
;
;  Collect the ASSOCIATE and APPLY values, if applicable.
;
  associate = 0
  apply = 0
  if apply_dist then if tag_exist(wcs.distortion, 'associate') then begin
     associate = wcs.distortion.associate
     apply = wcs.distortion.apply
  endif
;
;  Calculate the indices for each dimension, relative to the reference pixel.
;
  naxis = wcs.naxis
  n_axis = n_elements(naxis)
  if wcs.projection eq 'PIXEL-LIST' then begin
     n_axis_save = n_axis
     naxis = naxis[0]
     n_axis = 1
  endif
;
;  If PIXELS was passed, then use those coordinates.  If too few axes were
;  specified, assume 0 for the trailing dimensions.  This is intended for cases
;  where the WCS has trailing degenerate axes of size 1.
;
  if n_elements(pixels) gt 0 then begin
     sz = size(pixels)
     if sz[0] eq 0 then begin
        n_pixel_axes = 1
        num_elements = 1
     end else begin
        if wcs.projection eq 'PIXEL-LIST' then n_pixel_axes = n_axis else $
           n_pixel_axes = sz[1]
        num_elements = sz[sz[0]+2] / n_pixel_axes
        dim = sz[1:sz[0]]
     endelse
     if n_pixel_axes gt n_axis then message, $
        'PIXELS contains too many axes in its first dimension'
     coord = make_array(dimension = [n_axis, num_elements], /double)
     pixels = reform(pixels, [n_pixel_axes, num_elements], /overwrite)
     for i = 0, n_axis-1 do begin
        if i lt n_pixel_axes then begin
           temp = pixels[i, *]
           if wcs.projection eq 'PIXEL-LIST' then temp = temp-1
        end else temp = 0
        coord[i, *] = temp
     endfor
     if sz[0] eq 0 then pixels = pixels[0] else $
        pixels = reform(pixels, dim, /overwrite)
;
;  Otherwise, create a pixel index array for the entire data array.
;
  end else begin
     num_elements = product(long64(naxis))
     index = dindgen(num_elements)
     coord = make_array(dimension = [n_axis, num_elements], /double)
     nn = 1.d0
     for i = 0, n_axis-1 do begin
        coord[i, *] = long(index / nn) mod naxis[i]
        nn = nn * naxis[i]
     endfor
  endelse
  if apply_dist then pixels1 = coord
;
;  If applicable, then apply the prior distortion correction.
;
  if apply_dist then begin
     wcs_apply_distortion, wcs, coord, /prior
     wcs_apply_dist_table, wcs, pixels1, coord, /prior
  endif

; Otherwise, if distortion tables are supplied use them.

  if keyword_set(xhdr) && keyword_set(xtable) && $
     keyword_set(yhdr) && keyword_set(ytable) then begin

     x = pixels[0, *]
     y = pixels[1, *]
     
     cdelt1 = sxpar(xhdr, 'CDELT*')
     crval1 = sxpar(xhdr, 'CRVAL*')
     crpix1 = sxpar(xhdr, 'CRPIX*')
     xpos = (x+1-crval1[0])/cdelt1[0] + crpix1[0]
     ypos = (y+1-crval1[1])/cdelt1[1] + crpix1[1]
     dxp =  interpolate(xtable, xpos, ypos)
		
     cdelt2 = sxpar(hdis2, 'CDELT*')
     crval2 = sxpar(hdis2, 'CRVAL*')
     crpix2 = sxpar(hdis2, 'CRPIX*')
     xpos = (x+1-crval2[0])/cdelt2[0] + crpix2[0]
     ypos = (y+1-crval2[1])/cdelt2[1] + crpix2[1]		
     dyp = interpolate(imdis2, xpos, ypos)

     pixels[0, *] = x + dxp
     pixels[1, *] = y + dyp
  endif
;
;  Apply the CRPIX values.
;
  for i = 0, n_axis-1 do coord[i, *] = coord[i, *] - (wcs.crpix[i] - 1)
;
;  Apply any SOLARNET distortion corrections for step 2.  If needed, save the
;  intermediate coordinates.
;
  w0 = where((associate eq 1) and (apply eq 2), count0)
  if apply_dist and (count0 gt 0) then wcs_apply_dist_table, wcs, $
     pixels1, $
     coord, solarnet = [1, 2]
;
  w0 = where(associate eq 2, count0)
  if apply_dist and (count0 gt 0) then pixels2 = coord
;
;  If applicable, take the coordinates from a pixel list.
;
  if wcs.projection eq 'PIXEL-LIST' then begin
     coord = wcs.pixel_list[*, coord]
     n_axis = n_axis_save
     goto, reformat
  end
;
;  Calculate the intermediate (relative) coordinates.  Apply any subsequent
;  distortion corrections before applying the CDELT keywords, including any
;  SOLARNET distortion corrections for step 3.  If needed, save the
;  intermediate coordinates.
;
  if wcs.variation eq 'CD' then begin
     coord = wcs.cd # coord
     if apply_dist then begin
        wcs_apply_distortion, wcs, coord
        wcs_apply_dist_table, wcs, pixels2, coord
        w0 = where(apply eq 3, count0)
        if count0 gt 0 then begin
           for i = 1, 2 do begin
              w1 = where(associate eq i, count1)
              if count1 gt 0 then begin
                 case i of
                    1: pixelsn = pixels1
                    2: pixelsn = pixels2
                 endcase
                 wcs_apply_dist_table, wcs, pixelsn, coord, $
                                       solarnet = [i, 3]
              endif
           endfor
        endif
        w0 = where(associate eq 3, count0)
        if count0 gt 0 then pixels3 = coord
     endif
  end else begin
     coord = wcs.pc # coord
     if apply_dist then begin
        wcs_apply_distortion, wcs, coord
        wcs_apply_dist_table, wcs, pixels1, coord
        w0 = where(apply eq 3, count0)
        if count0 gt 0 then begin
           for i = 1, 2 do begin
              w1 = where(associate eq i, count1)
              if count1 gt 0 then begin
                 case i of
                    1: pixelsn = pixels1
                    2: pixelsn = pixels2
                 endcase
                 wcs_apply_dist_table, wcs, pixelsn, coord, $
                                       solarnet = [i, 3]
              endif
           endfor
        endif
        w0 = where(associate eq 3, count0)
        if count0 gt 0 then pixels3 = coord
     endif
     for i = 0, n_axis-1 do coord[i, *] = wcs.cdelt[i]*coord[i, *]
  endelse
;
;  Apply any SOLARNET distortion corrections for step 4.
;
  w0 = where(apply eq 4, count0)
  if apply_dist and (count0 gt 0) then begin
     for i = 1, 3 do begin
        w1 = where(associate eq i, count1)
        if count1 gt 0 then begin
           case i of
              1: pixelsn = pixels1
              2: pixelsn = pixels2
              3: pixelsn = pixels3
           endcase
           wcs_apply_dist_table, wcs, pixelsn, coord, solarnet = [i, 4]
        endif
     endfor
  endif
;
;  If needed, save the intermediate coordinates.
;
  w0 = where(associate eq 4, count0)
  if count0 gt 0 then pixels4 = coord
;
;  If only relative values were requested, we're done.
;
  if keyword_set(relative) then goto, reformat
;
;  Apply any tabular projections.
;
  wcs_proj_tab, wcs, coord
;
;  Add in the reference values for the non-spherical (and non-tabular)
;  coordinates, or apply the appropriate projection.
;
  for i = 0, n_axis-1 do if ((i ne wcs.ix) and (i ne wcs.iy)) or $
     (wcs.projection eq '') then begin
     if strmid(wcs.ctype[i], 4, 1) eq '-' then begin
        proj = strupcase(strmid(wcs.ctype[i], 5, 3))
        case proj of
           'LOG': wcs_proj_log, wcs, coord, i
           'TAB': dummy = 0     ;Already applied
;
           'A2F': wcs_proj_a2f, wcs, coord, i
           'A2V': wcs_proj_a2v, wcs, coord, i
           'A2W': wcs_proj_a2w, wcs, coord, i
           'F2A': wcs_proj_f2a, wcs, coord, i
           'F2V': wcs_proj_f2v, wcs, coord, i
           'F2W': wcs_proj_f2w, wcs, coord, i
           'V2A': wcs_proj_v2a, wcs, coord, i
           'V2F': wcs_proj_v2f, wcs, coord, i
           'V2W': wcs_proj_v2w, wcs, coord, i
           'W2A': wcs_proj_w2a, wcs, coord, i
           'W2F': wcs_proj_w2f, wcs, coord, i
           'W2V': wcs_proj_w2v, wcs, coord, i
;
           'GRI': wcs_proj_gri, wcs, coord, i
           'GRA': wcs_proj_gra, wcs, coord, i
;
           else: begin
              if proj ne '' then message, /informational, $
                                          'Unrecognized projection ' + proj
              coord[i, *] = coord[i, *] + wcs.crval[i]
           endcase
        endcase
     end else coord[i, *] = coord[i, *] + wcs.crval[i]
  endif
;
;  Apply the proper spherical projection, if applicable.  If unrecognized,
;  simply add the reference values.
;
  projection = strupcase(wcs.projection)
  if projection ne '' then case projection of
     'AIR': wcs_proj_air, wcs, coord, tolerance = tolerance, max_iter $
                          = max_iter
     'AIT': wcs_proj_ait, wcs, coord, missing = missing
     'ARC': wcs_proj_arc, wcs, coord, missing = missing
     'AZP': wcs_proj_azp, wcs, coord, missing = missing
     'BON': wcs_proj_bon, wcs, coord, missing = missing
     'CAR': wcs_proj_car, wcs, coord, missing = missing, quick = $
                          quick, nowrap = nowrap
     'CEA': wcs_proj_cea, wcs, coord, missing = missing, nowrap = nowrap
     'COD': wcs_proj_cod, wcs, coord, missing = missing
     'COE': wcs_proj_coe, wcs, coord, missing = missing
     'COO': wcs_proj_coo, wcs, coord, missing = missing
     'COP': wcs_proj_cop, wcs, coord, missing = missing
     'CSC': wcs_proj_csc, wcs, coord, missing = missing
     'CYP': wcs_proj_cyp, wcs, coord, missing = missing, nowrap = nowrap
     'MER': wcs_proj_mer, wcs, coord, nowrap = nowrap
     'MOL': wcs_proj_mol, wcs, coord, missing = missing
     'PAR': wcs_proj_par, wcs, coord, missing = missing
     'PCO': wcs_proj_pco, wcs, coord, missing=missing, $
                          tolerance = tolerance, $
                          max_iter = max_iter
     'QSC': wcs_proj_qsc, wcs, coord, missing = missing
     'SFL': wcs_proj_sfl, wcs, coord, missing = missing
     'STG': wcs_proj_stg, wcs, coord
     'SIN': wcs_proj_sin, wcs, coord, missing = missing
     'SZP': wcs_proj_szp, wcs, coord, missing = missing
     'TAN': wcs_proj_tan, wcs, coord, quick = quick, force_proj = force_proj
     'TSC': wcs_proj_tsc, wcs, coord, missing = missing
     'ZEA': wcs_proj_zea, wcs, coord, missing = missing
     'ZPN': wcs_proj_zpn, wcs, coord, missing=missing, $
                          tolerance = tolerance, $
                          max_iter = max_iter
     else: begin
        message, /informational, 'Unrecognized projection ' + projection
        coord[wcs.ix, *] = coord[wcs.ix, *] + wcs.crval[wcs.ix]
        coord[wcs.iy, *] = coord[wcs.iy, *] + wcs.crval[wcs.iy]
     endcase
  endcase
;
;  Apply any SOLARNET distortion corrections for steps 5 or 6, which are
;  treated as equivalent.
;
  w0 = where((apply eq 5) or (apply eq 6), count0)
  if apply_dist and (count0 gt 0) then begin
     for i = 1, 4 do begin
        w1 = where(associate eq i, count1)
        if count1 gt 0 then begin
           case i of
              1: pixelsn = pixels1
              2: pixelsn = pixels2
              3: pixelsn = pixels3
              4: pixelsn = pixels4
           endcase
           wcs_apply_dist_table, wcs, pixelsn, coord, solarnet = [i, 5]
           wcs_apply_dist_table, wcs, pixelsn, coord, solarnet = [i, 6]
        endif
     endfor
  endif
;
;  If the POS_LONG keyword was set, then remap the coordinates to be between 0
;  and 360 degrees.
;
  if keyword_set(pos_long) then begin
     onerot = 360.d0
     case wcs.cunit[wcs.ix] of
        'arcmin': onerot = onerot * 60.d0
        'arcsec': onerot = onerot * 3600.d0
        'mas':    onerot = onerot * 3600.d3
        'rad':    onerot = 2.d0 * !dpi
        else:     onerot = onerot
     endcase
     w = where(coord[wcs.ix, *] lt 0, count)
     if count gt 0 then coord[wcs.ix, w] = coord[wcs.ix, w] + onerot
  endif
;
;  If the NOWRAP keyword was set, then make sure the longitude of the reference
;  pixel matches the reference longitude.
;
  if keyword_set(nowrap) then begin
     coord0 = dblarr(n_axis)
     case projection of
        'CAR': wcs_proj_car, wcs, coord0, /nowrap
        'CEA': wcs_proj_cea, wcs, coord0, /nowrap
        'CYP': wcs_proj_cyp, wcs, coord0, /nowrap
        'MER': wcs_proj_mer, wcs, coord0, /nowrap
        else:
     endcase
     deltalon = wcs.crval[wcs.ix] - coord0[wcs.ix]
     if deltalon ne 0 then coord[wcs.ix, *] = coord[wcs.ix, *] + deltalon
  endif
;
;  Reformat the coordinate array into the proper dimensions.
;
REFORMAT:
  if n_elements(pixels) gt 0 then begin
     if num_elements gt 1 then begin
        if wcs.projection eq 'PIXEL-LIST' then newdim = [n_axis, dim] else $
           newdim = [n_axis, dim[1:sz[0]-1]]
        coord = reform(coord, newdim, /overwrite)
     end else coord = reform(coord, [n_axis], /overwrite)
  end else coord = reform(coord, [n_axis, naxis], /overwrite)
;
  return, coord
end
