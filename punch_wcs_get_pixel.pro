;+
; Project     :	STEREO
;
; Name        :	PUNCH_WCS_GET_PIXEL()
;
; Purpose     :	Inverse of PUNCH_WCS_GET_COORD()
;
; Category    :	FITS, Coordinates, WCS
;
; Explanation :	This procedure takes a WCS structure, and converts coordinates
;               back into IDL pixel positions.
;
; Syntax      :	Pixels = PUNCH_WCS_GET_PIXEL( WCS, COORD )
;
; Examples    :	Pixels = PUNCH_WCS_GET_PIXEL( WCS, [0,0] )
;
;               Returns the pixel coordinate of Sun-center for a 2D image in
;               helioprojective-cartesian coordinates.
;
; Inputs      :	WCS   = Structure from FITSHEAD2WCS
;
;		COORD = An array of coordinates to return pixel locations for.
;			The first dimension must be the number of axes.
;
; Opt. Inputs :	
;
; Outputs     :	The result of the function is the array of IDL pixel locations,
;		where the first dimension steps through each coordinate axis.
;
;               Note that IDL pixel locations differ by 1 from FITS pixels,
;               e.g. IDL pixel 0 is FITS pixel 1, etc.
;
; Opt. Outputs:	None.
;
; Keywords    :	XHDR, YHDR = FITS headers for X & Y distortion tables.
; 		XTABLE, YTABLE = X & Y distortion tables.
; 		QUICK      = Depending on the projection, using /QUICK selects
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
;                            Not-A-Number (NaN) values.
;
;               TOLERANCE  = Convergence tolerance for reiterative technique
;                            used by the MOL projection.
;
;               MAX_ITER   = Maximum number of iterations for the MOL
;                            projection.
;
;               TOLERANCE and MAX_ITER are also used for inverting distortion
;               corrections, in which case the defaults are 1E-3 (pixels) and
;               100 respectively.  See WCS_INV_PROJ_MOL for the defaults when
;               applied to the MOL projection.  Depending on the projection,
;               these keywords may also be used for the forward projection, in
;               which case the 
;
;               NODISTORTION = If set, then don't apply any distortion keywords.
;
; Calls       :	VALID_WCS, PRODUCT
;
; Common      :	None.
;
; Restrictions:	None.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 03-Jun-2005, William Thompson, GSFC
;               Version 2, 08-Jun-2005, William Thompson, GSFC
;                       Add support for spectral projections
;               Version 3, 21-Dec-2005, William Thompson, GSFC
;                       Added projections AIR,CSC,MOL,PCO,QSC,TSC,ZPN
;                       Added keywords TOLERANCE, MAX_ITER
;               Version 4, 03-Jul-2019, WTT, Handle distortion keywords
;               PUNCH_WCS... 30/10/24, incorporate distortion table
;               	handling for PUNCH (SJT)
;
; Contact     :	WTHOMPSON
;-
;
function punch_wcs_get_pixel, wcs, coord, xhdr = xhdr, yhdr = yhdr, $
                              xtable = xtable, ytable = ytable, $
                              quick = quick, force_proj = force_proj, $
                              missing = missing, tolerance = tolerance, $
                              max_iter = max_iter, nodistortion = nodistortion
  on_error, 2
;
  if n_params() ne 2 then message, $
     'Syntax: Result = PUNCH_WCS_GET_PIXEL(WCS, COORD)'
  if not valid_wcs(wcs) then $
     message, 'Input not recognized as WCS structure'
;
;  Determine whether or not the distortion correction should be
;  applied. N.B. If a distortion lookup table is provided, then other
;  distortion corrections will be ignored

  apply_dist = tag_exist(wcs, 'DISTORTION') && ~keyword_set(nodistortion) $
     && ~keyword_set(xdistort) && ~keyword_set(ydistort)

;  Calculate the indices for each dimension, relative to the reference pixel.
;
  naxis = wcs.naxis
  n_axis = n_elements(naxis)
;
;  If too few axes were specified, assume the reference value for the trailing
;  dimensions.  This is intended for cases where the WCS has trailing
;  degenerate axes of size 1.
;
  sz = size(coord)
  if sz[0] eq 0 then begin
     n_pixel_axes = 1
     n_elements = 1
  end else begin
     n_pixel_axes = sz[1]
     n_elements = sz[sz[0]+2] / n_pixel_axes
     dim = sz[1:sz[0]]
  endelse
  if n_pixel_axes gt n_axis then message, $
     'COORD contains too many axes in its first dimension'
  pixels = make_array(dimension = [n_axis, n_elements], /double)
  coord = reform(coord, [n_pixel_axes, n_elements], /overwrite)
  for i = 0, n_axis-1 do begin
     if i lt n_pixel_axes then temp = coord[i, *] else temp = wcs.crval[i]
     pixels[i, *] = temp
  endfor
  if sz[0] eq 0 then coord = coord[0] else $
     coord = reform(coord, dim, /overwrite)
;
;  Apply the proper spherical projection.  If unrecognized, simply subtract
;  the reference values.
;
  projection = strupcase(wcs.projection)
  if projection ne '' then case projection of
     'AIR': wcs_inv_proj_air, wcs, pixels
     'AIT': wcs_inv_proj_ait, wcs, pixels
     'ARC': wcs_inv_proj_arc, wcs, pixels
     'AZP': wcs_inv_proj_azp, wcs, pixels
     'BON': wcs_inv_proj_bon, wcs, pixels
     'CAR': wcs_inv_proj_car, wcs, pixels, quick = quick
     'CEA': wcs_inv_proj_cea, wcs, pixels
     'COD': wcs_inv_proj_cod, wcs, pixels
     'COE': wcs_inv_proj_coe, wcs, pixels
     'COO': wcs_inv_proj_coo, wcs, pixels
     'COP': wcs_inv_proj_cop, wcs, pixels
     'CSC': wcs_inv_proj_csc, wcs, pixels
     'CYP': wcs_inv_proj_cyp, wcs, pixels
     'MER': wcs_inv_proj_mer, wcs, pixels
     'MOL': wcs_inv_proj_mol, wcs, pixels, tolerance = tolerance, $
                              max_iter = max_iter
     'PAR': wcs_inv_proj_par, wcs, pixels
     'PCO': wcs_inv_proj_pco, wcs, pixels
     'QSC': wcs_inv_proj_qsc, wcs, pixels
     'SFL': wcs_inv_proj_sfl, wcs, pixels
     'STG': wcs_inv_proj_stg, wcs, pixels
     'SIN': wcs_inv_proj_sin, wcs, pixels
     'SZP': wcs_inv_proj_szp, wcs, pixels
     'TSC': wcs_inv_proj_tsc, wcs, pixels
     'TAN': wcs_inv_proj_tan, wcs, pixels, quick = quick, force_proj = force_proj
     'ZEA': wcs_inv_proj_zea, wcs, pixels
     'ZPN': wcs_inv_proj_zpn, wcs, pixels
     else: begin
        message, /informational, 'Unrecognized projection ' + projection
        pixels[wcs.ix, *] = pixels[wcs.ix, *] - wcs.crval[wcs.ix]
        pixels[wcs.iy, *] = pixels[wcs.iy, *] - wcs.crval[wcs.iy]
     endcase
  endcase
;
;  Subtract the reference values for the non-spherical (and non-tabular)
;  coordinates, or apply the appropriate de-projection.
;
  for i = 0, n_axis-1 do if ((i ne wcs.ix) and (i ne wcs.iy)) or $
     (wcs.projection eq '') then begin
     if strmid(wcs.ctype[i], 4, 1) eq '-' then begin
        proj = strupcase(strmid(wcs.ctype[i], 5, 3))
        case proj of
           'LOG': wcs_inv_proj_log, wcs, pixels, i
           'TAB': dummy = 0     ;Applied below
;
           'A2F': wcs_inv_proj_a2f, wcs, pixels, i
           'A2V': wcs_inv_proj_a2v, wcs, pixels, i
           'A2W': wcs_inv_proj_a2w, wcs, pixels, i
           'F2A': wcs_inv_proj_f2a, wcs, pixels, i
           'F2V': wcs_inv_proj_f2v, wcs, pixels, i
           'F2W': wcs_inv_proj_f2w, wcs, pixels, i
           'V2A': wcs_inv_proj_v2a, wcs, pixels, i
           'V2F': wcs_inv_proj_v2f, wcs, pixels, i
           'V2W': wcs_inv_proj_v2w, wcs, pixels, i
           'W2A': wcs_inv_proj_w2a, wcs, pixels, i
           'W2F': wcs_inv_proj_w2f, wcs, pixels, i
           'W2V': wcs_inv_proj_w2v, wcs, pixels, i
;
           'GRI': wcs_inv_proj_gri, wcs, pixels, i
           'GRA': wcs_inv_proj_gra, wcs, pixels, i
;
           else: begin
              if proj ne '' then message, /informational, $
                                          'Unrecognized projection ' + proj
              pixels[i, *] = pixels[i, *] - wcs.crval[i]
           endcase
        endcase
     end else pixels[i, *] = pixels[i, *] - wcs.crval[i]
  endif
;
;  De-apply any tabular projections.
;
  wcs_inv_proj_tab, wcs, pixels
;
;  Convert from intermediate (relative) coordinates to pixels.
;
  if wcs.variation eq 'CD' then pixels = invert(wcs.cd) # pixels else begin
     for i = 0, n_axis-1 do pixels[i, *] = pixels[i, *] / wcs.cdelt[i]
     pixels = invert(wcs.pc) # pixels
  endelse
;
;  Add in the reference pixel (minus 1 for IDL).
;
  for i = 0, n_axis-1 do pixels[i, *] = pixels[i, *] + (wcs.crpix[i] - 1)
;
;  If distortion keywords should be applied, then reiteratively correct the
;  pixels until the distortion is matched.  First set up the parameters for the
;  iteration.
;
  if apply_dist then begin
     coord0 = reform(coord, n_pixel_axes, n_elements)
;
;  Set the default pixel scale based either on CDELTi or CDi_j.
;
     if wcs.variation eq 'CD' then begin
        i = indgen(n_pixel_axes)
        delta = wcs.cd[i, i]
     end else delta = wcs.cdelt
;
;  Reiterate until the correction falls below the tolerance, or the maximum
;  number of iterations is reached.
;
     if n_elements(max_iter) ne 0 then imax_iter = max_iter else $
        imax_iter = 100
     if n_elements(tolerance) ne 0 then itolerance = tolerance else $
        itolerance = 1e-3
     i_iter = 0
     repeat begin
;
;  Get the coordinates of the current pixel position, and get the derivative at
;  that position.
;
        coord1 = wcs_get_coord(wcs, pixels, quick = quick, $
                               force_proj=force_proj, $
                               tolerance = tolerance, $
                               max_iter = max_iter)
        coord2 = wcs_get_coord(wcs, pixels+1, quick = quick, $
                               force_proj=force_proj, $
                               tolerance = tolerance, $
                               max_iter = max_iter)
        deriv = coord2 - coord1
;
;  If the derivative couldn't be calculated, then try subtracting one.
;
        w = where(total((deriv eq 0) or ~finite(deriv), 1), count)
        if count gt 0 then begin
           coord2 = wcs_get_coord(wcs, pixels[*, w]-1, quick = quick, $
                                  missing=missing, $
                                  force_proj = force_proj, $
                                  tolerance = tolerance, max_iter = max_iter)
           deriv[*, w] = coord1[*, w] - coord2
        endif
;
;  If it still can't be calculated then use the default pixel scale.
;
        w = where(total((deriv eq 0) or ~finite(deriv), 1), count)
        if count gt 0 then for i = 0, n_pixel_axes-1 do deriv[i, w] = delta[i]
;
;  Apply the correction.
;
        corr = (coord1 - coord0) / deriv
        pixels = pixels - corr
        i_iter = i_iter + 1
     endrep until (max(abs(corr), /nan) le itolerance) or (i_iter ge imax_iter)
  endif

; If distortion tables are supplied, applay the AFTER the baseline
; transformation
  
  if keyword_set(xhdr) && keyword_set(xtable) && $
     keyword_set(yhdr) && keyword_set(ytable) then begin

     x = pixels[0, *]
     y = pixels[1, *]
 
     cdelt1 = sxpar(xhdr, 'CDELT*')
     crval1 = sxpar(xhdr, 'CRVAL*')
     crpix1 = sxpar(xhdr, 'CRPIX*')
     xpos = (x-crval1[0])/cdelt1[0] + crpix1[0]
     ypos = (y-crval1[1])/cdelt1[1] + crpix1[1]
     dxp =  interpolate(xtable, xpos, ypos)

     cdelt2 = sxpar(yhdr, 'CDELT*')
     crval2 = sxpar(yhdr, 'CRVAL*')
     crpix2 = sxpar(yhdr, 'CRPIX*')
     xpos = (x-crval2[0])/cdelt2[0] + crpix2[0]
     ypos = (y-crval2[1])/cdelt2[1] + crpix2[1]		
     dyp = interpolate(ytable, xpos, ypos)

     pixels[0, *] = x - dxp
     pixels[1, *] = y - dyp
  endif

;
;  If the MISSING keyword was passed, then flag any missing pixels.  Otherwise,
;  missing pixels should already be flagged as NaN.
;
  if n_elements(missing) eq 1 then begin
     w_missing = where((coord eq missing) or (finite(pixels) eq 0), n_missing)
     if n_missing gt 0 then pixels[w_missing] = missing
  endif
;
;  Reformat the coordinate array into the proper dimensions.
;
  if n_elements(coord) gt 0 then begin
     if sz[0] le 1 then pixels = reform(pixels, [n_axis], /overwrite) else $
        pixels = reform(pixels, [n_axis, dim[1:sz[0]-1]], /overwrite)
  end else pixels = reform(pixels, [n_axis, naxis], /overwrite)
;
  return, pixels
end
