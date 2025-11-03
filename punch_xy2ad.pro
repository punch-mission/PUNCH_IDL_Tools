pro punch_xy2ad, x, y, astr, a, d, xhdr = xhdr, yhdr = yhdr, xtable = $
                 xtable, ytable = ytable
;+
; NAME:
;     PUNCH_XY2AD
;
; PURPOSE:
;     Compute R.A. and Dec from X and Y and a FITS astrometry structure
;		This routine which works with externally-passed
;		distortion tables, is now deprecated. Use
;		FITSHEAD2WCS with the FILENAME keyword, or
;		WCS_APPEND_TABLES to merge the tables into the WCS
;		structure. SJT: 3/11/25.
;-
  
  common Broyden_coeff, pv1, pv2 ;Needed for TPV transformation
  compile_opt idl2

  print, "punch_xy2ad, is now deprecated, use fitshead2wcs & wcs_append_tables"
  print, "to use the distortion tables."
 
  if N_params() LT 4 then begin
     print, 'Syntax -- XY2AD, x, y, astr, a, d'
     return
  endif
  
  Catch, theError
  IF theError NE 0 then begin
     Catch, /Cancel
     void = cgErrorMsg(/quiet)
     RETURN
  ENDIF

; Distortion lookup table is applied BEFORE the projection correction

  if keyword_set(xhdr) && keyword_set(xtable) && $
     keyword_set(yhdr) && keyword_set(ytable) then begin

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
     dyp = interpolate(ytable, xpos, ypos)

     xp = x + dxp
     yp = y + dyp
  endif else begin
     if keyword_set(xhdr) || keyword_set(xtable) || $
        keyword_set(yhdr) || keyword_set(ytable) then $
           message, "WARNING: To apply distortion ALL of XHDR, " + $
                    "XTABLE, YHDR and YTABLE must be present", $
                    /continue
     xp = x
     yp = y
  endelse
  
  cd = astr.cd
  crpix = astr.crpix
  cdelt = astr.cdelt 
  
  cd[0, 0] *= cdelt[0] & cd[0, 1] *= cdelt[0]
  cd[1, 1] *= cdelt[1] & cd[1, 0] *= cdelt[1]

  xdif = xp - (crpix[0]-1)            
  ydif = yp - (crpix[1]-1)
  no_PV1 = 0                    ;Set if PV1 used by TGV distortion
  
  if tag_exist(astr, 'DISTORT') && astr.distort.name EQ 'SIP' then begin
     distort  = astr.distort
     a = distort.a
     b = distort.b
     na = ((size(a, /dimen))[0])
     xdif1 = xdif
     ydif1 = ydif

     for i = 0, na-1 do begin
        for j = 0, na-1 do begin
           if a[i, j] NE 0.0 then xdif1 +=  xdif^i*ydif^j*a[i, j]            
           if b[i, j] NE 0.0 then ydif1 +=  xdif^i*ydif^j*b[i, j]
        endfor
     endfor
     xdif = TEMPORARY(xdif1)
     ydif = TEMPORARY(ydif1)
     
  ENDIF 

  astr2 = TAG_EXIST(astr, 'AXES') ; version 2 astrometry structure
  
  xsi = cd[0, 0]*xdif + cd[0, 1]*ydif ;Can't use matrix notation, in
                                ;case X and Y are vectors
  eta = cd[1, 0]*TEMPORARY(xdif) + cd[1, 1]*TEMPORARY(ydif)   

  if tag_exist(astr, 'DISTORT') && astr.distort.name EQ 'TPV' then begin
     pv1 = astr.pv1
     pv2 = astr.pv2
     result = tpv_eval( [[xsi], [eta]])
     xsi = reform( result[*, 0] )
     eta = reform( result[*, 1] )
     no_PV1 = 1
     ctype = strmid(astr.ctype, 0, 4) + '-TAN'
  endif	   

  if tag_exist(astr, 'DISTORT') && astr.distort.name EQ 'TNX' then begin
     pv1 = astr.distort.lngcor
     pv2 = astr.distort.latcor
     result = tnx_eval( [[xsi], [eta]])
     xsi = reform( result[*, 0] )
     eta = reform( result[*, 1] )
     no_PV1 = 1
     ctype = strmid(astr.ctype, 0, 4) + '-TAN'
     
  endif
  
  if tag_exist(astr, 'DISTORT') && astr.distort.name EQ 'SIP' then $
     no_PV1 = 1
  if N_elements(ctype) Eq 0 then ctype = astr.ctype
  crval = astr.crval
  IF astr2 THEN reverse = astr.reverse ELSE BEGIN
     coord = strmid(ctype, 0, 4)
     reverse = ((coord[0] EQ 'DEC-') && (coord[1] EQ 'RA--')) || $
        ((coord[0] EQ 'GLAT') && (coord[1] EQ 'GLON')) || $
        ((coord[0] EQ 'ELAT') && (coord[1] EQ 'ELON'))
  ENDELSE
  if reverse then begin
     crval = rotate(crval, 2)
     temp = TEMPORARY(xsi) & xsi = TEMPORARY(eta) & eta = TEMPORARY(temp)
  endif

  if strmid(ctype[0], 4, 1) EQ '-' then begin
     if no_PV1 then begin       ;Set default values for tangent projection
        pv1 = [0.0d, 0, 90.0d, 180.0d, 90.0d]  & pv2 = [0.0d, 0.0d]	   
     endif else begin 
        pv1 = astr.pv1
        pv2 = astr.pv2
     endelse 
     
     if astr2 THEN $
        WCSXY2SPH, xsi, eta, a, d, CTYPE = ctype[0:1], PV1 = pv1, $
                   PV2 = astr.PV2, CRVAL = crval, CRXY = astr.x0y0 $
     ELSE $ 
        WCSXY2SPH, xsi, eta, a, d, CTYPE = ctype[0:1], PV2 = pv2, $
                   LONGPOLE = astr.longpole, CRVAL = crval, LATPOLE = astr.latpole
  endif else begin
     a = crval[0] + TEMPORARY(xsi) & d = crval[1] + TEMPORARY(eta)	
  endelse

  return
end
