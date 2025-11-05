pro punch_ad2xy, a, d, astr, x, y, xhdr = xhdr, yhdr = yhdr, xtable = $
                 xtable, ytable = ytable
;+
; NAME:
;     AD2XY
; PURPOSE:
;     Compute X and Y from native coordinates and a FITS  astrometry structure
;		This routine which works with externally-passed
;		distortion tables, is now deprecated. Use
;		FITSHEAD2WCS with the FILENAME keyword, or
;		WCS_APPEND_TABLES to merge the tables into the WCS
;		structure. SJT: 3/11/25.
;-

 compile_opt idl2
 common broyden_coeff, xcoeff, ycoeff
 
 print, "punch_ad2xy, is now deprecated, use fitshead2wcs & wcs_append_tables"
 print, "to use the distortion tables."
 
 if N_params() lT 4 then begin
        print,'Syntax -- AD2XY, a, d, astr, x, y'
        return
 endif

 Catch, theError
 IF theError NE 0 then begin
     Catch,/Cancel
     void = cgErrorMsg(/quiet)
     RETURN
     ENDIF

  if tag_exist(astr,'DISTORT') && ((astr.distort.name EQ 'TPV') || (astr.distort.name EQ 'TNX')) then $
  ctype = strmid(astr.ctype,0,4) + '-TAN' else ctype = astr.ctype 
 crval = astr.crval

 testing = 0B
 size_a = SIZE(a)
 ndima = size_a[0]

 astr2 = TAG_EXIST(astr,'AXES') ; version 2 astrometry structure
 IF astr2 THEN reverse = astr.reverse ELSE BEGIN
     coord = strmid(ctype,0,4)
     reverse = ((coord[0] EQ 'DEC-') && (coord[1] EQ 'RA--')) || $
               ((coord[0] EQ 'GLAT') && (coord[1] EQ 'GLON')) || $
               ((coord[0] EQ 'ELAT') && (coord[1] EQ 'ELON'))
 ENDELSE
 if reverse then crval = rotate(crval,2)        ;Invert CRVAL?
     
  spherical = strmid(astr.ctype[0],4,1) EQ '-'
  if spherical then begin
      IF astr2 THEN BEGIN
          cylin = WHERE(astr.projection EQ ['CYP','CAR','MER','CEA','HPX'],Ncyl)
          IF Ncyl GT 0 THEN BEGIN	
	  testing = 1
          size_d = SIZE(d)
          ndimd = size_d[0]
          IF ndima GT 1 THEN a = REFORM(a, size_a[ndima+2], /OVERWRITE)
          IF ndimd GT 1 THEN d = REFORM(d, size_d[ndimd+2], /OVERWRITE)
          a0 = [a, 0d0,180d0]  & d0 = [d, 0d0, 0d0] ; test points
          wcssph2xy, a0, d0, xsi, eta, CTYPE = ctype, PV1 = astr.pv1, $
              PV2 = astr.pv2, CRVAL = crval, CRXY = astr.x0y0 
	  ENDIF ELSE BEGIN
	  pv1 = astr.pv1
	  pv2 = astr.pv2
          if tag_exist(astr,'DISTORT') then begin
	      if (astr.distort.name EQ 'TPV') || (astr.distort.name EQ 'SIP') then begin 
	           pv1 = [0.0d,0,90.0d,180d,90d]    ;Tangent projection
	           pv2 = [0.0,0.0]
	      ENDIF 
	      ENDIF
          wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PV1 = pv1, $
              PV2 = pv2, CRVAL = crval, CRXY = astr.x0y0 
	   ENDELSE
      ENDIF ELSE wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PV2 = astr.pv2, $
        LONGPOLE = astr.longpole, CRVAL = crval, LATPOLE = astr.latpole
  endif else begin
        xsi = a - crval[0] & eta = d - crval[1]
  endelse	
  cd = astr.cd
  cdelt = astr.cdelt

  cd[0,0] *= cdelt[0] & cd[0,1] *= cdelt[0]
  cd[1,1] *= cdelt[1] & cd[1,0] *= cdelt[1]
     
 if reverse then begin
     temp = TEMPORARY(xsi) &  xsi = TEMPORARY(eta) & eta = TEMPORARY(temp)
 endif

   if tag_exist(astr,'DISTORT') && (astr.distort.name EQ 'TPV') then begin
            ctype = strmid(astr.ctype,0,4) + '-TAN'
            xcoeff = astr.pv1
	    ycoeff = astr.pv2
	    x0 = xcoeff[0]
	     y0 = ycoeff[0]  
	    for i=0, N_elements(xsi)-1 do begin	
	      	xcoeff[0] = x0 - xsi[i]
	     	ycoeff[0] = y0 - eta[i]    
	      	res = broyden([xsi[i],eta[i]], 'TPV_EVAL' )	     
	      	xsi[i] = res[0]
	      	eta[i] = res[1]
	      endfor
       ENDIF
   if tag_exist(astr,'DISTORT') && (astr.distort.name EQ 'TNX') then begin
            ctype = strmid(astr.ctype,0,4) + '-TAN'
            xcoeff = astr.distort.lngcor
	    ycoeff = astr.distort.latcor
            x0 = xcoeff.coeff[0]
            y0 = ycoeff.coeff[0]  
	    for i=0, N_elements(xsi)-1 do begin	
	      	xcoeff.coeff[0] = x0 - xsi[i]
	     	ycoeff.coeff[0] = y0 - eta[i]    
	      	res = broyden([xsi[i],eta[i]], 'TNX_EVAL' )	     
	      	xsi[i] = res[0]
	      	eta[i] = res[1]
	      endfor
       ENDIF

 crpix = astr.crpix - 1
 
 cdinv = invert(cd)
 x = ( cdinv[0,0]*xsi + cdinv[0,1]*eta  )
 y = ( cdinv[1,0]*TEMPORARY(xsi) + cdinv[1,1]*TEMPORARY(eta)  )

 if tag_exist(astr,'DISTORT') && ( astr.distort.name EQ 'SIP') then begin
           distort  = astr.distort
           ap = distort.ap
           bp = distort.bp
           na = ((size(ap,/dimen))[0])
; If reverse SIP coefficients are not supplied we iterate on the forward 
; coefficients (using BROYDEN).	   
           if na LE 1 then begin     
	      xcoeff = distort.a
	      ycoeff = distort.b	
	      x0 = xcoeff[0]
	      y0 = ycoeff[0]  
	      for i=0, N_elements(x)-1 do begin	
	      	xcoeff[0] = x0 - x[i]
	     	ycoeff[0] = y0 - y[i]    
	      	res = broyden([x[i],y[i]], 'SIP_EVAL' )	     
	      	x[i] = res[0]
	      	y[i] = res[1]
	      endfor
	   endif else begin   
           xdif1 = x
           ydif1 = y	   
           for i=0,na-1 do begin
               for j=0,na-1 do begin
                  if ap[i,j] NE 0.0 then xdif1 += x^i*y^j*ap[i,j]            
                  if bp[i,j] NE 0.0 then ydif1 += x^i*y^j*bp[i,j]
           endfor
           endfor

           x = xdif1
           y = ydif1
         ENDELSE
 ENDIF

 x += crpix[0] 
 y += crpix[1] 


; If distortion tables are supplied, applay the AFTER the baseline
; transformation
 
 
  if keyword_set(xhdr) && keyword_set(xtable) && $
     keyword_set(yhdr) && keyword_set(ytable) then begin

     cdelt1 = sxpar(xhdr, 'CDELT*')
     crval1 = sxpar(xhdr, 'CRVAL*')
     crpix1 = sxpar(xhdr, 'CRPIX*')
     xpos = (x-crval1[0])/cdelt1[0] + crpix1[0]
     ypos = (y-crval1[1])/cdelt1[1] + crpix1[1]
     dxp =  interpolate(xtable, xpos, ypos)

     cdelt2 = sxpar(hdis2, 'CDELT*')
     crval2 = sxpar(hdis2, 'CRVAL*')
     crpix2 = sxpar(hdis2, 'CRPIX*')
     xpos = (x-crval2[0])/cdelt2[0] + crpix2[0]
     ypos = (y-crval2[1])/cdelt2[1] + crpix2[1]		
     dyp = interpolate(imdis2, xpos, ypos)

     x = x - dxp
     y = y - dyp
  endif
  
; Check for wrapping in cylindrical projections: since the same phi
; appears at regular intervals in (x,y), depending on the location of
; the reference point on the pixel grid, some of the returned pixel 
; values may be offset by 360 degrees from the ones we want.
;
; The pixel grid may be rotated relative to intermediate world coords, 
; so the offset may have both x and y components in pixel space. 
;
; Doesn't try if native and astronomical poles are misaligned
; as this fix doesn't work in that case.

 IF testing THEN BEGIN
     npt = N_ELEMENTS(a)
     x0 = x[npt:npt+1] & y0 = y[npt:npt+1]
     x  = x[0:npt-1]   & y  = y[0:npt-1]
     
         crval = astr.crval
         IF astr.reverse THEN crval = REVERSE(crval)
         WCS_GETPOLE, crval, astr.pv1[3]-astr.pv1[1], astr.pv1[2], $
                      alpha_p, delta_p, $
                      LATPOLE = astr.pv1[4], AT_POLE = at_pole
         IF at_pole THEN BEGIN
             naxis = astr.naxis
             offmap = WHERE(x LT 0 OR y LT 0 OR $
                            x GT naxis[0] OR y GT naxis[1], noff)          
             IF offmap[0] NE -1 THEN BEGIN
                                 ; 360 degree shift
                 x360 = 2d0*(x0[1] - x0[0])
                 y360 = 2d0*(y0[1] - y0[0])
                 IF x360 LT 0 THEN BEGIN
                     x360 *= -1d0
                     y360 *= -1d0
                 ENDIF
                 xshift = x360 NE 0d0
                 yshift = y360 NE 0d0
                             ; Figure out which direction shift is
                 IF xshift THEN BEGIN
                     IF (MIN(x[offmap],/NAN) LT 0) THEN BEGIN
                         x[offmap] += x360
                         IF yshift THEN y[offmap] += y360
                     ENDIF ELSE IF MAX(x[offmap],/NAN) GT naxis[0] THEN BEGIN
                         x[offmap] -= x360
                         IF yshift THEN y[offmap] -= y360
                     ENDIF
                 ENDIF ELSE BEGIN
                     IF y360 LT 0 THEN BEGIN
                         x360 *= -1d0
                         y360 *= -1d0
                     ENDIF
                     IF (MIN(y[offmap],/NAN) LT 0) THEN BEGIN
                         IF xshift THEN x[offmap] += x360
                         y[offmap] += y360
                     ENDIF ELSE BEGIN
                         IF xshift THEN x[offmap] -= x360
                         y[offmap] -= y360
                     ENDELSE
                 ENDELSE
             ENDIF
         ENDIF
     ENDIF
 

 IF ndima GT 1 THEN BEGIN
    a = REFORM(a, size_a[1:ndima], /OVERWRITE)
    d = REFORM(d, size_a[1:ndima], /OVERWRITE)
    x = REFORM(x, size_a[1:ndima], /OVERWRITE)
    y = REFORM(y, size_a[1:ndima], /OVERWRITE)
 ENDIF ELSE if ndima EQ 0 THEN BEGIN
    a = a[0]
    d = d[0]
    x = x[0]
    y = y[0]
 ENDIF
 
 return
 end
