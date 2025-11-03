;+
; REPAIR_PUNCH_HEAD
;	Make repairs to some PUNCH (L0 & L1) headers.
;
; Usage:
;	repair_punch_head, hdr
;
; Argument:
;	hdr	string	A FITS header to be updated.
;
; Keywords:
;	/strip		If set then simply strip out the distortion
;			information from the  header.
;	wfi	int	If specified and the header does NOT have a
;			PV2_1 field, then strip the distortion tables
;			and replace with best-estimate AZP parameters.
;
; Notes:
;	If neither keyword is given, then the header is modified so
;	that the distortion tables with be applied to the equatorial
;	astrometry information as well.
;	In all cases it is ensured that LONPOLE and LATPOLE are
;	present in the "A" system.
;	Avoids making the corrections if the header is complete.
;
; History:
;	Original: Oct 25; SJT
;-

pro repair_punch_head, hdr, wfi = wfi, strip = strip

  if keyword_set(strip) then begin
                                    ; Remove the distortion params
      sxdelpar, hdr,  ['CPDIS1', 'CPDIS2', 'DP1', 'DP2']
      sxdelpar, hdr,  ['CPDIS1A', 'CPDIS2A', 'DP1A', 'DP2A']

   endif else if keyword_set(wfi) then begin
      pv2_1 = fxpar(hdr, 'PV2_1',  count = nn)
      if nn eq 0 then begin
         print, 'Adding AZP µ term, removing tabulated distortions'
         
         mu = [0.13029194d, 0.12306140d, 0.12205994d]
         plsc = [0.02455978d, 0.02475163d, 0.02455928d]

                                ; Remove the distortion params
         sxdelpar, hdr,  ['CPDIS1', 'CPDIS2', 'DP1', 'DP2']
         sxdelpar, hdr,  ['CPDIS1A', 'CPDIS2A', 'DP1A', 'DP2A']

         fxaddpar, hdr, 'CDELT1', -plsc[wfi-1]
         fxaddpar, hdr, 'CDELT2', plsc[wfi-1]
         fxaddpar, hdr, 'CDELT1A', -plsc[wfi-1]
         fxaddpar, hdr, 'CDELT2A', plsc[wfi-1]
         
         fxaddpar, hdr, 'PV2_1', mu[wfi-1], 'AZP mu parameter', $
                   after = 'CDELT2'
         fxaddpar, hdr, 'PV2_1A', mu[wfi-1], 'AZP mu parameter', $
                   after = 'CDELT2A'
      endif else print, 'AZP already has µ, skipping'
      
   endif else begin
      cpdis1 = strtrim(fxpar(hdr, 'CPDIS1', comment = cmt1))
      cpdis2 = strtrim(fxpar(hdr, 'CPDIS2', comment = cmt2))

      if strupcase(cpdis1) ne 'LOOKUP' && $
         strupcase(cpdis2) ne 'LOOKUP' then begin
         print, "No distortion tables here, returning"
         return
      endif

      if strupcase(cpdis1) eq 'LOOKUP' then begin
                                ; Add system A

         cpdis1a = fxpar(hdr, 'CPDIS1A', count = nn)
         if nn eq 0 then begin
            print, 'Connecting X distortion table to "A" system'
            fxaddpar, hdr, 'CPDIS1A', cpdis1, cmt1+' RA&DEC', after = 'CRVAL2A'

            dp1 = fxpar(hdr, 'DP1', /multi, comment = cc1)
            for j = 0, n_elements(dp1) -1 do $
               fxaddpar, hdr, 'DP1A', dp1[j], cc1[j], /multi, after = 'CPDIS1A'
         endif else print, 'X distortion table already connected with "A" system.'
      endif

      if strupcase(cpdis2) eq 'LOOKUP' then begin
                                ; Add system A
         
         cpdis1a = fxpar(hdr, 'CPDIS2A', count = nn)
         if nn eq 0 then begin
            print, 'Connecting Y distortion table to "A" system'
            fxaddpar, hdr, 'CPDIS2A', cpdis2, cmt2+' RA&DEC', after = 'DP1A'
         
            dp2 = fxpar(hdr, 'DP2', /multi, comment = cc2)
            for j = 0, n_elements(dp1) -1 do $
               fxaddpar, hdr, 'DP2A', dp2[j], cc2[j], /multi, after = 'CPDIS2A'
         endif else print, 'Y distortion table already connected with "A" system.'
      endif
   endelse

                                ; This is needed either way.

   lpa = fxpar(hdr, 'LATPOLEA', count = nn)
   if nn eq 0 then begin
      c2a = fxpar(hdr, 'CRVAL2A')
      fxaddpar, hdr, 'LONPOLEA', 180., $
                '[deg] Native longitude of celestial pole', $
                after = 'CRVAL2A'
      fxaddpar, hdr, 'LATPOLEA', c2a, $
                '[deg] Native latitude of celestial pole', $
                after = 'LONPOLEA'
   endif
end
