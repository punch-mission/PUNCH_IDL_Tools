;+
; WCS_APPEND_TABLES
;	Append distortion lookup tables to a WCS structure.
;
; Usage:
;	wcs_append_tables, wcs, xhdr, xtable, yhdr, ytable
;
; Arguments:
;	wcs	struct	The existing WCS strcuture. Must be a
;			named variable as it is updated.
;	xhdr	string	The FITS header for the X distortion table
;	xtable	real	The X distortion table
;	yhdr	string	The FITS header for the Y distortion table
;	ytable	real	The Y distortion table
;
; Keywords:
;	status	int	Returns 1 for success, 0 for failure.
;	/force		If set, then force the addition of a
;			distortion tag when none is present.
;
; Notes:
;	Needed for PUNCH as the standard wcs_find_distortion
;	doesn't properly handle the PUNCH RICE compression.
;
; History:
;	Original: 14/10/25; SJT
;-

pro wcs_append_tables, wcs, xhdr, xtable, yhdr, ytable, status = $
                       status, force = force

  on_error, 2
  
  status = 0
  if size(wcs, /type) ne 8 then message, "WCS argument is not a " + $
                                         "structure."

  junk = where(tag_names(wcs) eq 'DISTORTION', nd)
  if nd eq 0 then begin
     if ~keyword_set(force) then message, $
        "WCS argument does not have a DISTORTION tag."

; This is actually the easiest to manage.

     dp1 = {param: ['EXTVER', 'NAXES', 'AXIS.1', 'AXIS.2'], $
            value: ['1.0', '2.0', '1.0', '2.0'], $
            cdis: 'Lookup', $
            header: xhdr, $
            table: xtable}
     
     dp2 = {param: ['EXTVER', 'NAXES', 'AXIS.1', 'AXIS.2'], $
            value: ['2.0', '2.0', '1.0', '2.0'], $
            cdis: 'Lookup', $
            header: yhdr, $
            table: ytable}

     distortion = {dp1: dp1, $
                   dp2: dp2}


     wcs = add_tag(wcs, distortion, 'distortion')
     status = 1
     
  endif else begin

                                ; In principle there could be a DQ or
                                ; DW distortion but AFAIK thay are not
                                ; used by PUNCH
     
     distortion = wcs.distortion
     dp1 = distortion.dp1
     dp2 = distortion.dp2

     if strupcase(dp1.cdis) ne 'LOOKUP' || $
        strupcase(dp2.cdis) ne 'LOOKUP'  then begin
        if ~keyword_set(force) then message, $
           "Distortion is not by table."
        
        dp1.cdis = 'Lookup'
        dp2.cdis = 'Lookup'

     endif

     dp1 = add_tag(dp1, xhdr, 'header')
     dp1 = add_tag(dp1, xtable, 'table')

     dp2 = add_tag(dp2, xhdr, 'header')
     dp2 = add_tag(dp2, xtable, 'table')

     
     distortion1 = {dp1: dp1, $
                    dp2: dp2}

     
     wcs = rem_tag(wcs, 'DISTORTION')
     wcs = add_tag(wcs, distortion1, 'distortion')

     status = 1
     
  endelse

end
  
