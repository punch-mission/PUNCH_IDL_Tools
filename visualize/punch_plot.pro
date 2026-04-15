;+
; NAME:
;  PUNCH_PLOT
;
; PURPOSE:
;  Display PUNCH Data with reasonable scaling and correct color tables using PLOT_IMAGE procedure
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  punch_plot, data, normalization = normalization, vmin = vmin, vmax = vmax, $
;	ctable = ctable, gamma_value = gamma_value
;  
; INPUTS:
;  data: PUNCH image to display
;
; OPTIONAL INPUTS:
;  normalization: display normalization strategy ('PowerNorm', 'LogNorm', 'None'; default is 'PowerNorm')
;
;  vmin: minimum brightness to display (default is 1e-14)
;
;  vmax: maximum brightness to display (default is 1e-12)
;
;  ctable: color table to display ('PUNCH' or IDL Color Table number; default is 'PUNCH')
; 			(TODO: Add support for custom color tables.)
;
;  gamma_value: gamma value to use for PowerNorm scaling (default is 1/2.2)
;
; KEYWORD IMPUTS:
;  None at this time
;
; RETURNS:
;  Nothing
;
; LIMITATIONS:
;  Many! Need to add better support for customization, need to add support for projection and coordinates,
;      need to add support for colorbars. 
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;
; NOTES:
;  Original is based on PUNCH_IMAGE function.
;-


pro punch_plot, data, normalization = normalization, vmin = vmin, vmax = vmax, $
	ctable = ctable, gamma_value = gamma_value

	;; set up
	if ~keyword_set(normalization) then normalization = 'powernorm'
	if ~keyword_set(vmin) then vmin = 1e-14
	if ~keyword_set(vmax) then vmax = 1e-12
	if ~keyword_set(gamma_value) then gamma_value = 1/2.2
	if ~keyword_set(ctable) then ctable = 'punch'

	;; simplify checking normalization approach
	normalization = normalization.tolower()

	;; normalize the data if required
	case normalization of
		'powernorm': normalized_data = punch_powernorm(data, vmin, vmax, gamma_value)
		'lognorm': normalized_data = punch_lognorm(data, vmin, vmax)
		else: normalized_data = (data > vmin) < vmax
	endcase

	;; load a color table on request
	tvlct, r_orig, g_orig, b_orig, /get
	if (var_type(ctable) ne 0) and (var_type(ctable) ne 7) and (var_type(ctable) ne 8) then begin
		if ctable le 74 then loadct, ctable $
		else message, 'Unknown color table', /info
	endif else begin
		if ctable eq 'punch' then punch_lct, /load $
		else message, 'Unknown color table', /info
	endelse

	;; plot the normalized data
	plot_image, normalized_data

	tvlct, r_orig, g_orig, b_orig

end



