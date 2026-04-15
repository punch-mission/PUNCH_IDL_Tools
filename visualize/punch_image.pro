;+
; NAME:
;  PUNCH_IMAGE
;
; PURPOSE:
;  Display PUNCH Data with reasonable scaling and correct color tables using IDL's image function.
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  im = PUNCH_IMAGE(data, normalization = normalization, vmin = vmin, vmax = vmax, $
;					ctable = ctable, gamma_value = gamma_value, dimension = dimension, $
;					draw_colorbar = draw_colorbar, cb_steps = cb_steps, cb_position = cb_position, _extra = ex)
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
;  dimension: dimensions (px) of the displayed image
;
;  cb_steps: number of steps to indicate in the colorbar, if colorbar is requested (default is 5)
;
;  cb_position: location of colorbar in normalized coordinates, if colobar is requested
;
;  extra: pass additional keywords to the image call
;
; KEYWORD IMPUTS:
;  draw colorbar: if selected, will draw the colobar
;
; OUTPUTS:
;  im: image object to make additional adjustments
;
; LIMITATIONS:
;  Many! Need to add better support for customization, need to add support for projection and coordinates,
;      need to add support for colorbars for LogNorm and None image normalization options. 
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-

function punch_image, data, normalization = normalization, vmin = vmin, vmax = vmax, $
	ctable = ctable, gamma_value = gamma_value, dimension = dimension, $
	draw_colorbar = draw_colorbar, cb_steps = cb_steps, cb_position = cb_position, _extra = ex

	;; set up
	if ~keyword_set(normalization) then normalization = 'powernorm'
	if ~keyword_set(vmin) then vmin = 1e-14
	if ~keyword_set(vmax) then vmax = 1e-12
	if ~keyword_set(gamma_value) then gamma_value = 1/2.2
	if ~keyword_set(ctable) then ctable = 'punch'
	if ~keyword_set(dimension) then dimension = [800, 800]
	draw_colorbar = keyword_set(draw_colorbar)
	if ~keyword_set(cb_steps) then cb_steps = 5
	if ~keyword_set(cb_position) then cb_position = -1

	;; simplify checking normalization approach
	normalization = normalization.tolower()

	;; normalize the data if required
	case normalization of
		'powernorm': normalized_data = punch_powernorm(data, vmin, vmax, gamma_value)
		'lognorm': normalized_data = punch_lognorm(data, vmin, vmax)
		else: normalized_data = (data > vmin) < vmax
	endcase

	;; load a color table on request -- will default to PUNCH
	tvlct, r_orig, g_orig, b_orig, /get
	if (var_type(ctable) ne 0) and (var_type(ctable) ne 7) and (var_type(ctable) ne 8) then begin
		if ctable le 74 then loadct, ctable, r, g, b $
		else message, 'Unknown color table', /info
	endif else begin
		if ctable eq 'punch' then cmap = punch_get_cmap() $
		else message, 'Unknown color table', /info
	endelse
	tvlct, r_orig, g_orig, b_orig

	;; plot the normalized data
	im = image(normalized_data, axis_style = 1, rgb_table = cmap, dimension = dimension, position = [0.08, 0.1, 0.83, 0.85], _extra = ex)

	;; draw a colorbar on request
	;; TODO: Add support for other approaches besides PowerNorm
	if draw_colorbar then begin

		case normalization of
			'powernorm': begin
			;; a bunch of calculations to get the mapping to the right positions and values
			cb_vals = punch_powernorm_colorbar_ticks(vmin, vmax, gamma_value)
			cb_val_pos = punch_cb_positions(cb_vals, cb_steps)
			cb_val_pos[*, 0] = cb_val_pos[*, 0]/255.
			cb_ticknames = STRING(cb_val_pos[*, 1], FORMAT='(E10.1)')

			;; make the colorbar
			if cb_position[0] eq -1 then begin
				image_pos = im.position
				cb_position = [image_pos[2] + 0.01, image_pos[1], image_pos[2] + 0.04, image_pos[3]]
			endif
			cb = COLORBAR(TARGET=im, orientation = 1, tickvalues = cb_val_pos[*, 0], tickname = cb_ticknames, /BORDER, $
				position = cb_position, title = 'Mean Solar Brightness (MSB)', textpos = 1)
			end
			else: message, 'Sorry, colorbars are not supported for your normalization -- yet!', /info
		endcase

	endif

	return, im

end
