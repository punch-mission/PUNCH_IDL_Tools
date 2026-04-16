;+
; NAME:
;  PUNCH_CB_POSITIONS
;
; PURPOSE:
;  Helper function for colorbar generation in PUNCH_IMAGE
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  punch_cb_positions, cb_vals, steps
;  
; INPUTS:
;  colorbar physical values from PUNCH_POWERNORM_COLOBAR_TICKS and number of desired steps in colorbar
;
; OPTIONAL INPUTS:
;  None at this time
;
; KEYWORD IMPUTS:
;  None at this time
;
; RETURNS:
;  steps x 2-element array with locations in color table of reference and the corresponding value
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-

function punch_cb_positions, cb_vals, steps

	;; figure out correct step size for evenly spaced colorbar values
	steps = double(steps)
	step_size = ( max(cb_vals) - min(cb_vals) ) / (steps - 1D)
	step_values = (dindgen(steps) * step_size) + min(cb_vals)

	xarr = findgen(n_elements(cb_vals))
	cb_positions = interpol(xarr, cb_vals, step_values, /spline)

	return, [[cb_positions], [step_values]]

end