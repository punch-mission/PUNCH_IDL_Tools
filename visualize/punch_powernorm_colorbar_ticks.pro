;+
; NAME:
;  PUNCH_POWERNORM_COLORBAR_TICKS
;
; PURPOSE:
;  Helper function for colorbar generation in PUNCH_IMAGE
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  punch_powernorm_colorbar_ticks, vmin, vmax, gamma_value
;  
; INPUTS:
;  scaling values for PowerNorm display from PUNCH_IMAGE
;
; OPTIONAL INPUTS:
;  None at this time
;
; KEYWORD IMPUTS:
;  None at this time
;
; RETURNS:
;  De-normalized olor table physical values (256-element array)
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-


function punch_powernorm_colorbar_ticks, vmin, vmax, gamma_value

	cb_norm = dindgen(256)/255.
	cb_vals = cb_norm^(1/gamma_value) * (vmax - vmin) + vmin

	return, cb_vals
end