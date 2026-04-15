;+
; NAME:
;  PUNCH_POWERNORM
;
; PURPOSE:
;  Helper function for PowerNorm image scaling in PUNCH_IMAGE
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  punch_cb_positions, cb_vals, steps
;  
; INPUTS:
;  data: PUNCH image to display
;
;  vmin: minimum brightness to display (default is 1e-14)
;
;  vmax: maximum brightness to display (default is 1e-12)
;
;  gamma_value: gamma value to use for PowerNorm scaling (default is 1/2.2)
;
; OPTIONAL INPUTS:
;  None at this time
;
; KEYWORD IMPUTS:
;  None at this time
;
; RETURNS:
;  PowerNorm normalized data
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-

function punch_powernorm, data, vmin, vmax, gamma_value

	normalized_data = ( (data - vmin)/(vmax - vmin) )^gamma_value
	normalized_data = (normalized_data > 0) < 1

	return, normalized_data

end
