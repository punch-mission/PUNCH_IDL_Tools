;+
; NAME:
;  PUNCH_LOGNORM
;
; PURPOSE:
;  Helper function for LogNorm image scaling in PUNCH_IMAGE
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
; OPTIONAL INPUTS:
;  None at this time
;
; KEYWORD IMPUTS:
;  None at this time
;
; RETURNS:
;  LogNorm scaled data
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-


function punch_lognorm, data, vmin, vmax

	normalized_data = alog10( (data > vmin) < vmax )

	return, normalized_data

end
