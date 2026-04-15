;+
; NAME:
;  PUNCH_LCT
;
; PURPOSE:
;  Load PUNCH Color Table
;
; CATEGORY:
;  PUNCH-mission/PUNCH_IDL_TOOLS
;
; CALLING SEQUENCE:
;  punch_lct, r, g, b, load = load
;  
; INPUTS:
;  none
;
; OPTIONAL INPUTS:
;  None at this time
;
; KEYWORD IMPUTS:
;  LOAD: Directly loads the color table into memory
;
; RETURNS:
;  PUNCH color table in r, g, b format (256-element byte arrays) for use in TVLCT, or direct loads on request
;
; MODIFICATON HISTORY:
;  Created: 2026 April 15, Dan Seaton, SwRI
;
;  See PUNCH-mission/PUNCH_IDL_Tools repo for history details.
;-


pro punch_lct, r, g, b, load = load

	load = keyword_set(load)

	cmap = punch_get_cmap()

	r = cmap[*, 0]
	g = cmap[*, 1]
	b = cmap[*, 2]

	if load then tvlct, r, g, b

end