;+
; FIX_Z_HEAD
;	Fix header returned by "fitsio_read_image" which correctly
;	converts the BINTABLE to an image, but does not update the
;	header to contain the appropriate NAXIS* keys etc.
;
; Usage:
;	fix_z_head, hdr
;
; Argument:
;	hdr	string	The FITS bintable header
;
; Keywords:
;	list	string	The list of keywords to update, to be supplied
;			without the leading 'Z'. Otherwise a default
;			list is used. 
;	/remove		If set, then keywords starting with Z and not
;			in the list are deleted. (This looks to
;			replicate the behaviour of "imcopy".
;	/extension	Set explicitly to zero to prevent changing the
;			XTENSION key to IMAGE
;
; Notes:
;	This is intended to try to remove the repeated conversions
;	from string to structure and back.
;
; History:
;	Original: 24/10/24; SJT
;-

pro fix_z_head, hdr, list = list, remove = remove, extension = extension

  if ~keyword_set(list) then olist = ['BITPIX', $
                                      'NAXIS', $
                                      'NAXIS*', $
                                      'BLANK', $
                                      'BZERO', $
                                      'BSCALE'] $
  else olist = strupcase(list)
  ilist = 'Z'+olist
  
  for j = 0, n_elements(ilist)-1 do begin
     val = sxpar(hdr, ilist[j], comment = pcom, count = nv,  ifound = idx)
     case nv of
        0:
        1: begin
           sxdelpar, hdr, ilist[j]
           fxaddpar, hdr, olist[j], val, pcom
        end
        else: begin
           istem = strmid(ilist[j], 0, strlen(ilist[j])-1)
           ostem = strmid(olist[j], 0, strlen(olist[j])-1)
           for k = 0, nv-1 do begin
              sxdelpar, hdr, istem+string(idx[k], format = "(i0)")
              fxaddpar, hdr, ostem+string(idx[k], format = "(i0)"), $
                        val[k], pcom[k]
           endfor
        end
     endcase
  endfor

  if keyword_set(remove) then begin
     locs = where(strpos(hdr, 'Z') eq 0, nz)
     if nz ne 0 then print, hdr[locs]
     for j =  nz -1, 0, -1 do begin
        ss = strsplit(hdr[locs[j]], '=', /extr)
        print, ss[0]
        sxdelpar, hdr, strtrim(ss[0])
     endfor
  endif

  if n_elements(extension) eq 0 || keyword_set(extension) then $
     fxaddpar, hdr, 'XTENSION', 'IMAGE'

end
