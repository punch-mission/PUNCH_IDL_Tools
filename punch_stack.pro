;+
;  PUNCH_STACK
;	Read a set of punch images into a datacube one with the
;	total brightness and one with the PB/
;
; Usage:
;	punch_stack, data, hdr
;
; Arguments:
;	sdata	float	A variable to return the data stack,
;	hdr	list	A variable to return the FITS headers.
;			
;
; Keywords:
;	uncertain flt	A variable to return the uncertainty map.
;	uhdr	list	A variable to return the fits headers for the
;			uncertainty maps.
;	polar	bool	If not specified, then return both the total
;			power and pB images as an i×j×2×N array. If
;			set then only return the pB data, if set to
;			zero then only return the total power
;			data. Ignored for lower-level datasets with
;			only a single plane.
;	/use_shared	If set, use the shared library method to
;			access the compressed FITS file.
;	path	string	The path to start the search for data.
;	/select_all	If set, then select all ths fits files in
;			PATH, otherwise a dialogue is used.
;	wcs	struct	A variable to return the WCS information for
;			the images (FITSHEAD2WCS).
;	ast	struct	A variable to return thr WCS information for
;			the images (EXTAST)
;	system	string	Specify which coordinate system is in required
;			for the WCS.
;	/as_list	If set then the WCS & AST parameters are returned in
;			lists of structures rather than arrays. This
;			allows for different structures for different
;			elements. Headers are always returned as lists.
;	file_list string	Specify a list of files to
;			read. Must be all full paths or all
;			basenames. If it is a null variable then the
;			list is returned.
;	/fudge_pv2	Fudge the PV2 parameters when they have an
;			illegal value.
;	[xy]dhdr list	Variables to return the distortion headers.
;	[xy]distort dbl	Variables to return the distortion maps.
;
; History:
;	Original: 27/6/24; SJT
;	Add WCS: 28/6/24; SJT
;	Add AS_LIST & FILE_LIST: 16/7/24; SJT
;	Add uncertainties and distortions: 28/10/24; SJT
;-

pro punch_stack, sdata, hdr, uncertain = uncertain, uhdr = uhdr, $
                 polar = polar, use_shared = use_shared, $
                 path = path, select_all = select_all, wcs = wcs, $
                 system = system, ast = ast, as_list = as_list, $ $
                 file_list = file_list, fudge_pv2 = fudge_pv2, $
                 xdhdr = xdhdr, xdistort = xdistort, $
                 ydhdr = ydhdr, ydistort = ydistort

  defsysv, '!gdl', exist = isgdl
  if n_elements(use_shared) eq 0 then use_shared = ~isgdl
  
  if ~keyword_set(path) then path = './' $
  else begin
     if strpos(path, '/', /reverse_search) ne strlen(path)-1 then $
        path = path+'/'
  endelse

  if keyword_set(select_all) then begin
     flist = file_search(path+'*.fits', count = nfiles)
     if arg_present(file_list) then file_list = flist
  endif else if keyword_set(file_list) then begin
     if strpos(file_list[0], '/') eq -1 then flist = path+file_list $
     else flist = file_list
     nfiles = n_elements(flist)
  endif else begin
     flist = dialog_pickfile(filter = '*.fits', $
                               path = path, $
                               /must, $
                             /multiple)
     nfiles = n_elements(flist)
     if arg_present(file_list) then file_list = flist
  endelse
  
  if flist[0] eq '' then return
                                          
  nfdg = lonarr(2)
  for j = 0, nfiles-1 do begin
     read_punch, flist[j], index, data, data_uncert = udata, $
                 index_uncert = uhdr, use_shared = use_shared, $
                 /string_header, xdidx = xdhdr1, xdistort = xdistort1, $
                 ydidx = ydhdr1, ydistort = ydistort1

     if keyword_set(fudge_pv2) then begin
        prj = fxpar(index, 'CTYPE1')
        if strpos(prj, 'AZP') ge 0 then begin
           pv2_1 = fxpar(index, 'PV2_1')
           if pv2_1 lt 0 then begin
              fxaddpar, index,  'PV2_1', 0.d
              nfdg[0] ++
           endif
        endif
        
        prja = fxpar(index, 'CTYPE1A')
        if strpos(prja, 'AZP') ge 0 then begin
           pv2_1a = fxpar(index, 'PV2_1A')
           if pv2_1a lt 0 then begin
              fxaddpar, index,  'PV2_1A', 0.d
              nfdg[1] ++
           endif
        endif
     endif
     
     if j eq 0 then begin       ; Initializations based on requested
                                ; parameters  and properties of the
                                ; file.
        
        nd = size(data, /n_dim)
        sz = size(data, /dim)
        dtype = size(data, /type)
        if nd eq 2 then begin
           if n_elements(polar) ne 0 then $
              message, /continue, "Single-plane input file, " + $
                       "ignoring POLAR setting."
           pflag = 0
           sdata = make_array(sz[0], sz[1], nfiles, type = dtype)
        endif else if n_elements(polar) eq 0 then begin
           sdata = make_array(sz[0], sz[1], sz[2], nfiles, type = dtype)
           pflag = 2
        endif else begin
           sdata = make_array(sz[0], sz[1], nfiles, type = dtype)
           pflag = keyword_set(polar)
        endelse
        
        hdr = list(length = nfiles)

        if arg_present(wcs) then begin
           if keyword_set(as_list) then $
              wcs = list(length = nfiles) $
           else begin
              wcs0 = fitshead2wcs(index, system = system)
              wcs = replicate(wcs0, nfiles)
           endelse
        endif
        if arg_present(ast) then begin
           if keyword_set(as_list) then $
              ast = list(length = nfiles) $
           else begin
              extast, index, ast0, alt = system
              ast = replicate(ast0, nfiles)
           endelse
        endif

        if arg_present(uncertain) || arg_present(uheader) then begin
           if n_elements(udata) eq 0 then begin
              message, /continue, "No uncertainty data in input " + $
                       "file, ignoring."
              ucflag = 0b
           endif else begin
              utype = size(udata, /type)
              if pflag eq 2 then uncertain = $
                 make_array(sz[0], sz[1], sz[2], nfiles, type = utype) $
              else  uncertain = make_array(sz[0], sz[1], nfiles, type = utypee)
              uheader = list(length = nfiles)
              ucflag = 1b
           endelse
        endif else ucflag = 0b

        if arg_present(xdhdr) || arg_present(xdistort) || $
           arg_present(ydhdr) || arg_present(ydistort) then begin
           if n_elements(xdistort1) eq 0 || $
              n_elements(ydistort1) eq 0 then begin
              message, /continue, "No distortion tables in " + $
                       "input file, ignoring"
              dflag = 0b
           endif else begin
              sd = size(xdistort1, /dim)
              td = size(xdistort1, /type)
              xdistort = make_array(sd[0], sd[1], nfiles, type = td)
              ydistort = make_array(sd[0], sd[1], nfiles, type = td)
              xdhdr = list(length = nfiles)
              ydhdr = list(length = nfiles)
              dflag = 1b
           endelse
        endif else dflag = 0b
        
     endif

     case pflag of
        0: begin
           sdata[*, *, j] = data[*, *, 0]
           if ucflag then uncertain[*, *, j] = data_uncert[*, *, 0]
        end
        1: begin
           sdata[*, *, j] = data[*, *, 1]
           if ucflag then uncertain[*, *, j] = data_uncert[*, *, 1]
        end
        2: begin
           sdata[*, *, *, j] = data
           if ucflag then uncertain[*, *, *, j] = data_uncert
        end
     endcase

     if dflag then begin
        xdistort[*, *, j] = xdistort1
        ydistort[*, *, j] = ydistort1
        xdhdr[j] = xdhdr1
        ydhdr[j] = ydhdr1
     endif
     
     hdr[j] = index
     if ucflag then uhdr[j] = index_uncert
     
     if arg_present(wcs) then begin
        wcs1 =  fitshead2wcs(index, system = system)
        if keyword_set(as_list) then $
           wcs[j] = wcs1 $
        else begin
           struct_assign, wcs1, wcs0
           wcs[j] = wcs0
        endelse
     endif
     if arg_present(ast) then begin
        extast, index, ast1, alt = system
        if keyword_set(as_list) then $
           ast[j] = ast1 $
        else begin
           struct_assign, ast1, ast0
           ast[j] = ast0
        endelse
     endif
  endfor

  if keyword_set(fudge_pv2) then print, "Fudged:", nfdg
end
