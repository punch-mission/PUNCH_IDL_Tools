;+
; READ_PUNCH
;   Purpose: read compressed PUNCH fits files
;
;   Calling Examples:
;   
;   read_punch, tfile, index, data
;   read_punch, tfile, index, data, /use_shared_lib, /uncertainty, $
;               data_uncert=data_uncert, index_uncert=index_uncert
;
;   read_punch, './PUNCH_L3_MPM_20230704013600.fits', index, data,
;                    use_shared_lib=1 
;
;   Input Parameters:
;	tfile - file to read
;
; Output Parameters:
;
;	index - fits metadata (structure or string array) 
;	data - Total Brightness and Polarized Brighness image datacube
;             (for higher level products) 
;             Only Total Brightness for QuickPUNCH products
;
; Keywords:
;   
;   /use_shared_lib	if set use call_external interface to cfitsio
;   			shared library (instead of spawning imcopy),
;   			note this is OS dependent and possibly IDL
;   			version dependent, and has the potential to
;   			crash IDL if the shared library is not
;   			properly compiled. Also does not work
;   			(currently) on GDL.
;   shared_lib_path	string	optional keyword specifying path of
;   				shared library(with trailing slash)
;   				for fitsio.so, default is the same
;   				path as for imcopy 
;   imcopy_path	string	set an explicit path for imcopy.  By default,
;   			a version in $PATH is used if available, then in
;			the ontology path of SolarSoft.
;   /string_header	Set this explicitly to zero to convert the
;			main data and uncertainty headers to
;			structures. N.B. Distortion headers are always
;			returned as string arrays.
;   
;   Optional Outputs:
;   	data_uncert	byte	Variable to return the uncertainty map.
;   	index_uncert	struct	Variable to return the metadata for the
;   				uncertainty map
;   	xdidx		string	Variable to return the header for the
;   				X-axis distortion table
;   	xdistort	double	Variable to return the X-axis
;   				distortion table.
;   	ydidx		string	Variable to return the header for the
;   				Y-axis distortion table
;   	ydistort	double	Variable to return the Y-axis
;   				distortion table.
;
;   Notes:
;     Reads one fits file at a time. If you plan to run for a
;     directory, ensure to run in a loop. 
;     The first HDU contains primary data products while the second
;     HDU contains uncertainty information. 
;     The output generated for one fits file can be a datacube with
;     two images tB and pB (depending on level of data product). 
;   
; History:
;	Original: March '24, M Hughes
;	Restore(?) ability to use "imcopy" for GDL etc. and other
;	extensive tidying: Jun-Jul; '24, SJT
;	Allow return of distortion tables: 23/10/24; SJT
;-

pro read_punch, tfile, index, data, $
                use_shared_lib = use_shared_lib, $
                shared_lib_path = shared_lib_path, $
                data_uncert = data_uncert, $
                index_uncert = index_uncert, $
                imcopy_path = imcopy_path, $
                string_header = string_header, $
                xdidx = xdidx, xdistort = xdistort, $
                ydidx = ydidx, ydistort = ydistort

  mreadfits_header, tfile, tlh, only_tags = 'NAXIS'

  hstructs = n_elements(string_header) ne 0 && $
     ~keyword_set(string_header)
  
  if tlh.naxis eq 0 then begin
                                ; If NAXIS == 0 in the primary header,
                                ; then we have a RICE-compressed file.

     uncertainty = arg_present(data_uncert) || $
                   arg_present(index_uncert)
     
     get_distort = arg_present(xdidx) || arg_present(xdistort) || $
                   arg_present(ydidx) || arg_present(ydistort)
     
     use_shared_lib = keyword_set(use_shared_lib) || $
                      data_chk(shared_lib_path, /string)

     if use_shared_lib then begin
        defpath = ssw_bin_path('fitsio.so', found = found, /path_only, $
                               /ontology) 
  
        if keyword_set(shared_lib_path) then begin
           so_path = shared_lib_path ; user supplied
           if strpos(so_path, '/', /reverse_search) ne $
              strlen(so_path)-1 then so_path = so_path+'/'
        endif else so_path = defpath ; default path

        found = file_exist(concat_dir(so_path, 'fitsio.so')) ; verify available
                                ; for OS/ARCH
        if not found then begin
           box_message, 'fitsio/shared object requested but not available ' + $
                        'for this OS/ARCH - using imcopy' 
           use_shared_lib = 0   ; override
        endif
     endif

     
     if use_shared_lib then begin
        data = fitsio_read_image(tfile, htest, so_path = so_path)
        
        get_distort  = get_distort && sxpar(index, 'CPDIS1') eq 'LOOKUP'
        
; removing 'z' from keywords
        
        fix_z_head, htest, /remove


        if hstructs then index = fitshead2struct(htest) $
        else index = htest
        
        if uncertainty then begin
           print, 'Reading uncertainty HDU...'
           data_uncert = fitsio_read_image(tfile+'[2]', htest, $
                                           so_path = so_path) 

           fix_z_head, htest, /remove

           if hstructs then index_uncert = fitshead2struct(htest) $
           else index_uncert = htest
        endif

        if get_distort then begin
           print,  "Reading distortion tables ..."
           xdistort = fitsio_read_image(tfile+'[3]', xdidx, so_path = $
                                        so_path)
           ydistort = fitsio_read_image(tfile+'[4]', ydidx, so_path = $
                                        so_path)

           fix_z_head, xdidx, /remove
           fix_z_head, ydidx, /remove
        endif
        
     endif else begin
                                ; Find IMCOPY

; Explicit first
        if keyword_set(imcopy) &&  file_test(imcopy, /exe) then imc = $
           imcopy $
        else begin
; System first
           
           spawn, 'which imcopy', wi

; Note CSH derivatives send <command> not found to STDOUT.  
           if strlen(wi) gt 0 &&  strpos(wi, 'not found') eq -1 then $
              imc = 'imcopy' $
           else begin
              
; If not in $PATH then look in $SSW.     
              imc = ssw_bin_path('imcopy', /ontology, found = imcfound)
              if ~imcfound then begin
                 message, /cont, "IMCOPY not found in system or SSW."
                 return
              endif
           endelse
        endelse

        print, imc
        
        fb = file_basename(tfile)
        ucfile = filepath(fb, /tmp)
        spawn, imc+' '+tfile+' '+ucfile
        
        n_ext = get_fits_nextend(ucfile)

        if n_params() eq 2 then begin
           hdr = headfits(ucfile, ext = 1)
        endif else begin
           data = readfits(ucfile, hdr, ext = 1)
        endelse
        get_distort  = get_distort && sxpar(hdr, 'CPDIS1') eq 'LOOKUP'

        if hstructs then index = fitshead2struct(hdr) $
        else index = hdr
        
        if uncertainty then begin
           if n_ext eq 1 then begin
              message, /cont, "File does not contain an uncertainty."
           endif else begin
              data_uncert = readfits(ucfile, uhdr, ext = 2)
              if keyword_set(string_header) then index_uncert = uhdr $
              else index_uncert = fitshead2struct(uhdr)
           endelse
        endif

        if get_distort then begin
           xdistort = readfits(ucfile, xdidx, ext = 3)
           ydistort = readfits(ucfile, ydidx, ext = 4)
        endif
        
        file_delete, ucfile
     endelse
     
  endif else begin
                                ; If NAXIS ne 0 then it's a
                                ; simple uncompressed FITS file.
     
     data = readfits(tfile, hdr)
     if keyword_set(string_header) then index = hdr $
     else index = fitshead2struct(hdr)

                                ; NB, no uncertainties or distortion
                                ; tables in simple files (I think)
     
  endelse
  
  return
end
