pro read_punch, tfiles, index, data, $
                use_shared_lib=use_shared_lib,shared_lib_path=shared_lib_path, $
                uncertainty=uncertainty, data_uncert = data_uncert, index_uncert = index_uncert

  ;   Purpose: read compressed PUNCH fits files
  ;
  ;   Input Parameters:
  ;      tfiles - file to read
  ;
  ;   Output Parameters:
  ;
  ;      index - fits metadata (structure) 
  ;      data - Total Brightness and Polarized Brighness image datacube (for higher level products)
  ;             Only Total Brightness for QuickPUNCH products
  ;
  ;   Keywords:
  ;   
  ;   use_shared_lib (switch) - (from mreadfits_tilecomp) use call_external interface to cfitsio shared library (instead of
  ;                              spawning imcopy), note this is OS dependent and possibly IDL version
  ;                              dependent, and has the potential to crash IDL if the shared library
  ;                              is not properly compiled
  ;   shared_lib_path - optional keyword specifying path of shared library (with trailing slash) for
  ;                     fitsio.so, default is the same path as for imcopy
  ;   uncertainty - if set returns the uncertainty in data with same dimension as 'data'
  ;   
  ;   Optional Outputs:
  ;   data_uncert, index_uncert - datacube and metadata corresponding to secondary HDU having uncertainty information.
  ;                                To be used with uncertainty keyword.
  ;
  ;
  ;   Notes:
  ;     Reads one fits file at a time. If you plan to run for a directory, ensure to run in a loop.
  ;     The first HDU contains primary data products while the second HDU contains uncertainty information.
  ;     The output generated for one fits file can be a datacube with two images tB and pB (depending on level of data product).
  ;   
  ;   Calling Examples:
  ;   
  ;   read_punch, tfile, index, data
  ;   read_punch, tfile, index, data, /use_shared_lib, /uncertainty, $
  ;               data_uncert=data_uncert, index_uncert=index_uncert
  ;
 
 
  loud=1-keyword_set(silent)
  mreadfits_header,tfiles,index,only_tags=only_tags,extension=1
  mreadfits_header,tfiles,index_uncert,only_tags=only_tags,extension=2
    
  use_shared_lib=keyword_set(use_shared_lib) or data_chk(shared_lib_path,/string)

  if use_shared_lib then begin
    defpath=ssw_bin_path('fitsio.so', found=found,/path_only,/ontology)
    case 1 of
      keyword_set(shared_lib_path): so_path=shared_lib_path ; user supplied
      else: so_path=defpath ; default path
    endcase
    found=file_exist(concat_dir(so_path,'fitsio.so')) ; verify available for OS/ARC
    if not found then begin
      box_message,'fitsio/shared object request but not available for this OS/ARCH - using imcopy
      use_shared_lib=0 ; override
    endif
  endif
  
  imcopy=ssw_bin_path('imcopy', /ontology, found=imcfound)
  
  ndata=n_params() gt 2 and ~keyword_set(only_uncompress)
  
  data = fitsio_read_image(tfiles, htest, so_path=so_path) 
  shtest=fitshead2struct(htest)

; removing 'z' from few keywords
  c2u=str_subset(shtest,'zbitpix,znaxis,znaxis1,znaxis2,blank,bzero,bscale')  ;,ZIMAGE,ZSIMPLE,ZTILE1,ZTILE2,ZTILE3,ZCMPTYPE,ZHECKSUM,ZDATASUM')
  tc2u=tag_names(c2u)
  ztags=where(strmid(tc2u,0,1) eq 'Z',zcnt)
  if zcnt gt 0 then begin
    zt=tc2u(ztags)
    nozt=strmid(zt,1,8)
    for i=0,zcnt-1 do c2u=rep_tag_name(c2u,zt(i),nozt(i))
  end
  
  index=join_struct(c2u,index)
  
  if keyword_set(uncertainty) then begin
    print, 'Reading uncertainty HDU...'
    data_uncert = fitsio_read_image(tfiles+'[2]', htest, so_path=so_path) 
    shtest=fitshead2struct(htest)

    ; removing 'z' from few keywords
    c2u=str_subset(shtest,'zbitpix,znaxis,znaxis1,znaxis2,blank,bzero,bscale')
    tc2u=tag_names(c2u)
    ztags=where(strmid(tc2u,0,1) eq 'Z',zcnt)
    if zcnt gt 0 then begin
      zt=tc2u(ztags)
      nozt=strmid(zt,1,8)
      for i=0,zcnt-1 do c2u=rep_tag_name(c2u,zt(i),nozt(i))
    end

    index_uncert=join_struct(c2u,index_uncert)
  endif
  
  print, 'Go PUNCH!'  
end