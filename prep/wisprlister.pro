;
; Project     : PSP - WISPR
;
; Name        : WISPRLISTER()
;
; Purpose     : Find desired subset of WISPR images in GUI with command line option
;
; Category    : WISPR
;
; Explanation : Matches data from summary file to desired criteria to provide list of WISPR images
;
; Syntax      : data = WISPRLISTER([ keywords ])
;
; Examples    : data = wisprlister()    ;FOR USE WITH GUI
;		data = wisprlister(orbit='01', xsize=960, det=1, date='20181101', /no_gui)       ;FOR COMMAND LINE
;		data = wisprlister(orbit='01', xsize=[960, 1920], det=1, date=['20181101', '20181102', '20181103'], /no_gui)       ;FOR COMMAND LINE
;
; Inputs      : Orbit:		String Containing Desired Orbit (Must be ##, i.e. '01')	
;
; Opt. Inputs : None
;
; Outputs     : List of files, including location, sorted by file name
;
; Opt. Outputs: None.
;
; Keywords    : Type:		For choosing data type. Default is synoptic, other possibility is 'WTS'
;   	    	/WTS	    	Type=WTS
;		Level:		For choosing data level. Default is currently 'L1', 'L2' is other option, if 'L3' or 'L2b' data products are added they $
;				be included as well
;		/FINAL:		If set, search for final data summary, otherwise assume QL
;		Det:		WISPR Detector (integer, can be array)
;		Reg:		Region (integer, can be array)
;		Xsize:		X size in pixels (integer, can be array)
;		Ysize:		Y size in pixels (integer, can be array)
;		Date:		Date of images in 'YYYYMMDD' format (string, can be array)
;		Nbin:		Total number of bins (integer, can be array)
;		Target:		Observing Target (string, can be array)
;		Study_ID:	Observing Study ID# (integer, can be array)
;		Xposure:	Exposure Time (integer, can be array)
;		Gain:		Observing Gain Mode (must be 'H' or 'L', can be array)
;		SSR:		SSR Priority Level (integer, can be array)
;		no_gui:		Set this keyword to skip the GUI 
;
;
; Env. Vars.  : None
;
; Calls       : WISPR_READ_SUMMARY, WISPR_FITS_PATH
;
; Common      : None.
;
; Limitations:  If all values for a given search paramter are not present in summary file, this parameter is ignored and data is not queried on it
;
; Side effects: None.
;
; Prev. Hist. : None.
;
; Written     : Phillip Hess NRL 2019
;
;
;$Log: wisprlister.pro,v $
;Revision 1.17  2022/01/10 16:40:28  phess
;Added orbits up to #15
;
;Revision 1.16  2021/03/11 20:34:45  phess
;add dates for encoutners 6 and 7
;
;Revision 1.15  2021/01/21 21:37:06  phess
;Add dates for encounter 5
;
;Revision 1.14  2020/02/04 14:50:56  phess
;Fixed CAL1 searches
;
;Revision 1.13  2020/01/29 20:59:26  phess
;Improved gendatearray to work with larger time ranges
;
;Revision 1.12  2019/11/05 22:14:11  phess
;Add rel keyword to wispr_fits_path
;
;Revision 1.11  2019/11/05 14:49:21  phess
;Modify window size to fit full row
;
;Revision 1.10  2019/11/04 16:06:39  phess
;Add choice pf ql or rel
;
;Revision 1.9  2019/11/02 00:00:40  nathan
;add /WTS
;
;Revision 1.8  2019/10/11 19:41:23  phess
;Fixed bug on empty days
;
;Revision 1.7  2019/06/18 18:05:53  phess
;Fixed problem with reload orig
;
;Revision 1.6  2019/05/20 13:40:52  phess
;Fixed bug in query command
;
;Revision 1.5  2019/05/13 20:43:54  phess
;added point and click, help and cleaned up the format
;
;
;
;-
FUNCTION GENDATEARRAY, DSTOP, MSTOP, YSTOP, DSTART, MSTART,YSTART
dstartint=fix(dstart)
dstopint=fix(dstop)
darr0=ystart+mstart+dstart
darrn=ystop+mstop+dstop
mcur=mstart

dutc0=str2utc(string(darr0))
dutc1=str2utc(string(darrn))


darr=replicate(darr0, dutc1.mjd-dutc0.mjd+1)

if dutc1.mjd-dutc0.mjd gt 0 then begin
for i=1, dutc1.mjd-dutc0.mjd do begin
	date=utc2str({mjd:dutc0.mjd+i, time:0})
	darr[i]=strmid(date, 0, 4)+strmid(date, 5, 2)+strmid(date, 8, 2)
endfor
return, darr
endif else begin
return, darr0
endelse
end






;subroutine to search data structure for given value of a specific parameter
function search_param, data, param, value

calinx=0
if strmid(data[0].filename, 4,4) eq 'CAL1' then calinx=2



CASE param OF
    'det': tmpdata=data.detector
    'reg': tmpdata=data.region
    'xsize': tmpdata=data.naxis1
    'ysize': tmpdata=data.naxis2
    'date': tmpdata=strmid(data.filename, 13+calinx, 8)
    'nbin': tmpdata=data.nbin
    'target': tmpdata=data.target
    'study_id': tmpdata=data.study_id
    'xposure': tmpdata=data.xposure
    'gain': tmpdata=data.gainmode
    'ssr': tmpdata=data.ssrpriority
endcase 
n=n_elements(value)
cnt=0
for i=0, n-1 do begin
if i gt 0 and cnt gt 0 then begin
   tmp=cnt
   wnew=where(tmpdata eq value[i], cnt)
   if cnt gt 0 then w=[w,wnew]
   cnt=cnt+tmp 
endif else begin
   w=where(tmpdata eq value[i], cnt)
endelse
endfor


if n_elements(w) gt 0 then data=data[w] else message, 'INVALID VALUE '+param, /info

return, data
end

PRO WISPRLISTER_EVENT,event
COMMON wisp_date_block1, month, day, year, ibm, ibd, iby, iem, ied, iey, iip, iil, idp, icx, $
		ipi, sec_ipi, orbs, orbmon_b, orbday_b, orbyear_b, orbmon_e, orbday_e, orbyear_e
COMMON wisp_queries1, qtel, qreg, qexpt, qxsize, qysize, qbin, qtarg, qid, qgain, $
                qpri, rmvsub, reset, qtelval, qregval, qexptval, qxsizeval, qysizeval, $
                qbinval, qtargval, qidval, qgainval, qprival, qclist, qreglist, qprilist, $
                qgainlist, matches, orig_matches, sc_dir, orig_files, orig_msg, qry
COMMON wisp_savefiles1, saveid,savename,sname
COMMON wisp_sorts1, sortid,sortit,sort_mtd,sortlist,last_sort_mtd,sort_order,sorder
;COMMON scraft1, sc_sel, sclist, sel_id, sec_select, sec_lvl, lvl_ind, dtype, p_type, p_ind, $
;               sec_level, sources, src_used, src_ind
COMMON wisp_scraft1, orb_ind, typ_ind, lvl_ind, rel_ind, orbsel, slevel, stype, detlist,det_sel
COMMON wisp_fdisplay1, draw
COMMON wev, ms


;print,qtel, qreg, qexpt, qxsize, qysize, qbin, qtarg, qid, qgain


WIDGET_CONTROL, event.id, GET_UVALUE=uval          ; get the UVALUE of the event
WIDGET_CONTROL, event.top, GET_UVALUE=uv           ; get structure from UVALUE



CASE uval OF

  "FNSH": BEGIN

;help,files,matches
           ;help, matches
           ; sipath= CHECK_MATCHES(files, uv, fmsg, /DONE
	    
            WIDGET_CONTROL, event.top, /DESTROY

          END



  "QRY":  BEGIN
            WIDGET_CONTROL,/HOUR
            SUBQUERY,files,uv
            n=n_elements(matches)
            WIDGET_CONTROL,uv.lab,SET_VALUE= 'Found total of '+STRTRIM(n,2)+' files.'
            WIDGET_CONTROL,uv.lab1,SET_VALUE= ''
            WIDGET_CONTROL,uv.lab2,SET_VALUE= ''
	    draw_matches
          END

  "REMOVESUB":  BEGIN
            WIDGET_CONTROL,/HOUR
            SUBQUERY,files,uv,/remove
            n=n_elements(matches)
	    WIDGET_CONTROL,uv.lab,SET_VALUE= 'Found total of '+STRTRIM(n,2)+' files.'
            WIDGET_CONTROL,uv.lab1,SET_VALUE= ''
            WIDGET_CONTROL,uv.lab2,SET_VALUE= ''
	    draw_matches
          END



"RESET":  BEGIN
;help,files,orig_files,matches,orig_matches
;stop
              WIDGET_CONTROL,/HOUR
              matches= orig_matches
            ;  files= orig_files
              ;fmsg= orig_msg
              ;WIDGET_CONTROL,draw,SET_VALUE= matches
	      draw_matches
              WIDGET_CONTROL,qtel,SET_DROPLIST_SELECT= 0
              qtelval='All' 
              WIDGET_CONTROL,qreg,SET_DROPLIST_SELECT= 0
              qregval='All' 
              WIDGET_CONTROL,qexpt,SET_VALUE= ''
              WIDGET_CONTROL,qxsize,SET_VALUE= ''
              WIDGET_CONTROL,qysize,SET_VALUE= ''
              WIDGET_CONTROL,qbin,SET_VALUE= ''
              WIDGET_CONTROL,qtarg,SET_VALUE= ''
              WIDGET_CONTROL,qid,SET_VALUE= ''
	      WIDGET_CONTROL,qgain,SET_DROPLIST_SELECT= 0
	      qgainvel='All'
              WIDGET_CONTROL,qpri,SET_DROPLIST_SELECT= 0 
	      qprivel='All'
              ;WIDGET_CONTROL,qpolr,SET_COMBOBOX_SELECT= 0 
              WIDGET_CONTROL,uv.lab,SET_VALUE= fmsg
              last_sort_mtd= ''
              sort_order=0 ; Ascending
              last_sort_mtd= last_sort_mtd+STRTRIM(sort_order,2)
              WIDGET_CONTROL,sorder,SET_DROPLIST_SELECT= sort_order 
          END

  "QTEL": BEGIN
               qtelval= qclist(event.index)
               ;help,qtelval
          END
  "QREG": BEGIN
               qregval= qreglist(event.index)
               ;help,qtelval
          END
  "QEXPT": BEGIN
              WIDGET_CONTROL, qexpt ,GET_VALUE= qexptval
              qexptval= strtrim(qexptval,2)
              ;help,qexpt1val
          END
  "QXZ": BEGIN
              WIDGET_CONTROL, qxsize ,GET_VALUE= qxsizeval
              qxsizeval= strtrim(qxsizeval,2)
              ;help,qxsizeval
          END
 "QYZ": BEGIN
              WIDGET_CONTROL, qysize ,GET_VALUE= qysizeval
              qysizeval= strtrim(qysizeval,2)
              ;help,qysizeval
          END
 "QBIN": BEGIN
		WIDGET_CONTROL, qbin ,GET_VALUE= qbinval
               qbin=strtrim(qbinval,2)
              ;help,qfltval
          END
 "QTARG": BEGIN
		WIDGET_CONTROL, qtarg ,GET_VALUE= qtargval
               qtarg=strtrim(qtarg,2)
              ;help,qfltval
          END
 "QID": BEGIN
		WIDGET_CONTROL, qid ,GET_VALUE= qidval
               qid=strtrim(qidval,2)
              ;help,qfltval
          END
  "QGAIN": BEGIN
               qgainval= qgainlist(event.index)
               ;help,qtelval
          END
  "QPRI": BEGIN
               qprival= qprilist(event.index)
               ;help,qtelval
          END
   "SAVEID": BEGIN
               WIDGET_CONTROL, sname, GET_VALUE= savename
               savename= STRTRIM(savename(0))
;help,matches,files

               IF (matches(0).filename NE '') THEN BEGIN
		 WISPR_IMAGES=matches            
                 SAVE,filename= savename, WISPR_IMAGES 
                 WIDGET_CONTROL, uv.lab, SET_VALUE='Saved displayed file names and' 
		 WIDGET_CONTROL, uv.lab2, SET_VALUE='info as '+savename+'.'
               ENDIF ELSE WIDGET_CONTROL, uv.lab, SET_VALUE='Nothing to Save.'
          END
   "SORDER": BEGIN
              sort_order= event.index
          END 
  "SRT_SEL": BEGIN
              sort_mtd= strtrim(sortlist[event.index],2)
              ;help, sort_mtd
          END
  "SORT": BEGIN
            ; help,event, sort_mtd, last_sort_mtd, matches, files
             WIDGET_CONTROL, /HOUR
             IF (matches(0).filename EQ '') THEN BEGIN
               WIDGET_CONTROL, uv.lab, SET_VALUE= 'Nothing to Sort.'
               RETURN
             END
             IF (sort_mtd+STRTRIM(sort_order,2) NE last_sort_mtd) THEN BEGIN
               last_sort_mtd= sort_mtd+STRTRIM(sort_order,2)
               SORT_ROWS, matches, sord, sort_mtd, sort_order
               draw_matches
               WIDGET_CONTROL, uv.lab,SET_VALUE= "Sorted by"+sord+sort_mtd+"."
             ENDIF
          END
   "BM": ibm = event.index
   "BD": ibd = event.index
   "BY": iby = event.index  
   "EM": iem = event.index
   "ED": ied = event.index
   "EY": iey = event.index

   "DETFT" : det_sel= event.index
   "LEVELFT" : lvl_ind= event.index
   "RELFT" : rel_ind=event.index
   "TYPEFT" : typ_ind= event.index
   "ORBFT" : BEGIN
	orb_ind= event.index
   	if orb_ind le orbs-1 then begin
	iby=orbyear_b[orb_ind]
	ibd=orbday_b[orb_ind]
	ibm=orbmon_b[orb_ind]
	iey=orbyear_e[orb_ind]
	ied=orbday_e[orb_ind]
	iem=orbmon_e[orb_ind]
	widget_control,uv.b_year, SET_VALUE=iby
	widget_control,uv.b_month,SET_VALUE=ibm
	widget_control,uv.b_day,  SET_VALUE=ibd
	widget_control,uv.e_year, SET_VALUE=iey
	widget_control,uv.e_month,SET_VALUE=iem
	widget_control,uv.e_day,  SET_VALUE=ied
	endif
   END	
   "SB": BEGIN
          ;widget_control,uv.b_year, SET_DROPLIST_SELECT=iey
	  ;widget_control,uv.b_month,SET_DROPLIST_SELECT=iem
	  ;widget_control,uv.b_day,  SET_DROPLIST_SELECT=ied
	  widget_control,uv.b_year, SET_VALUE=iey
	  widget_control,uv.b_month,SET_VALUE=iem
	  widget_control,uv.b_day,  SET_VALUE=ied
           ibd = ied
           ibm = iem
           iby = iey

         END

   "SE": BEGIN
          ;widget_control,uv.e_year, SET_DROPLIST_SELECT=iby
	  ;widget_control,uv.e_month,SET_DROPLIST_SELECT=ibm
	  ;widget_control,uv.e_day,  SET_DROPLIST_SELECT=ibd
	  widget_control,uv.e_year, SET_VALUE=iby
	  widget_control,uv.e_month,SET_VALUE=ibm
	  widget_control,uv.e_day,  SET_VALUE=ibd
          ied = ibd
          iem = ibm
          iey = iby
         END
  "FIND": BEGIN
     matches=wispr_read_summary(orbsel[orb_ind], level=slevel[lvl_ind], wts=typ_ind, final=1-rel_ind)
     ;print, n_elements(matches)
     if detlist[det_sel] ne 'All' then matches=search_param(matches, 'det', strtrim(detlist[det_sel],2))
    
     ;print, day(ied), month(iem), year(iey), day(ibd), month(ibm), year(iby)
     datearr=gendatearray(day(ied), month(iem), year(iey), day(ibd), month(ibm), year(iby))
     
     matches=search_param(matches, 'date', datearr)
     
     orig_matches=matches
     WIDGET_CONTROL,uv.lab2, SET_VALUE=''
     WIDGET_CONTROL,uv.lab1, SET_VALUE=''
     WIDGET_CONTROL,/HOUR
     WIDGET_CONTROL,uv.lab,SET_VALUE=" Working ........                       "
     WIDGET_CONTROL,qtel,SENSITIVE=0
     WIDGET_CONTROL,qreg,SENSITIVE=0
     WIDGET_CONTROL,qexpt,SENSITIVE=0
     WIDGET_CONTROL,qxsize,SENSITIVE=0
     WIDGET_CONTROL,qysize,SENSITIVE=0
     WIDGET_CONTROL,qbin,SENSITIVE=0
     WIDGET_CONTROL,qtarg,SENSITIVE=0
     WIDGET_CONTROL,qid,SENSITIVE=0
     WIDGET_CONTROL,qgain,SENSITIVE=0
     WIDGET_CONTROL,qpri,SENSITIVE=0
     WIDGET_CONTROL,reset,SENSITIVE=0
     WIDGET_CONTROL,sorder,SENSITIVE=0
     WIDGET_CONTROL,sortid,SENSITIVE=0
     WIDGET_CONTROL,sortit,SENSITIVE=0
     WIDGET_CONTROL,saveid,SENSITIVE=0
     WIDGET_CONTROL,sname,SENSITIVE=0 
     
 IF uv.draw GT 0 THEN BEGIN 
     WIDGET_CONTROL,qtel,sensitive=1
     WIDGET_CONTROL,qreg,sensitive=1
     WIDGET_CONTROL,qexpt,sensitive=1
     WIDGET_CONTROL,qxsize,sensitive=1
     WIDGET_CONTROL,qysize,sensitive=1
     WIDGET_CONTROL,qbin,sensitive=1
     WIDGET_CONTROL,qtarg,sensitive=1
     WIDGET_CONTROL,qid,sensitive=1
     WIDGET_CONTROL,qgain,sensitive=1
     WIDGET_CONTROL,qpri,sensitive=1
     WIDGET_CONTROL,reset,sensitive=1
     WIDGET_CONTROL,sorder,sensitive=1
     WIDGET_CONTROL,sortid,sensitive=1
     WIDGET_CONTROL,sortit,sensitive=1
     WIDGET_CONTROL,saveid,sensitive=1
     WIDGET_CONTROL,sname,sensitive=1 
     WIDGET_CONTROL,qry,sensitive=1
     WIDGET_CONTROL,rmvsub,sensitive=1 

  
  WIDGET_CONTROL,uv.lab,SET_VALUE= 'Checking and processing summary file matches....' 
  WIDGET_CONTROL,uv.lab1,SET_VALUE= ''
  WIDGET_CONTROL,uv.lab2,SET_VALUE= ''
 ENDIF
     
     



     
    draw_matches

    nmatches=n_elements(matches)

    WIDGET_CONTROL,uv.lab,SET_VALUE= 'Found total of '+STRTRIM(nmatches,2)+' files.'
    WIDGET_CONTROL,uv.lab1,SET_VALUE= ''
    WIDGET_CONTROL,uv.lab2,SET_VALUE= ''

    ; endif
     ;print, files



	END
   "PDMENU": BEGIN
	CASE event.value OF
	"PointAndClickFiles": BEGIN
		n=n_elements(matches)
                  IF (n gt 1) THEN BEGIN

			i=0
     write_string=matches[i].filename+'   '+strtrim(matches[i].detector,2)+'   '+strtrim(matches[i].region,2)+' '+strpad(strtrim(matches[i].naxis1,2),5)+$
	'  '+strpad(strtrim(matches[i].naxis2,2),5)+'    '+strpad(strtrim(matches[i].nsumexp,2),2)+' '+strpad(strtrim(matches[i].xposure,2),6)+'     '+strtrim(matches[i].nbin,2)+'    '+matches[i].gainmode+$
	' '+strpad(strtrim(matches[i].det_t,2),6)+' '+string(matches[i].datamdn, '(g8.3)')+' '+matches[i].target+'     '+strtrim(matches[i].study_id,2)+'      '+strtrim(matches[i].ssrpriority,2)
	draw_matches=replicate(write_string, n)
     for i=1, n-1 do draw_matches[i]=matches[i].filename+'   '+strtrim(matches[i].detector,2)+'   '+strtrim(matches[i].region,2)+' '+strpad(strtrim(matches[i].naxis1,2),5)+$
	'  '+strpad(strtrim(matches[i].naxis2,2),5)+'    '+strpad(strtrim(matches[i].nsumexp,2),2)+' '+strpad(strtrim(matches[i].xposure,2),6)+'     '+strtrim(matches[i].nbin,2)+'    '+matches[i].gainmode+$
	' '+strpad(strtrim(matches[i].det_t,2),6)+' '+string(matches[i].datamdn, '(g8.3)')+' '+matches[i].target+'     '+strtrim(matches[i].study_id,2)+'      '+strtrim(matches[i].ssrpriority,2)
                     ;print, matches.filename

                    ;resp= xmenu_sel(TIT="  (Save/Append filenames to ./myfiles.list)",$
                    resp= xmenu_sel(TIT="  (Select Files)",draw_matches, nlines=30,group=ms.WLISTERBASE)
                    IF resp(0) NE -1 THEN BEGIN 
                      pickedfiles= matches(resp)
                      ;help,resp
                      ;help,pickedfiles

                      ;help,matches,files
                      matches= TEMPORARY(matches(resp))
                              ;help,matches,files
                      
                      ;WIDGET_CONTROL,draw,SET_VALUE= matches
		      draw_matches
;                      PRINT,''
;                      PRINT,'Adding selected files to ./myfiles.list'
;                      OPENW, fslun, './myfiles.list', /GET_LUN, /APPEND
;                      FOR fsel= 0, N_ELEMENTS(resp)-1 DO $
;                        ;PRINTF,fslun, files(resp(fsel))
;                        PRINTF,fslun, files((fsel)
;                      CLOSE,fslun
;                      FREE_LUN, fslun 
;                      PRINT,'Done.'

                    ENDIF
		ENDIF
	END
   "Help":  BEGIN
   
     hmsg= ['Select Data level, type and orbit. Selecting an orbit will automatically',$
	    'set the date ranges to cover the days for which there is valid data for',$
	    'that orbit. The date range can then be further restricted using the drop',$
	    'down menu.',$
	    ' ',$
	    'The fields that are found will then shown in the window and can be',$
	    'further restricted by the listed parameters. The Query button will return',$
	    'the desired sub-sample and the remove button will exclude those files.',$
	    'After performing a search the reload button will return the original list',$
	    'The data can be sorted in either direction based on file name, detector or',$
	    'region. If an invalid date range for a given orbit or a parameter value not',$
	    'found in the file is provided, that search term will be excluded and the',$
	    'full data set will be returned.',$
	    ' ',$
	    'The save button will put the file names (paths included) into the listed',$
	    '.sav file.']

          
             XDISPLAYFILE,TEXT=hmsg,TITLE='WISPRLISTER Help', $
             GROUP=event.top, HEIGHT=20, WIDTH=90
  
               	 END
      ENDCASE
	END
ELSE: donothing=0 
ENDCASE


END

PRO draw_matches
common wisp_sorts1
COMMON wisp_queries1
COMMON wisp_fdisplay1

n=n_elements(matches)
	i=0
     write_string=matches[i].filename+'   '+strtrim(matches[i].detector,2)+'   '+strtrim(matches[i].region,2)+' '+strpad(strtrim(matches[i].naxis1,2),5)+$
	'  '+strpad(strtrim(matches[i].naxis2,2),5)+'    '+strpad(strtrim(matches[i].nsumexp,2),2)+' '+strpad(strtrim(matches[i].xposure,2),6)+'     '+strtrim(matches[i].nbin,2)+'    '+matches[i].gainmode+$
	' '+strpad(strtrim(matches[i].det_t,2),6)+' '+string(matches[i].datamdn, '(g8.3)')+' '+matches[i].target+'     '+strtrim(matches[i].study_id,2)+'      '+strtrim(matches[i].ssrpriority,2)
	draw_matches=replicate(write_string, n)
     if n gt 0 then begin
     for i=1, n-1 do draw_matches[i]=matches[i].filename+'   '+strtrim(matches[i].detector,2)+'   '+strtrim(matches[i].region,2)+' '+strpad(strtrim(matches[i].naxis1,2),5)+$
	'  '+strpad(strtrim(matches[i].naxis2,2),5)+'    '+strpad(strtrim(matches[i].nsumexp,2),2)+' '+strpad(strtrim(matches[i].xposure,2),6)+'     '+strtrim(matches[i].nbin,2)+'    '+matches[i].gainmode+$
	' '+strpad(strtrim(matches[i].det_t,2),6)+' '+string(matches[i].datamdn, '(g8.3)')+' '+matches[i].target+'     '+strtrim(matches[i].study_id,2)+'      '+strtrim(matches[i].ssrpriority,2)
	WIDGET_CONTROL,draw,SET_VALUE=draw_matches,/NO_COPY
     endif
END


PRO SUBQUERY, files,uv,DONE=DONE,REMOVE=REMOVE
common wisp_sorts1
COMMON wisp_queries1

if keyword_set(remove) then matches0=matches

if qtelval ne 'All' then matches=search_param(matches, 'det', strtrim(qtelval,2))
if qprival ne 'All' then matches=search_param(matches, 'ssr', strtrim(qprival,2))
if qgainval ne 'All' then matches=search_param(matches, 'gain', strtrim(qgainval,2))
if qregval ne 'All' then matches=search_param(matches, 'reg', strtrim(qregval,2))
if qexptval ne '' then matches=search_param(matches, 'xposure', strtrim(qexptval,2))
if qxsizeval ne '' then matches=search_param(matches, 'xsize', strtrim(qxsizeval,2))
if qysizeval ne '' then matches=search_param(matches, 'ysize', strtrim(qysizeval,2))
if qbinval ne '' then matches=search_param(matches, 'nbin', strtrim(qbinval,2))
if qtargval ne '' then matches=search_param(matches, 'target', strtrim(qtargval,2))
if qidval ne '' then matches=search_param(matches, 'study_id', strtrim(qidval,2))

if keyword_set(remove) then begin
     match, matches0.filename, matches.filename, suba, subb
     remove, suba, matches0
     matches=matches0
     n=n_elements(matches)
endif

end



PRO SORT_ROWS, matches, sord, sort_mtd, sort_order
   CASE sort_mtd OF

   "Filename":sind=sort(matches.filename)
   "Detector":sind=sort(matches.detector)
   "Region":sind=sort(matches.region)
   "Priority":sind=sort(matches.ssrpriority)
   "Study ID":sind=sort(matches.study_id)
   ENDCASE
   sord=' Ascending '

   if sort_order EQ 1 then begin
      sind=reverse(sind)
      sord=' Descending '
   endif

   matches=temporary(matches(sind))
   return
END


function WISPRLISTER, orbit=orbit, WTS=wts, TYPE=type, level=level, det=det, reg=reg, $
    	xsize=xsize, ysize=ysize, date=date, nbin=nbin, target=target, study_id=study_id, $
	xposure=xposure, gain=gain, ssr=ssr, final=final, no_gui=no_gui
COMMON wisp_date_block1
COMMON wisp_queries1
COMMON wisp_sorts1
COMMON wisp_savefiles1
COMMON wisp_scraft1
COMMON wisp_fdisplay1
COMMON wev
if keyword_set(no_gui) then begin 
IF keyword_set(TYPE) THEN IF strupcase(type) EQ 'WTS' THEN wts=1
IF NOT KEYWORD_SET(level) THEN level = 'L3'
IF NOT KEYWORD_SET(final) THEN final= 1
data=wispr_read_summary(orbit, level=level, final=final, wts=wts)
; PR change - added second conditional to check that det NE 3
IF KEYWORD_SET(det) AND (det NE 3) THEN data=search_param(data,'det', det)
IF KEYWORD_SET(reg) THEN data=search_param(data,'reg', reg)
IF KEYWORD_SET(xsize) THEN data=search_param(data,'xsize', xsize)
IF KEYWORD_SET(ysize) THEN data=search_param(data,'ysize', ysize)
IF KEYWORD_SET(date) THEN data=search_param(data,'date', date)
IF KEYWORD_SET(nbin) THEN data=search_param(data,'nbin', nbin)
IF KEYWORD_SET(target) THEN data=search_param(data,'target', target)
IF KEYWORD_SET(study_id) THEN data=search_param(data,'study_id', study_id)
IF KEYWORD_SET(xposure) THEN data=search_param(data,'xposure', xposure)
IF KEYWORD_SET(gain) THEN data=search_param(data,'gain', gain)
IF keyword_set(ssr) THEN data=search_param(data,'ssr', ssr)

data.filename=wispr_fits_path(data.filename,FINAL=final)

return, data[sort(data.filename)].filename

ENDIF ELSE BEGIN
mnames = [ 'Load from Catalog(s)']
nmodes = n_elements(mnames)
ms = { wisprlister, WListerBase:0L, $
       mnames:mnames, $
       mbases:lonarr(nmodes) $
     }


FromTo = [" From .. To "," Single Day "]

month=['01','02','03','04','05','06','07','08','09','10','11','12']

day=['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31']
day = STRCOMPRESS(day,/REMOVE_ALL)

year=['2018','2019','2020','2021','2022','2023','2024','2025']

orbs=15
orbmon_b=[10,3,8,0,5,8,0,3,7,10,1,4,8,11,2]
orbday_b=[0,0,0,22,0,21,11,22,2,14,18,25,0,4,10]
orbyear_b=[0,1,1,2,2,2,3,3,3,3,4,4,4,4,5]

orbmon_e=[10,3,8,1,5,9,0,4,7,10,2,5,8,11,2]
orbday_e=[11,9,9,3,12,2,22,3,14,26,1,6,11,16,22]
orbyear_e=[0,1,1,2,2,2,3,3,3,3,4,4,4,4,5]

DEVICE, GET_SCREEN_SIZE= scr

xs1=0
ys1=0
IF scr[0] LT 1100 or scr[1] LT 700 THEN BEGIN
    xs1=(scr[0]-40)<1095
    ys1=(scr[1]-40)<700
ENDIF

ms.WListerBase = WIDGET_BASE(TITLE = "WISPR Image Selection Tool",/ROW, $
                             XOFFSET=5, YOFFSET=5, X_SCROLL_SIZE=xs1,Y_SCROLL_SIZE=ys1)
lcol      = WIDGET_BASE(ms.WListerBase, /COLUMN)

lcol1= WIDGET_BASE(ms.WListerBase,/COLUMN,/FRAME)

colhdr=' FileName                                  Det Reg Xsize  Ysize  NSum Xposure Nbin  Gain    Det_T  DataMdn  Target  StudyID   SSR'
txt=WIDGET_LABEL(lcol1,VALUE= colhdr,scr_xsize=820,scr_ysize=15,/align_left)
draw= WIDGET_LIST(lcol1,UVALUE='drawn',scr_xsize=820,scr_ysize=400,/frame)
lcol0= WIDGET_BASE(lcol1,/COL,/FRAME, xsize=820)


txt=WIDGET_LABEL(lcol0,VALUE= 'Sub-Select Options', /align_center)

lcol2= WIDGET_BASE(lcol0,/ROW)
qclist= ['All','1','2']
qtelval= 'All' 
lcolflds= WIDGET_BASE(lcol2,/ROW)
lcolfld= WIDGET_BASE(lcolflds,/COL)


txt=WIDGET_LABEL(lcolfld, VALUE= 'Det')
qtel = WIDGET_DROPLIST(lcolfld, VALUE=qclist, UVALUE="QTEL")
WIDGET_CONTROL,qtel,SENSITIVE=0


qreglist=['All', '1', '2', '3', '4', '5', '6']
qregval='All'
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'Reg')
qreg = WIDGET_DROPLIST(lcolfld, VALUE=qreglist, UVALUE="QREG")
WIDGET_CONTROL,qreg,SENSITIVE=0

qexptval=''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'XPosure')
qexpt= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QEXPT", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qexpt,SENSITIVE=0

qxsizeval= ''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'Xsize')
qxsize= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QXZ", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qxsize,SENSITIVE=0

qysizeval= ''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'Ysize')
qysize= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QYZ", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qysize,SENSITIVE=0

qbinval=''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'Nbin')
qbin= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QBIN", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qbin,SENSITIVE=0

qtargval=''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'Target')
qtarg= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QTARG", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qtarg,SENSITIVE=0

qidval=''
lcolfld= WIDGET_BASE(lcolflds,/COL)
txt=WIDGET_LABEL(lcolfld, VALUE= 'StudyID')
qid= WIDGET_TEXT(lcolfld, VALUE='',UVALUE="QID", XSIZE=9,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,qid,SENSITIVE=0

qgainlist= ['All','H','L']
qgainval= 'All' 
lcolfld= WIDGET_BASE(lcolflds,/COL)

txt=WIDGET_LABEL(lcolfld, VALUE= 'Gain')
qgain = WIDGET_DROPLIST(lcolfld, VALUE=qgainlist, UVALUE="QGAIN")
WIDGET_CONTROL,qgain,SENSITIVE=0


qprilist= ['All','1','2','3','4','5','6']
qprival= 'All' 
lcolfld= WIDGET_BASE(lcolflds,/COL)

txt=WIDGET_LABEL(lcolfld, VALUE= 'Priority')
qpri = WIDGET_DROPLIST(lcolfld, VALUE=qprilist, UVALUE="QPRI")
WIDGET_CONTROL,qpri,SENSITIVE=0

lcol3= WIDGET_BASE(lcol0,/ROW,/FRAME)

;txt= WIDGET_LABEL(lcol3,VALUE= '  ')
txt= WIDGET_LABEL(lcol3,VALUE= '                     ')
qry= WIDGET_BUTTON(lcol3, VALUE='Query Sub set', UVALUE="QRY", FONT=font,/ALIGN_LEFT,/FRAME)
WIDGET_CONTROL,qry,SENSITIVE=0
txt= WIDGET_LABEL(lcol3,VALUE= '                      ')
rmvsub= WIDGET_BUTTON(lcol3, VALUE='Remove Sub set', UVALUE="REMOVESUB", FONT=font,/ALIGN_LEFT,/FRAME)
WIDGET_CONTROL,rmvsub,SENSITIVE=0
txt= WIDGET_LABEL(lcol3,VALUE= '                      ')
;reset= WIDGET_BUTTON(lcol3, VALUE='ReloadOrigResult', UVALUE="RESET")
reset= WIDGET_BUTTON(lcol3, VALUE='ReloadOrig', UVALUE="RESET",/ALIGN_RIGHT,/FRAME)
WIDGET_CONTROL,reset,SENSITIVE=0
;txt= WIDGET_LABEL(lcol1, VALUE='')

;;sortlist=['Time','Filename','Telescope']
sortlist=['Filename','Detector','Region','Priority','Study_ID']
sort_mtd= sortlist(0) ; Time
last_sort_mtd=''
lcol4= WIDGET_BASE(lcol1,/ROW)
txt= WIDGET_LABEL(lcol4, valUE='                                Sort Display by')
sort_order= 0 ;Ascending
last_sort_mtd= last_sort_mtd+STRTRIM(sort_order,2)
sorder= WIDGET_DROPLIST(lcol4, VALUE=['Ascending','Descending'], UVALUE="SORDER")
WIDGET_CONTROL,sorder,SENSITIVE=0
sortid= WIDGET_DROPLIST(lcol4, VALUE=sortlist, UVALUE="SRT_SEL")
WIDGET_CONTROL,sortid,SENSITIVE=0
txt= WIDGET_LABEL(lcol4, VALUE='  ')
sortit= WIDGET_BUTTON(lcol4, VALUE='Sort', UVALUE="SORT", FONT=font)
WIDGET_CONTROL,sortit,SENSITIVE=0


lcol5= WIDGET_BASE(lcol1,/ROW,/FRAME)
;txt= WIDGET_LABEL(lcol5, VALUE='                      ')
txt= WIDGET_LABEL(lcol5, VALUE='           Save Displayed Image Summary Structure')
savename= 'wispr_images.sav'
sname= WIDGET_TEXT(lcol5,VALUE=savename,UVALUE="SNAME",XSIZE=17,YSIZE=1,/EDITABLE, /all_events)
WIDGET_CONTROL,sname,SENSITIVE=0
txt= WIDGET_LABEL(lcol5, VALUE='consisting of structure named WISPR_IMAGES')
saveid= WIDGET_BUTTON(lcol5, VALUE='Save', UVALUE="SAVEID", FONT=font)
WIDGET_CONTROL,saveid,SENSITIVE=0

txt= WIDGET_LABEL(lcol1, VALUE='')
;fnsh = WIDGET_BUTTON(lcol1, VALUE='Done/SaveResults', UVALUE = 'FNSH', FONT=font)
fnsh = WIDGET_BUTTON(lcol1, VALUE='Done - Return Filenames', UVALUE = 'FNSH', FONT=font)


junk = { CW_PDMENU_S, flags:0, name:'' }
pdm_desc = [ $
             { CW_PDMENU_S, 0, 'Help' },        $
             { CW_PDMENU_S, 2, 'PointAndClickFiles' }]

menu = CW_PDMENU(lcol, pdm_desc, UVALUE = 'PDMENU', /RETURN_FULL_NAME)


files=[""]

;ff = findfile(img_dir)
;ff = FILE_SEARCH(img_dir)

; IF ff(0) EQ '' THEN ff = ['20050101']
 ff = ['20050101']
;stop
; ff = yymmdd(ff)
 nn = n_elements(ff)

 tyear = STRCOMPRESS(STRMID(ff(0),0,2),/REMOVE_ALL)


IF (DATATYPE(ibd) EQ 'UND') THEN ibd=orbday_b[0]
IF (DATATYPE(ied) EQ 'UND') THEN ied=orbday_e[0]
IF (DATATYPE(ibm) EQ 'UND') THEN ibm=orbmon_b[0]
IF (DATATYPE(iem) EQ 'UND') THEN iem=orbmon_e[0]
IF (DATATYPE(iby) EQ 'UND') THEN iby=orbyear_b[0]
IF (DATATYPE(iey) EQ 'UND') THEN iey=orbyear_e[0]


bday= day(ibd)
eday= day(ied)
bmonth= month(ibm)
emonth= month(iem)
byear= year(iby)
eyear= year(iey)
 
 DatePath = ff
 iip = 0
 iil = 0
 idp = 0
 icx = 0
 
llcol= WIDGET_BASE(lcol,/COLUMN,/FRAME)
lbl01= WIDGET_LABEL(llcol,VALUE='Loading Methods:')
junk = WIDGET_BASE(llcol,COLUMN=4,/EXCLUSIVE)
bf = lonarr(nmodes)
for i=0,nmodes-1 do $
  bf(i) = WIDGET_BUTTON(junk, VALUE=ms.mnames(i), UVALUE=ms.mnames(i), $
                            /NO_RELEASE)
                            
junk = WIDGET_BASE(lcol,/FRAME,/COLUMN)
mode_base = WIDGET_BASE(junk)

for i=0,nmodes-1 do $
    ms.mbases(i) = WIDGET_BASE(mode_base, UVALUE=0L,/COLUMN)

parent = ms.mbases(0)

xjunk = WIDGET_BASE(parent,column=1)
 bl1 = WIDGET_BASE(xjunk,/COLUMN)

 detft= WIDGET_BASE(bl1,/ROW)
 tmp = WIDGET_LABEL(detft,VALUE="Select WISPR Detector: ")
 detlist= ['All','1','2']
 IF (DATATYPE(det_sel) EQ 'UND') THEN det_sel= 0 
 ;scp = WIDGET_DROPLIST(detft, VALUE=detlist, UVALUE="DETFT")
 detp = CW_BSELECTOR2(detft, detlist, UVALUE="DETFT", SET_VALUE=det_sel)

r6  = WIDGET_BASE(bl1, /ROW)
b6  = WIDGET_LABEL(r6,VALUE="Select Data Level:")
slevel=['L1', 'L2', 'L3', 'CAL1']
rf  = WIDGET_BASE(r6,/ROW)	;,/EXCLUSIVE)
b64 = CW_BSELECTOR2(rf, slevel, UVALUE="LEVELFT", SET_VALUE= lvl_ind)

r6  = WIDGET_BASE(bl1, /ROW)
b6  = WIDGET_LABEL(r6,VALUE="Select Release Level:")
srel=['REL', 'QL']
rf  = WIDGET_BASE(r6,/ROW)	;,/EXCLUSIVE)
b64 = CW_BSELECTOR2(rf, srel, UVALUE="RELFT", SET_VALUE= rel_ind)

r6  = WIDGET_BASE(bl1, /ROW)
 b6  = WIDGET_LABEL(r6,VALUE="Select Data Type:")
 stype=['Synoptic', 'WTS']
 rf  = WIDGET_BASE(r6,/ROW )	;,/EXCLUSIVE)
b64 = CW_BSELECTOR2(rf, stype, UVALUE="TYPEFT", SET_VALUE= typ_ind)


r6  = WIDGET_BASE(bl1, /ROW)
 b6  = WIDGET_LABEL(r6,VALUE="Select Orbit:")
 orbsel=['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25']
 rf  = WIDGET_BASE(r6,/ROW )	;,/EXCLUSIVE)
b64 = CW_BSELECTOR2(rf, orbsel, UVALUE="ORBFT", SET_VALUE= orb_ind)

 r6  = WIDGET_BASE(bl1, /ROW)
 b6  = WIDGET_LABEL(r6,VALUE="Select Observation Date:")

   rf1  = WIDGET_BASE(bl1, /FRAME,/COLUMN)
   r5  = WIDGET_BASE(rf1, /ROW)
   b5  = WIDGET_LABEL(r5,VALUE="From : ")

   ;w_m_b = WIDGET_DROPLIST(r5, VALUE=month, UVALUE="BM", set_value=ibm)
   ;w_d_b = WIDGET_DROPLIST(r5, VALUE=day,   UVALUE="BD")
   ;w_y_b = WIDGET_DROPLIST(r5, VALUE=year,  UVALUE="BY")

   w_m_b = CW_BSELECTOR2(r5, month, UVALUE="BM", SET_VALUE=ibm)
   w_d_b = CW_BSELECTOR2(r5, day, UVALUE="BD", SET_VALUE=ibd)
   w_y_b = CW_BSELECTOR2(r5, year, UVALUE="BY", SET_VALUE=iby)
   w_s_e = WIDGET_BUTTON(r5, VALUE='Same  end  date', UVALUE="SE")
   

   r6  = WIDGET_BASE(rf1, /ROW)
   b6  = WIDGET_LABEL(r6,VALUE="To   : ")

   ;w_m_e = WIDGET_DROPLIST(r6, VALUE=month, UVALUE="EM")
   ;w_d_e = WIDGET_DROPLIST(r6, VALUE=day,   UVALUE="ED") 
   ;w_y_e = WIDGET_DROPLIST(r6, VALUE=year,  UVALUE="EY")
   w_m_e = CW_BSELECTOR2(r6, month, UVALUE="EM", SET_VALUE=iem)
   w_d_e = CW_BSELECTOR2(r6, day, UVALUE="ED", SET_VALUE=ied)
   w_y_e = CW_BSELECTOR2(r6, year, UVALUE="EY", SET_VALUE=iey)
   w_s_b = WIDGET_BUTTON(r6, VALUE='Same begin date', UVALUE="SB")

 bl3 = WIDGET_BASE(bl1,/ROW)
 txt = WIDGET_LABEL(bl3,VALUE='                 ')
 b3  = WIDGET_BUTTON(bl3,VALUE='  Submit  ',UVALUE='FIND',FONT=font)



r7 = WIDGET_BASE(lcol, /COL)
lab = WIDGET_LABEL(r7, vALUE="                                                   ",FONT=font)
 lab1= WIDGET_LABEL(r7, vALUE="                                                  ",FONT=font)
 lab2= WIDGET_LABEL(r7, vALUE="                                                  ",FONT=font)

dirs=''

wl_uv = {  WListerBase:ms.WListerBase,   $  ; Base widget ID
           bf:bf,		  $  ; Widget buttons ID
           r7:r7,                 $
           files:files,           $
           dirs:dirs,             $                        
           nname:nn,	          $  ; Widget ID for the File text
           ;npath:np,              $          
           b_day:w_d_b,           $  ; Widget ID for From : day button
           b_month:w_m_b, 	  $  ; Widget ID for From : month button
           b_year:w_y_b,          $  ; Widget ID for From : year button
           e_day:w_d_e, 	  $
           e_month:w_m_e, 	  $
           e_year:w_y_e,          $
           w_s_e:w_s_e,		  $
	   w_s_b:w_s_b,		  $          
           bmonth:bmonth,	  $
           bday:bday,		  $
           byear:byear,		  $
           emonth:emonth,	  $
           eday:eday,		  $
           eyear:eyear,		  $
       ;    seb_id:b63,            $
           ;cid:cid,		  $
           idc:b3,		  $
;           iLevel:iLevel,         $
           lpath2:b64,		  $
        ;   ImgPath:ImgPath,       $
           DatePath:DatePath,     $
           ;Cx:Cx, 		  $
      ;     n_old:n_old,		  $
           lab:lab,		  $
           lab1:lab1,             $
           lab2:lab2,             $
           draw:draw,             $
	   rf1:rf1 }

wl_uv.draw=1
WIDGET_CONTROL, ms.WListerBase, /REALIZE

WIDGET_CONTROL, ms.WListerBase, SET_UVALUE = wl_uv, /NO_COPY

WIDGET_CONTROL, lab, SET_VALUE= 'Is "WISPR" Environment Variable Set Correctly?'
WIDGET_CONTROL, lab1, SET_VALUE= 'WISPR = "'+GETENV('WISPR')+'"' 
;print,qtel, qreg, qexpt, qxsize, qysize, qbin, qtarg, qid, qgain
XMANAGER,"wisprlister",ms.WListerBase, $               
                EVENT_HANDLER = "wisprlister_event", $
                GROUP_LEADER = GROUP, MODAL = keyword_set(modal)





n_matches=n_elements(matches)
if n_matches ne 0 then begin
   matches.filename=wispr_fits_path(matches.filename, final=1-rel_ind)
   return, matches[sort(matches.filename)].filename
endif else begin
  return, ''
endelse
endelse
end



