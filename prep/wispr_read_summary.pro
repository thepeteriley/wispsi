function wispr_read_summary, orbit, level=level, final=final, sumpath=sumpath, WTS=WTS
;
; Project     : PSP - WISPR
;
; Name        : WISPR_READ_SUMMARY()
;
; Purpose     : Extract Summary File Data
;
; Category    : WISPR
;
; Explanation : Locates, reads and returns data from WISPR SUMMARY FILE
;
; Syntax      : data = WISPR_READ_SUMMARY( Orbit  [, keywords ])
;
; Examples    : data = wispr_read_summary('01')
;		data = wispr_read_summary('01', /wts)
;
; Inputs      : Orbit:		String Containing Desired Orbit (Must be ##, i.e. '01')	
;
; Opt. Inputs : None
;
; Outputs     : Structure array containing all data from summary file
;
; Opt. Outputs: None.
;
; Keywords    : Level:		For choosing data level. Default is 'L3', 'L1', 'L2' and 'CAL1' are other options
;		/FINAL:		If set, search for final data summary, otherwise assume QL
;		sumpath:	Path to search for summary files. If not set will default to $WISPR subdirectories
;		WTS:		If set get WTS files, otherwise synoptic data is used
;
; Env. Vars.  : WISPR = location of ql/ and/or fin/
;
; Calls       : None
;
; Common      : None.
;
; Limitations:  Summary file for desired type and level must exit in desired path
;
; Side effects: None.
;
; Prev. Hist. : None.
;
; Written     : Phillip Hess NRL 2019
;
;
;
;
;-


if keyword_set(FINAL) then fin='rel' else fin='ql'

if ~keyword_set(sumpath) then sumpath=getenv_slash('WISPR')+fin+'/fits/summary/'
if ~keyword_set(level) then level='L3'

if ~keyword_set(WTS) then file=file_search(sumpath+'summary_orbit'+orbit+'_'+level+'.txt') else file=file_search(sumpath+'summary_orbit'+orbit+'_'+level+'_WTS.txt')

if file eq '' then begin
	PRINT, 'SUMMARY NOT FOUND'
	RETURN, ''
endif ELSE BEGIN


template={version:1.0, datastart:1L, delimiter:32B, missingvalue:!values.f_nan, commentsymbol:'', fieldcount:14L,fieldtypes:[7L,3,3,3,3,4,3,7,4,4,7,3,3,7], fieldnames:['FILENAME','DETREG','NAXIS1','NAXIS2','NSUMEXP','XPOSURE','NBIN','GAINMODE','DET_T','DATAMDN','TARGET','STUDY_ID','SSRPRIORITY','OSFILE'], fieldlocations:[0L,40,44, 50, 57,62, 71, 74, 77,88,94,104,109,112], fieldgroups:indgen(14, /long)}

data=read_ascii(file, template=template)

result0={filename:'', detector:0, region:0, naxis1:0,naxis2:0, nsumexp:0, xposure:0.0, nbin:0, gainmode:'', det_t:0.0, datamdn:0.0, target:'', study_id:0, ssrpriority:0, osfile:''}

n=n_elements(data.filename)
result=replicate(result0, n)

for i=0, n-1 do begin
    result[i].filename=data.filename[i]
    result[i].detector=data.detreg[i]/10
    result[i].region=data.detreg[i] mod 10
    result[i].naxis1=data.naxis1[i]
    result[i].naxis2=data.naxis2[i]
    result[i].nsumexp=data.nsumexp[i]
    result[i].xposure=data.xposure[i]
    result[i].nbin=data.nbin[i]
    result[i].gainmode=data.gainmode[i]
    result[i].det_t=data.det_t[i]
    result[i].datamdn=data.datamdn[i]
    result[i].target=data.target[i]
    result[i].study_id=data.study_id[i]
    result[i].ssrpriority=data.ssrpriority[i]
    result[i].osfile=data.osfile[i]
endfor

return, result
endelse
end
