function wispr_mk_frame, file, hdr, base_file=base_file, base_hdr=base_hdr,ratio=ratio, rdiff=rdiff, med_filt=med_filt, sigma=sigma, timestamp=timestamp, outsize=outsize, subfield=subfield, range=range, bytscl=bytscl, display=display, log_scale=log_scale, med_scale=med_scale, no_scale=no_scale, hist_equal=hist_equal, expo=expo, mulfac=mulfac, addfac=addfac
;+
;
; Project   : WISPR
;                   
; Name      : wispr_mk_frame
;               
; Purpose   : Make WISPR L2/L3 frame           
;
; Use       : IDL> wisprim=wispr_mk_frame(file, /log_scale)
;    
; Inputs    : 	file	   - WISPR Fits file or image
;	
; Optional Input:
;             	hdr	   - if file is image, this is the associated header
;               
; Outputs   : 	processed WISPR image
;
; Side Effects : 
;
; Keywords  :   /base_file  -  base image or file for ratio or difference image
;		/base_hdr  -  if base_file is image, associated header
;		/ratio  -  make ratio image
;		/rdiff  -  make running difference image
;		/outsize  -  specify output size
;		/subfield -  specify subfield for image
;		/bytscl - bytscl output image
;		/range - specify scaling range
;		/sigma  -  if not 0, applies sigma filter of given value. If array of values, applies multiple sigma filters
;		/med_filt  -  if not 0, apply a median filter of given value
;		/log_scale  -  use log scale
;		/med_scale - scale image by hdr.datamdn
;		/timestamp - add timestep to image
;		/expo - scaling exponent
;		/mulfac - scaling multiplier
;		/addfac - scaling additive
;		IF USING SCALING FACTORS, THEY WILL HAVE THE EFFECT OF:
;		IMG_OUT=(IMG*MULFAC+ADDFAC)^EXPO
;
; Calls     :   
;
; Category    : Display
;               
; Prev. Hist. : None.
;
; Written     : Phillip Hess NRL 2021
;               
;
;-

if typename(file) eq 'STRING' then wispreadfits, file, hdr, im else im=file
if keyword_set(mulfac) then im=im*mulfac
if keyword_set(addfac) then im=im+addfac
if keyword_set(expo) then im=im^expo
if keyword_set(med_scale) then im=im/hdr.datamdn
if keyword_set(log_scale) then im=alog10(im)

if keyword_set(subfield) then begin
    if subfield[0] lt 0 or subfield[1] lt 0 or subfield[2] gt hdr.naxis2-1 or subfield[3] gt hdr.naxis2-1 then begin
	message, 'Invalid Subsript Range for Subfield', /inform
        return, -1
    endif else begin
        im=im[subfield[0]:subfield[2], subfield[1]:subfield[3]]
        outsize=[subfield[2]-subfield[0]+1, subfield[3]-subfield[1]+1]
    endelse
endif else begin 
    if keyword_set(outsize) then im=congrid(im, outsize[0], outsize[1]) else outsize=[hdr.naxis1, hdr.naxis2]
endelse

if keyword_set(sigma) then begin
  
   n=n_elements(sigma)
   for i=0, n-1 do im=sigma_filter(im, sigma[i])
endif
if keyword_set(med_filt) then im=median(im, med_filt)


if keyword_set(ratio) or keyword_set(rdiff) then begin
   if ~keyword_set(base_file) then begin
        message, 'No base file entered, cannot use difference or ratio', /inform
        return, -1
   endif
   if typename(base_file) eq 'STRING' then wispreadfits, base_file, base_hdr, base_im else base_im=base_file
   if keyword_set(med_scale) then base_im=base_im/base_hdr.datamdn
   if keyword_set(log_scale) then base_im=alog10(base_im)
   if keyword_set(subfield) then base_im=base_im[subfield[0]:subfield[2], subfield[1]:subfield[3]]

   base_im=congrid(base_im, outsize[0], outsize[1])
  ; if hdr.nbin ne base_hdr.nbin then stop
   if keyword_set(sigma) then begin
  
      n=n_elements(sigma)
      for i=0, n-1 do base_im=sigma_filter(base_im, sigma[i])
   endif
   if keyword_set(med_filt) then base_im=median(base_im, med_filt)
   ;stop
   if keyword_set(ratio) then im=im/base_im
   if keyword_set(rdiff) then im=im-base_im
   if keyword_set(log_scale) then im(where(im eq 0))=-50
endif
;stop

if keyword_set(no_scale) then return, im
if keyword_set(hist_equal) then begin
 rangeo=sigrange(im, fraction=.95)
 imbs=bytscl(im, min(rangeo, /nan), max(rangeo, /nan))
print, 'Bmin:'+string(min(rangeo, /nan))
print, 'Bmax:'+string(max(rangeo, /nan))

endif else begin

if ~keyword_set(range) then begin
   y=histogram(im, nbins=100, locations=x, /nan)
   mx=max(y, loc)
  ;stop
   while mx/total(y) gt .5 do begin

   if loc eq 0 then hr=[x[0], x[loc+1]] else hr=[x[loc-1],x[loc+1]]
  ; print, hr
   y=histogram(im, nbins=100, locations=x, min=hr[0], max=hr[1], /nan)
   mx=max(y, loc)
   endwhile

   cdf=total(y, /cumulative)/total(y)
   r1=where(cdf gt .45)
   r2=where(cdf gt .55)
   if r2[0] eq r1[0] then r2[0]=r2[1]
   range_im=[x[r1[0]], x[r2[0]]] 

endif else begin
   range_im=range
endelse

imbs=bytscl(im, range_im[0], range_im[1])

print, 'Bmin:'+string(range_im[0])
print, 'Bmax:'+string(range_im[1])

endelse

if keyword_set(timestamp) then begin
set_plot, 'Z'
device, set_resolution=[outsize[0],outsize[1]]
tv, imbs
if hdr.detector eq 1 then ds='I' else ds='O'
xyouts, 5, 5, 'WISPR-'+ds+' '+strmid(hdr.date_obs, 0, 10)+' '+strmid(hdr.date_obs, 11, 8), color=255, charsize=round(outsize[1]/250.), font=1, /device
imbs=tvrd()
endif

set_plot, 'X'
if keyword_set(display) then plot_image, imbs
return, imbs
end

   