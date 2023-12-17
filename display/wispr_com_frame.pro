function wispr_com_frame, filei, fileo, hdri, hdro, inonly=inonly, outonly=outonly, prei=prei, preo=preo, prehi=prehi, preho=preho, ratio=ratio, rdiff=rdiff, proj_type=proj_type,  outsize=outsize, irange=irange, orange=orange, sigma=sigma, med_filt=med_filt,log_scale=log_scale, med_scale=med_scale, timestamp=timestamp, radial=radial, hist_equal=hist_equal, iexpo=iexpo, oexpo=oexpo, imulfac=imulfac, omulfac=omulfac, iaddfac=iaddfac, oaddfac=oaddfac, COORD2=coord2
;+
;
; Project   : WISPR
;                   
; Name      : wispr_com_frame
;               
; Purpose   : Combine WISPR-I and WISPR-O on projected grid            
;
; Use       : IDL> com_image=wispr_com_frame(filein, fileout, /log_scale)
;    
; Inputs    : 	filei	   - WISPR-I Fits file or image
;		fileo	   - WISPR-O Fits file or image
;		
;	
; Optional Input:
;             	hdri	   - if filei is image, this is the associated header
;             	hdro	   - if fileo is image, this is the associated header
;               
; Outputs   : 	image array of inputs interpolated onto grid
;
; Side Effects : 
;
; Keywords  :   /imonly  -  only use inner image
;		/outonly -  only use outer image
;		/prei  -  base inner image or file for ratio or difference image
;		/preo  -  base outer image or file for ratio or difference image
;		/prehi  -  if prei is image, associated header
;		/preho  -  if preh is image, associated header
;		/ratio  -  make ratio image
;		/rdiff  -  make running difference image
;		/proj_type - define projection type
;		/outsize  -  specify output size
;		/irange - specify scaling range, if combo, for WISPR-i
;		/orange - specify scaling range for WISPR-O if combo
;		/sigma  -  if not 0, applies sigma filter of given value. If array of values, applies multiple sigma filters
;		/med_filt  -  if not 0, apply a median filter of given value
;		/log_scale  -  use log scale
;		/med_scale - scale image by hdr.datamdn
;		/timestamp - add timestep to image
;		/iexpo - scaling exponent
;		/oexpo - scaling exponent for WISPR-O if combo
;		/imulfac - scaling multiplier
;		/omulfac - scaling multiplier for WISPR-O if combo
;		/iaddfac - scaling additive
;		/oaddfac - scaling additive for WISPR-O if combo
;		IF USING SCALING FACTORS, THEY WILL HAVE THE EFFECT OF:
;		IMG_OUT=(IMG*MULFAC+ADDFAC)^EXPO
;
;
; Calls     :   WISPR_MK_FRAME, FITSHEAD2WCS, WCS_GET_COORD, WCS_GET_PIXEL, WCS_2D_SIMULATE, INTERPOLATE
;
; Category    : Display
;               
; Prev. Hist. : None.
;
; Written     : Phillip Hess NRL 2021
;               
;
;-
;stop

imi=wispr_mk_frame(filei, hdri, base_file=prei, base_hdr=prehi, ratio=ratio, rdiff=rdiff, range=irange,sigma=sigma, med_filt=med_filt, med_scale=med_scale, log_scale=log_scale, expo=iexpo, mulfac=imulfac, hist_equal=hist_equal, addfac=iaddfac) 
imo=wispr_mk_frame(fileo, hdro, base_file=preo, base_hdr=preho, ratio=ratio, rdiff=rdiff, range=orange,sigma=sigma, med_filt=med_filt, med_scale=med_scale, log_scale=log_scale, expo=oexpo, mulfac=omulfac, hist_equal=hist_equal, addfac=oaddfac)


if ~keyword_set(proj_type) then proj_type='hpca'
if ~keyword_set(outsize) then outsize=[1900,1300]

case proj_type of
   'hpca': begin
    wcsi=fitshead2wcs(hdri)
    wcso=fitshead2wcs(hdro)
    coordi=wcs_get_coord(wcsi)
    coordo=wcs_get_coord(wcso)
    
    minx=min([coordi[0,*,*], coordo[0,*,*]])
    maxx=max([coordi[0,*,*], coordo[0,*,*]])
    miny=min([coordi[1,*,*], coordo[1,*,*]])
    maxy=max([coordi[1,*,*], coordo[1,*,*]])
    wcssim=wcs_2d_simulate(outsize[0], outsize[1],type='hcpa', crval=[mean([minx,maxx]), mean([miny,maxy])], cdelt=[float(maxx-minx)/outsize[0], float(maxy-miny)/outsize[1]], cunit='DEG') 
    end
endcase

coord2=wcs_get_coord(wcssim)
pix1=wcs_get_pixel(wcso, coord2)
pix2=wcs_get_pixel(wcsi, coord2)
imcr1=interpolate(imo, pix1[0,*,*], pix1[1,*,*], missing=0, cubic=-.5)
imcr2=interpolate(imi, pix2[0,*,*], pix2[1,*,*], missing=0, cubic=-.5)

; PR change
; coord2 is the HPC coords of the combined image, so 
; if we return this as an keyword parameter then we 
; can use it to contour the combined image on a plot 
; with the correct coordinates. 

if keyword_set(inonly) then imcr1=fltarr(outsize[0], outsize[1])
if keyword_set(outonly) then imcr2=fltarr(outsize[0], outsize[1])


w=where(imcr1 ne 0)
imcr2[w]=0
imcr=reform(imcr1)+reform(imcr2)
imcr(where(imcr eq 0))=!values.f_nan

if keyword_set(radial) then begin
r=reform(coord2[0,*,*])
h=hdri.dsun_obs*sin(r*!dtor)
scl=h^2/min(h)^2
if ~keyword_set(log_scale) then imcr=imcr*scl else imcr=alog10(10^imcr*scl) 
endif




;if keyword_set(hist_equal) then begin
; rangeo=sigrange(imcr, fraction=.95)
; imbs=bytscl(imcr, min(rangeo, /nan), max(rangeo, /nan))
;print, 'Bmin:'+string(min(rangeo, /nan))
;print, 'Bmax:'+string(max(rangeo, /nan));
;endif else begin
;if ~keyword_set(range) then begin
;   y=histogram(imcr[where(imcr ne 0)], nbins=100, locations=x, /nan)
;   mx=max(y, loc)
;   while mx/total(y) gt .5 do begin;
;   if loc eq 0 then hr=[x[0], x[loc+1]] else hr=[x[loc-1],x[loc+1]]
   ;print, hr
;   y=histogram(imcr[where(imcr ne 0)], nbins=100, locations=x, min=hr[0], max=hr[1], /nan)
;   mx=max(y, loc)
;   endwhile

;   cdf=total(y, /cumulative)/total(y)
;   r1=where(cdf gt .45)
;   r2=where(cdf gt .55)
;   if r2[0] eq r1[0] then r2[0]=r2[1]
;   range_im=[x[r1[0]], x[r2[1]]]
;endif else begin
;   range_im=range
;endelse

;w2=where(imcr eq 0)
;imcr[w2]=!values.f_nan

;imbs=bytscl(imcr, range_im[0], range_im[1])
;print, 'Bmin:'+string(range_im[0])
;print, 'Bmax:'+string(range_im[1])
;endelse

imbs=imcr
if keyword_set(timestamp) then begin
set_plot, 'Z'
device, set_resolution=[outsize[0],outsize[1]]
tv, imbs
xyouts, 5, 5, 'WISPR-I:  '+strmid(hdri.date_obs, 0, 10)+' '+strmid(hdri.date_obs, 11, 8), color=255, charsize=round(outsize[1]/250.), font=1, /device
xyouts, 5, 55, 'WISPR-O: '+strmid(hdro.date_obs, 0, 10)+' '+strmid(hdro.date_obs, 11, 8), color=255, charsize=round(outsize[1]/250.), font=1, /device
imcr=tvrd()
set_plot, 'X'
;plot_image, wispr_bytscl(imcr)
imbs=bytscl(imcr)
endif

return,imbs

end
