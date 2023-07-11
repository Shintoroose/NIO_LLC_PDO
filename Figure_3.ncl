load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

  f1   = addfile("Epochal_Change_SST_1951_2010.nc","r")
  print(f1)

 
;___________________________________________________________________________________________________________________
  sst_rc      = f1->difAve_sig(:,:)
 
 
SST_clim=short2flt(f1->clim)
;___________________________________________________________________________________________________________________
;---Recreating jpeg images only works for X11 and PNG.
;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "eps"
   wks_type@wkWidth = 7.28346
   wks_type@wkHeight = 4 
   pname ="SST_850UV"
   wks = gsn_open_wks("eps",pname)   
  ;gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")   ;
  ;gsn_define_colormap(wks,"BrownBlue12")  ;for color blindness
  ;gsn_define_colormap(wks,"CBR_coldhot")  ;for color blindness
  ;gsn_reverse_colormap(wks)
  
  ;gsn_define_colormap(wks,"cmp_b2") ;

 
 
 
   cmap = read_colormap_file("BlueDarkRed18") ;for color blindness
;___________________________________________________________________________________________________________________
  cnres                             = True
  ;cnres@gsnMaximize                = True
  cnres@cnFillDrawOrder             = "PreDraw"       ; draw contours before continents
  cnres@gsnDraw                     = False
  cnres@gsnFrame                    = False     
  cnres@cnLinesOn                   = False
  cnres@cnLineLabelsOn              = False         
  cnres@cnFillOn                    = True

  ;cnres@cnFillColors               = (/30,60,90/)
  cnres@mpFillOn                    = True
  cnres@mpGeophysicalLineColor      = "black"
  cnres@mpGeophysicalLineThicknessF = 2
  ;cnres@mpLandFillColor             = "black"  
  cnres@mpLandFillPattern           = 4 
  cnres@gsnAddCyclic                = False  
  cnres@mpLimitMode                 = "LatLon"		    
  cnres@mpMinLatF                   = -20		  
  cnres@mpMaxLatF                   = 20
  cnres@mpMinLonF                   = 40			   
  cnres@mpMaxLonF                   = 100              
  cnres@cnLevelSelectionMode        = "ManualLevels"
  cnres@cnMinLevelValF              = 0.25
  cnres@cnMaxLevelValF              = 0.65
  cnres@cnLevelSpacingF             = 0.02
  cnres@gsnRightString              = ""
  cnres@gsnLeftString               = ""

  cnres@lbLabelBarOn                = True             ; turn off individual cb's 
  cnres@lbOrientation               = "Vertical"      
  cnres@pmLabelBarWidthF            = 0.07                                          
  cnres@pmLabelBarHeightF           = 0.45
  cnres@lbBoxEndCapStyle            = "TriangleBothEnds"   
  ;cnres@lbBoxEndCapStyle           = "TriangleHighEnd" 
  ;cnres@pmLabelBarOrthogonalPosF = -0.01          ; move label bar closer
  cnres@lbLabelFont                     	= "helvetica"                             
  cnres@lbLabelFontHeightF              	= 0.02  
  ;cnres@lbLabelPosition                 	= "Center"                                     
  ;cnres@lbLabelAlignment                	= "BoxCenters"
  cnres@lbLabelStride            = 4
  cnres@lbTitleOn          = True
  cnres@lbTitleString      = "~F34~0~F~ C ";"kJ/cm^2"
  cnres@lbTitleFontHeightF = 0.022
  cnres@lbTitleFont        = "helvetica"  


  ;cnres@tiXAxisString        = "longitude"    ; x-axis title
  ;cnres@tiYAxisString        = "latitude"     ; y-axis title
  ;cnres@tiYAxisFont 	      = 10
  ;cnres@tiXAxisFont          = 10
  ;cnres@tiXAxisFontHeightF   = 0.018
  ;cnres@tiYAxisFontHeightF   = 0.018

  ;cnres@tmXBLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  ;cnres@tmYLLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  ;cnres@tmXTOn               = False       ; Turn off top tickmarks
  ;cnres@tmYROn               = False      ; Turn off left tickmarks
  ;cnres@tmYROn               = True      ; Turn off bottom tickmarks

  ;cnres@gsnLeftString        = ""  
  ;cnres@gsnLeftStringFontHeightF = 0.013

  cnres@tmXBLabelFontHeightF    = 0.022    ; Make these labels smaller.
  cnres@tmYLLabelFontHeightF    = 0.022    ; Make these labels smaller.

  cnres@tiYAxisFont 	     = "helvetica"
  cnres@tiXAxisFont          = "helvetica"
  
  cnres@tmXBMajorLengthF     = 0.012
  cnres@tmYLMajorLengthF     = 0.012

  cnres@tmYLLabelFont                  = "helvetica"
  cnres@tmXBLabelFont                  = "helvetica"
  cnres@tmXBLabelsOn                   = True

  cnres@tmYRLabelsOn = False
  cnres@tmXBMajorLengthF=0.02
  ;cnres@tmYRMajorLengthF=0.02

  ;cnres@tmXBOn               = True     ; Turn off top tickmarks
  ;cnres@tmXTOn               = False     ; Turn off top tickmarks
  ;cnres@tmYLOn               = False     ; Turn off left tickmarks
  cnres@tmYROn                = False      ; Turn off bottom tickmarks



  cnres@pmTickMarkDisplayMode = "Always"   ; nicer tickmarks
  cnres@mpOutlineBoundarySets = "Geophysical"
  cnres@mpDataSetName         = "Earth..4" 
  cnres@mpDataBaseVersion     = "MediumRes"
  cnres@tmXBLabelFontHeightF    = 0.022    ; Make these labels smaller.
  cnres@tmYLLabelFontHeightF    = 0.022   ; Make these labels smaller.
  cnres@tiYAxisFont 	        = "helvetica"
  cnres@tiXAxisFont             = "helvetica"
  cnres@tmXBMajorLengthF        = 0.01
  cnres@tmYLMajorLengthF        = 0.01
  cnres@tmYLLabelFont           = "helvetica"
  cnres@tmXBLabelFont           = "helvetica"
  cnres@tmXBLabelsOn            = True
  cnres@tmYRLabelsOn = True
  cnres@tmXBMajorLengthF=0.022
  cnres@tmYRMajorLengthF=0.022
  ;res@tmXBOn               = True     ; Turn off top tickmarks
  ;res@tmXTOn               = False     ; Turn off top tickmarks
  ;res@tmYLOn               = False     ; Turn off left tickmarks
  ;res@tmYROn               = True      ; Turn off bottom tickmarks
  
        cnres@tmXBMajorLengthF = 0.01        ; set x axis major tick mark length to 0.01
        cnres@tmYLMajorLengthF = 0.01        ; set y axis major tick mark length to 0.01
        cnres@tmXBLabelFontHeightF = 0.022   ; set x axis tick label font size to 0.01
        cnres@tmYLLabelFontHeightF = 0.022   ; set y axis tick label font size to 0.01
        ;cnres@tmXBMinorOn = False          ; remove x axis tick lines
        ;cnres@tmYLMinorOn = False          ; remove y axis tick lines  
        cnres@tmXTOn = False                ; turn off x-axis tick marks
        cnres@tmYROn = False                ; turn off y-axis tick marks
        cnres@tmXBLabelFontHeightF = 0.022  ; set x-axis label font size
        cnres@tmYLLabelFontHeightF = 0.022  ; set y-axis label font size
        cnres@tmXBMajorOutwardLengthF = -0.02 ; set x-axis tick marks inward
        cnres@tmYLMajorOutwardLengthF = -0.02 ; set y-axis tick marks inward
        cnres@tmXBMinorOutwardLengthF = 0.0 ; set x-axis tick marks inward
        cnres@tmYLMinorOutwardLengthF = 0.0 ; set y-axis tick marks inward
        
        

  cnres_diff         = cnres
  cnres_diff@cnFillPalette               = cmap(7:17,:)
  
  cnres_mean         = cnres
  cnres_mean@cnLevelSelectionMode        = "ManualLevels"
  cnres_mean@cnMinLevelValF              = 27
  cnres_mean@cnMaxLevelValF              = 29
  cnres_mean@cnLevelSpacingF             = 0.1
  cnres_mean@cnFillPalette               = cmap(7:17,:)
 
  
  
 

vort_cnres                             = cnres
;vort_cnres@cnFillPalette               = "MPL_PiYG" 
;vort_cnres@cnFillPalette               = cmap ;for color blindness
vort_cnres@cnFillPalette               = cmap(2:16,:)
vort_cnres@cnLevelSelectionMode        = "ManualLevels"
vort_cnres@cnMinLevelValF              = -0.35
vort_cnres@cnMaxLevelValF              = 0.35
vort_cnres@cnLevelSpacingF             = 0.05
vort_cnres@lbTitleOn          = True
vort_cnres@lbTitleString      = "s~S~-1~N~" ;"kJ/cm^2"
vort_cnres@lbTitleFontHeightF = 0.025
vort_cnres@lbTitleFont        = "helvetica" 
vort_cnres@mpLimitMode                 = "LatLon"		    
vort_cnres@mpMinLatF                   = 0		  
vort_cnres@mpMaxLatF                   = 15
vort_cnres@mpMinLonF                   = 75			   
vort_cnres@mpMaxLonF                   = 100 
vort_cnres@gsnLeftString             = "b  Epochal change in absolute vorticity"

VWS_cnres                             = cnres
;VWS_cnres@cnFillPalette               = cmap ;for color blindness
VWS_cnres@cnFillPalette               = cmap(2:16,:)
VWS_cnres@cnLevelSelectionMode        = "ManualLevels"
VWS_cnres@cnLevelSelectionMode        = "ManualLevels"
VWS_cnres@cnMinLevelValF              = -6
VWS_cnres@cnMaxLevelValF              = 6
VWS_cnres@cnLevelSpacingF             = 1.0
VWS_cnres@lbTitleOn          = True
VWS_cnres@lbTitleString      = "ms~S~-1~N~" ;"kJ/cm^2"
VWS_cnres@lbTitleFontHeightF = 0.025
VWS_cnres@lbTitleFont        = "helvetica" 
VWS_cnres@mpLimitMode                 = "LatLon"		    
VWS_cnres@mpMinLatF                   = 0		  
VWS_cnres@mpMaxLatF                   = 15
VWS_cnres@mpMinLonF                   = 75			   
VWS_cnres@mpMaxLonF                   = 100 
VWS_cnres@gsnLeftString             = "a  Epochal change in vertical wind shear"


;_________________________________________________________________________________

  f_U = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/Epochal_Diff/Clim_and_Change/Wind_850/Epochal_change_U850.nc","r")
  f_V = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/Epochal_Diff/Clim_and_Change/Wind_850/Epochal_change_V850.nc","r")
 

print(f_U)
  u_clim=f_U->Clim(:,:)
  v_clim=f_V->Clim(:,:)


 
  u_change=f_U->change(:,:)
  v_change=f_V->change(:,:)
  
  u_alpha=f_U->alpha(:,:)
  v_alpha=f_V->alpha(:,:)
  
  copy_VarMeta(u_change,u_alpha)
  copy_VarMeta(v_change,v_alpha)

;-------------------------------------


;_______________________________________________________________________________________________________
  vcres                         = True
  vcres@gsnDraw                 = False
  vcres@gsnFrame                = False

  ;vcres@sfXCStartV = 40 
  ;vcres@sfXCEndV   = 100 
  ;vcres@sfYCStartV = -20 
  ;vcres@sfYCEndV   = 20 

  vcres@mpLimitMode                 = "LatLon"		    
  vcres@mpMinLatF                   = -20		  
  vcres@mpMaxLatF                   = 20
  vcres@mpMinLonF                   = 40			   
  vcres@mpMaxLonF                   = 110    

  ;vcres@vcLineArrowThicknessF     =   0.5                         ;-- make vector lines thicker (default: 1.0)
  vcres@vcRefMagnitudeF           = 5.0             ; define vector ref mag
  vcres@vcRefLengthF              = 0.05            ; define length of vec ref
  vcres@vcMinFracLengthF          = 0.3
  vcres@vcMinDistanceF            = 0.03
  vcres@vcRefAnnoOrthogonalPosF   = -0.22
  vcres@vcRefAnnoFontHeightF      = 0.012
  vcres@vcRefAnnoString2On        = False 
  vcres@vcGlyphStyle             = "CurlyVector"     ; turn on curly vectors
  vcres@vcLineArrowThicknessF     = 1.2               ; change vector thickness
  vcres@gsnRightString            = ""
  vcres@gsnLeftString             = "c  Winds (850 hPa) and SST"
  vcres@gsnLeftStringFontHeightF  = 0.03
  vcres@vcRefAnnoArrowUseVecColor = False           ; don't use vec color for ref
  ;vcres@vcVectorDrawOrder         = "PreDraw" ; draw order 
  vcres@vcFillArrowsOn            = True
  ;vcres@vcFillArrowFillColor     = "tomato4"
  vcres@vcMonoFillArrowFillColor = True     ; Color according to magnitude
 

  vcres@vcLevelSelectionMode	 = "ManualLevels"
  vcres@vcMinLevelValF           = 0.0
  vcres@vcMaxLevelValF           = 2.5
  vcres@vcLevelSpacingF          = 0.1

  vcres@lbLabelBarOn              = False          ; turn off individual cb's 
  vcres@lbOrientation             = "Vertical"
  ;vcres@lbBoxEndCapStyle         = "TriangleBothEnds" 
  vcres@lbBoxEndCapStyle          = "TriangleHighEnd" 
  ;vcres@lbLabelFont               = "helvetica"                              ; label font
  vcres@lbLabelFontHeightF        = 0.022  
  vcres@lbLabelStride             = 2             
  vcres@pmLabelBarWidthF          = 0.08                                          
  vcres@pmLabelBarHeightF         = 0.4

  ;vcres@tmXBLabelFontHeightF    = 0.05    ; Make these labels smaller.
  ;vcres@tmYLLabelFontHeightF    = 0.03   ; Make these labels smaller.
  vcres@tiYAxisFont 	        = "helvetica"
  vcres@tiXAxisFont             = "helvetica"

  vcres@tmXBMajorLengthF        = 0.03
  vcres@tmYLMajorLengthF        = 0.04

  vcres@tmYLLabelFont           = "helvetica"
  vcres@tmXBLabelFont           = "helvetica"
  vcres@tmXBLabelsOn            = True

  ;vcres@tmYRLabelsOn = True
  ;vcres@tmXBMajorLengthF=0.015
  ;vcres@tmYRMajorLengthF=0.015
  ;vcres@tmXBOn               = True     ; Turn off top tickmarks
  ;vcres@tmXTOn               = False     ; Turn off top tickmarks
  ;vcres@tmYLOn               = False     ; Turn off left tickmarks
  ;vcres@tmYROn               = True      ; Turn off bottom tickmarks


  ;vcres@vcVectorDrawOrder = "PreDraw" ; draw order 

  vcres_change  = vcres
  ;vcres_change@vcLineArrowThicknessF     =   0.5                         ;-- make vector lines thicker (default: 1.0)
  vcres_change@vcRefMagnitudeF           = 1.0             ; define vector ref mag
  vcres_change@vcRefLengthF              = 0.06            ; define length of vec ref
  vcres_change@vcMinFracLengthF          = 0.3
  vcres_change@vcMinDistanceF            = 0.03
  vcres_change@gsnLeftString             = "d  Epochal change in Winds (850 hPa) and SST"
  vcres_change@gsnLeftStringFontHeightF  = 0.03
 

;_______________________________________________________________________________________
 

   fAbs_925_change     = addfile("/Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/Sig_Eppochal_diff/Eta/EpochalDiff_ABSVORT.nc","r")

   print(fAbs_925_change)

   etadiff = fAbs_925_change->change(:,:)
 
   etadiff&LATITUDE@units="degrees_north"
   etadiff&LONGITUDE@units="degrees_east"
 


;_______________________________________________________________________________________
 

   VWS_change     = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/Epochal_Diff/Clim_and_Change/VShear/EpochalDiff_VSHEAR.nc","r")

   print(VWS_change)

   VWSdiff = VWS_change->change(:,:)
 
   VWSdiff&LATITUDE@units="degrees_north"
   VWSdiff&LONGITUDE@units="degrees_east"
 
;__________________________________________________________________
;__________________________________________________________________


 ;_______________________________________________________________________________
  plot    = new(4,graphic)                             ; create graphics array

  plot(0) = gsn_csm_contour_map(wks,VWSdiff,VWS_cnres) 
  plot(1) = gsn_csm_contour_map(wks,etadiff,vort_cnres) 
  plot(2) = gsn_csm_contour_map(wks,SST_clim(:,:),cnres_mean)
  plot(3) = gsn_csm_contour_map(wks,sst_rc(:,:),cnres_diff)



 
 
  uv_clim      = gsn_csm_vector(wks,u_clim,v_clim,vcres)
  overlay(plot(2),uv_clim)
;----------------------------------------------

;---Overlay a red box
  lnres                  = True
  lnres@gsLineColor      = "red" ; red box 
  lnres@gsLineThicknessF = 2.0   ; make box thicker
  lnres@gsLineDashPattern = 2

  lonboxb = (/ 50, 90, 90, 50, 50/)
  latboxb = (/-3,-3,2,2,-3/)
  ;box1 = gsn_add_polyline(wks,plot(0),lonboxb,latboxb,lnres)
  ;box2 = gsn_add_polyline(wks,plot(1),lonboxb,latboxb,lnres)
;---Equator line
  lnresq                  = True
  lnresq@gsLineColor      = "blue" 
  lnresq@gsLineThicknessF = 1.0    
  lnresq@gsLineDashPattern = 1
  lonboxq = (/ 0, 180, 180, 0, 0/)
  latboxq = (/0,0,0,0,0/)

  EQLINE0    = gsn_add_polyline(wks,plot(2),lonboxq,latboxq,lnresq)
  EQLINE1    = gsn_add_polyline(wks,plot(3),lonboxq,latboxq,lnresq)

;------------------------------
; plot statistical significance
;------------------------------

  sgres                      = True		; significance
  sgres@gsnDraw              = False		; draw plot
  sgres@gsnFrame             = False		; advance frome
  sgres@cnInfoLabelOn        = False		; turn off info label
  sgres@cnLinesOn            = False		; draw contour lines
  sgres@cnLineLabelsOn       = False		; draw contour labels
  sgres@cnFillScaleF         = 0.9		; add extra density
  sgres@cnFillDotSizeF       = 0.002

  sgres@gsnAddCyclic         = False

; activate if gray shading for B&W plot  
  sgres@cnFillOn             = True
  sgres@cnFillColors         = (/"transparent","transparent"/) ; choose one color for our single cn level
  sgres@cnLevelSelectionMode = "ExplicitLevels"	         ; set explicit contour levels
  sgres@cnLevels             = 90.0                            ; only set one level
  sgres@lbLabelBarOn         = False

  sgres@tiMainString         = ""     ; title
  sgres@gsnCenterString      = ""  ; subtitle
  sgres@gsnLeftString        = ""    ; upper-left subtitle
  sgres@gsnRightString       = ""   ; upper-right subtitle

;---------------------------------------------------------Wind Sig
  u_alpha=f_U->alpha(:,:)
  v_alpha=f_V->alpha(:,:)
 
  u_alpha@_FillValue = -9.999999999999999e+33
  v_alpha@_FillValue = -9.999999999999999e+33

  sig_u_plot = gsn_csm_contour(wks,u_alpha,sgres)
  sig_v_plot = gsn_csm_contour(wks,v_alpha,sgres) 

  opt                  = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeHigh     = 17
  
  sig_u_plot = gsn_contour_shade(sig_u_plot,-999.,90.0,opt)
  sig_v_plot = gsn_contour_shade(sig_v_plot,-999.,90.0,opt) 
  

  overlay(plot(3),sig_u_plot)
  overlay(plot(3),sig_v_plot)

  uv_Change_plot      = gsn_csm_vector(wks,u_change,v_change,vcres_change)
  overlay(plot(3),uv_Change_plot)
;---------------------------------------------------------



;---------------------------------------------------------eta Sig
  eta_alpha=fAbs_925_change->alpha(:,:) 
  eta_alpha@_FillValue = -9.999999999999999e+33
 

  sig_eta_plot = gsn_csm_contour(wks,eta_alpha,sgres)

  opt                  = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeHigh     = 17
  
  sig_eta_plot = gsn_contour_shade(sig_eta_plot,-999.,90.0,opt)
 
  overlay(plot(1),sig_eta_plot) 

;---------------------------------------------------------VWS sig
  VWS_alpha=VWS_change->alpha(:,:) 
  VWS_alpha@_FillValue = -9.999999999999999e+33
 

  sig_VWS_plot = gsn_csm_contour(wks,VWS_alpha,sgres)

  opt                  = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeHigh     = 17
  
  sig_VWS_plot = gsn_contour_shade(sig_VWS_plot,-999.,90.0,opt)
 
  overlay(plot(0),sig_VWS_plot) 
;---------------------------------------------------------






;---------------------------------------------------------
;---Add dots and text.
lonE1  = (/91.21, 86.26, 89.76, 93.13, 90.41, 90.62, 88.45, 83.09, 91.30, 86.05, 88.50, 92.23, 84.00, 91.97, 92.43, 95.00, 93.48, 94.86, 86.62, 84.55, 89.06, 93.37, 90.82, 91.33, 89.20, 85.07, 87.60, 90.34, 92.48, 92.34, 92.83, 88.10, 90.20, 89.28, 93.30, 87.90, 90.29, 88.20, 86.54, 90.40, 87.30, 89.50, 88.31, 88.57, 84.87, 91.15/)


lonE2  = (/83.88, 87.29, 85.05, 85.00, 93.50, 85.83, 87.84, 86.94, 94.26, 87.47, 87.21, 91.64, 92.05, 91.75, 87.14, 91.66, 92.40, 85.93, 90.51, 88.56, 88.06, 89.55, 86.08, 90.74, 91.20/)


latE1  = (/5.84, 10.05,  7.41, 10.47,  9.94,  8.31, 10.92, 10.37, 10.84,  9.19,  9.89,  6.57,  9.05, 8.79,  7.27,  7.87,  8.16,  9.05, 10.06,  7.86,  8.63,  9.73, 9.28,  6.82, 10.53,  8.73, 10.78, 8.08,  6.07,  7.70,  6.23,  7.60,  7.52,  9.72,  5.00,  7.18,  5.66,  7.41,  5.05,  5.27,  5.00,  9.91,  7.22,  7.52, 9.45, 10.02/)
 
latE2  = (/9.08, 10.80, 8.96,  6.50, 10.64,  8.04, 9.38, 8.96, 8.77, 11.00, 7.38, 6.20, 10.47, 8.42, 10.35, 10.41, 9.25, 10.92, 8.88, 7.89, 6.81, 7.89, 7.84, 5.32, 10.41/)

;---------------------------------------------------------
mkres = True
mkres@gsMarkerIndex = 16 
mkres@gsMarkerColor = "red"  ;Change marker color.
mkres@gsMarkerSizeF =  5.0   ;Increase marker size.

mkres2 = True
mkres2@gsMarkerIndex = 16 
mkres2@gsMarkerColor = "blue"   ;Change marker color.
mkres2@gsMarkerSizeF =  5.0      ;Increase marker size.

GenesisLocation_E1 = gsn_add_polymarker(wks,plot(1),lonE1,latE1,mkres)
GenesisLocation_E2 = gsn_add_polymarker(wks,plot(1),lonE2,latE2,mkres2)
;---------------------------------------------------------
;__________________________________________________________________
  
  ;---Equator line
  lnresq                   = True
  lnresq@gsLineColor       = "black" 
  lnresq@gsLineThicknessF  = 2.0    
  lnresq@gsLineDashPattern = 1
  lonboxq = (/ 0, 180, 180, 0, 0/)
  latboxq = (/0,0,0,0,0/)

  ;EQLINE0    = gsn_add_polyline(wks,plot(2),lonboxq,latboxq,lnresq)


;---------------------------------------------------------
gres                  = True
gres@gsLineColor       = "Blue"   ; Polyline resources.
gres@gsLineThicknessF  = 3.0      ; thrice thickness
gres@gsFillColor       = "Red"    ; Polygon resources.
gres@gsFillIndex       = -1
;gres@gsFillOpacityF    = 2
gres@gsEdgesOn         = True     ; Line will be drawn around polygon.
gres@gsEdgeColor       = "black"
gres@gsEdgeThicknessF  = 2
gres@gsMarkerIndex     = 16       ; dots   # Polymarker resources.
gres@gsMarkerColor     = "HotPink"
gres@gsMarkerSizeF     = 0.014    ; twice normal size

xsquare = (/83.0,95.0,95.0,83.0,83.0/)    ; X and Y coordinates
ysquare = (/5,5, 11, 11, 5/)    ; for polygon.

Box0  = gsn_add_polygon(wks,plot(0),xsquare,ysquare,gres)
Box1  = gsn_add_polygon(wks,plot(1),xsquare,ysquare,gres)
Box2  = gsn_add_polygon(wks,plot(2),xsquare,ysquare,gres)
Box3  = gsn_add_polygon(wks,plot(3),xsquare,ysquare,gres)
;---------------------------------------------------------
 

;************************************************
; Attach text to plot using plot coordinates.
;************************************************
  txres               = True                     ; text mods desired
  txres@txFontHeightF = 0.022                     ; font smaller. default big
  txres@txFontColor   = "red"
  dum2_1 = gsn_add_text(wks,plot(1),"Epoch-1 : 46 LLCs",90,3.5,txres) 
  txres@txFontColor   = "blue"
  dum2_2 = gsn_add_text(wks,plot(1),"Epoch-2 : 26 LLCs",90,2.0,txres) 



;----------------------------------------------------------------------
; Draw both plots in one panel.
;----------------------------------------------------------------------
  ppres                     = True
  ppres@gsnMaximize         = True
  ;ppres@gsnPanelMainString  = "WRF output data - " + tc@description + " (" + tc@units + ")"
  ppres@gsnPanelLabelBar    = True
  ppres@gsnPanelRowSpec     = True
  ppres@pmLabelBarWidthF    = 0.7


  resP                  	        = True                   	                ; modify the panel plot
  resP@gsnFrame                         = True                   	                ; don't advance panel plot
  resP@gsnPanelLabelBar 	        = True       

  resP@gsnPanelLabelBar 	        = False                   	                ; add common colorbar
  resP@lbBoxEndCapStyle                 = "TriangleBothEnds"

  resP@gsnPanelLabelBar          = False                ; add common colorbar
  resP@lbLabelFontHeightF        = 0.006               ; make labels smaller
  resP@lbLabelFont               = "helvetica"
  ;resP@lbTitleOn                 = True
  resP@lbLabelStride             = 2
  ;resP@lbTitleString             = "mm ~NN~ day~S~-1" ;"kJ/cm^2"
  ;resP@lbTitleFontHeightF        = 0.008
  ;resP@lbTitleFont               = "helvetica"
  ;resP@lbTitlePosition           = "Bottom"
  ;resP@pmLabelBarOrthogonalPosF  = -0.1
  ;resP@pmLabelBarWidthF            = 0.04                                         
  ;resP@pmLabelBarHeightF           = 0.15 
  ;resP@lbOrientation              = "Vertical"     ; vertical label bar
  ;resP@gsnPanelFigureStringsFont = "helvetica"
  ;resP@gsnPanelBottom            = 0.05                 ; add space at bottom


  ;resP@pmLabelBarOrthogonalPosF = .01
  ;resP@pmLabelBarParallelPosF = .04
  ;resP@lbLabelFontHeightF  = 0.015     ; make labels smaller
  ;resP@lbTitleFontHeightF   =0.015



  ;resP@gsnPanelRowSpec                  = True                                          ; tell panel what order to plt

  ;resP@amJust   		        = "TopLeft"
  ;resP@gsnPanelFigureStringsFontHeightF = 0.009
  ;resP@gsnPanelFigureStringsBackgroundFillColor = "aliceblue"
  ;resP@gsnPanelFigureStringsFont        = "helvetica"

  resP@gsnPanelYWhiteSpacePercent = 0.0
  resP@gsnPanelXWhiteSpacePercent = 2.9

  gsn_panel(wks,plot,(/2,2/),resP)

  ;draw(plot)
  ;frame(wks)  

  delete(wks)

  ;cmd = "convert -geometry 2500x2500 -density 300 -trim " + pname + ".eps " + \
  ;                                                        pname + ".png"

  ;cmd = "convert -geometry 5000x5000 -density 300 -trim " + pname + ".eps " + pname + "b.eps"
  ;system(cmd)    

end
 