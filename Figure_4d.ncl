load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin

  f1   = addfile("reg_PDO_vs_SST_1951_2010.nc","r")
  print(f1)

 
;___________________________________________________________________________________________________________________
  sst_rc      = f1->SST_sig(:,:)
 
 

;___________________________________________________________________________________________________________________
;---Recreating jpeg images only works for X11 and PNG.
;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "png"
   wks_type@wkWidth = 2500
   wks_type@wkHeight = 2500 
   pname ="Figure_4d"
   wks = gsn_open_wks("eps",pname)   
  ;gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")   ;
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
  cnres@cnMinLevelValF              = 0.1
  cnres@cnMaxLevelValF              = 0.2
  cnres@cnLevelSpacingF             = 0.005
  cnres@gsnRightString              = ""
  cnres@gsnLeftString               = ""

  cnres@lbLabelBarOn                = True             ; turn off individual cb's 
  ;cnres@lbOrientation               = "Vertical"      
  ;cnres@pmLabelBarWidthF            = 0.07                                          
  ;cnres@pmLabelBarHeightF           = 0.45
  cnres@pmLabelBarWidthF            = 0.63                                          
  cnres@pmLabelBarHeightF           = 0.05
  cnres@lbBoxEndCapStyle            = "TriangleBothEnds"   
  ;cnres@lbBoxEndCapStyle           = "TriangleHighEnd" 
  ;cnres@pmLabelBarOrthogonalPosF = 0.005          ; move label bar closer
  cnres@pmLabelBarOrthogonalPosF = 0.12          ; move label bar closer
  cnres@lbLabelFont                     	= "helvetica"                             
  cnres@lbLabelFontHeightF              	= 0.02  
  ;cnres@lbLabelPosition                 	= "Center"                                     
  ;cnres@lbLabelAlignment                	= "BoxCenters"
  cnres@lbLabelStride            = 4
  cnres@lbTitleOn          = True
  cnres@lbTitleString      = "~F34~0~F~ C ";"kJ/cm^2"
  cnres@lbTitleFontHeightF = 0.018
  cnres@lbTitleFont        = "helvetica"  
  cnres@lbTitlePosition  = "Right"
  cnres@lbTitleDirection = "Across"

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

  cnres@gsnLeftString        = ""  
  ;cnres@gsnLeftStringFontHeightF = 0.013

  cnres@tmXBLabelFontHeightF    = 0.02    ; Make these labels smaller.
  cnres@tmYLLabelFontHeightF    = 0.02    ; Make these labels smaller.

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
  cnres@tmYROn               = False      ; Turn off bottom tickmarks
  
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



  cnres_reg         = cnres
  cnres_reg@cnFillPalette               = cmap(7:17,:)
;_________________________________________________________________________________

  fu_c = addfile("reg_PDO_vs_U_1951_2010.nc","r")
  fv_c = addfile("reg_PDO_vs_V_1951_2010.nc","r")

 
  u_change=fu_c->U(:,:)
  v_change=fv_c->V(:,:)
  
  u_alpha=fu_c->alpha(:,:)
  v_alpha=fv_c->alpha(:,:)
  
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
  ;vcres@vcRefAnnoOrthogonalPosF   = -0.14
  vcres@vcRefAnnoOrthogonalPosF   = -0.4
  vcres@vcRefAnnoFontHeightF      = 0.012
  vcres@vcRefAnnoString2On        = False 
  vcres@vcGlyphStyle             = "CurlyVector"     ; turn on curly vectors
  vcres@vcLineArrowThicknessF     = 0.7               ; change vector thickness
  vcres@gsnRightString            = ""
  vcres@gsnLeftString             = " "
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
  vcres@lbLabelFontHeightF        = 0.011  
  vcres@lbLabelStride             = 2             
  vcres@pmLabelBarWidthF          = 0.08                                          
  vcres@pmLabelBarHeightF         = 0.4

  vcres@tmXBLabelFontHeightF    = 0.05    ; Make these labels smaller.
  vcres@tmYLLabelFontHeightF    = 0.03   ; Make these labels smaller.
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
  vcres_change@vcLineArrowThicknessF     =   0.9                        ;-- make vector lines thicker (default: 1.0)
  vcres_change@vcRefMagnitudeF           = 1.0             ; define vector ref mag
  vcres_change@vcRefLengthF              = 0.08            ; define length of vec ref
  vcres_change@vcMinFracLengthF          = 0.3
  vcres_change@vcMinDistanceF            = 0.03
;_______________________________________________________________________________
plot = gsn_csm_contour_map(wks,sst_rc(:,:),cnres_reg)
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

  EQLINE0    = gsn_add_polyline(wks,plot,lonboxq,latboxq,lnresq)
 

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




  sig_u_plot = gsn_csm_contour(wks,u_alpha,sgres)
  sig_v_plot = gsn_csm_contour(wks,v_alpha,sgres) 

  opt                  = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeHigh     = 17
  
  sig_u_plot = gsn_contour_shade(sig_u_plot,-999.,90.0,opt)
  sig_v_plot = gsn_contour_shade(sig_v_plot,-999.,90.0,opt) 
  

  overlay(plot,sig_u_plot)
  overlay(plot,sig_v_plot)



  uv_Change_plot      = gsn_csm_vector(wks,u_change,v_change,vcres_change)
  overlay(plot,uv_Change_plot)

  draw(plot)
  frame(wks)  
 
  delete(wks)

  ;cmd = "convert -geometry 2500x2500 -density 300 -trim " + pname + ".eps " + \
  ;                                                        pname + ".png"

  cmd = "convert -geometry 2500x2500 -density 300 " + pname + ".eps " + \
                                                          pname + ".png"
  system(cmd)    

end