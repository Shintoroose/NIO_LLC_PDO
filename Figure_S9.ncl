
begin





 
 
 ;---Recreating jpeg images only works for X11 and PNG.
;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "eps"
   wks_type@wkWidth = 7.28346
   wks_type@wkHeight = 4 
   pname ="FigS9"
   wks = gsn_open_wks("eps",pname)   
  ;gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")   ;
  ;gsn_define_colormap(wks,"BrownBlue12")  ;for color blindness
  ;gsn_define_colormap(wks,"CBR_coldhot")  ;for color blindness
  ;gsn_reverse_colormap(wks)
  
  ;gsn_define_colormap(wks,"cmp_b2") ;

 
 
 
   cmap = read_colormap_file("BlueDarkRed18") ;for color blindness
 
 
;_______________________________________________________________________________________


   f_change     = addfile("EpochalDiff_GPI.nc","r")


print(f_change)


   gpidiff = f_change->change(:,:)
 


   ttest_Abs      = f_change->alpha(:,:)
   ttest_Abs@_FillValue = -9.999999999999999e+33


   gpidiff&LATITUDE1@units="degrees_north"
   gpidiff&LONGITUDE1@units="degrees_east"


   ;gpidiff= mask(gpidiff,(ttest_Abs.ge.90),True) ;90% 



 
 
;__________________________________________________________________
; create the plot
;__________________________________________________________________
;___________________________________________________________________________________________________________________

  ;gsn_define_colormap(wks,"MPL_PiYG") ;

  cnres                             = True
  cnres@gsnMaximize                 = False
  ;cnres@cnFillDrawOrder             = "PreDraw"       ; draw contours before continents
  cnres@gsnDraw                     = False
  cnres@gsnFrame                    = False     
  cnres@cnLinesOn                   = False
  cnres@cnLineThicknessF            = 0.5
  cnres@cnLineLabelsOn              = False         
  cnres@cnFillOn                    = True

  ;cnres@mpFillOn                    = False
  ;cnres@mpGeophysicalLineColor      = "black"
  cnres@mpGeophysicalLineThicknessF  = 1
  ;cnres@mpLandFillColor             = "cornsilk" ;"black"      ;darkolivegreen ;goldenrod4
  ;cnres@mpLandFillPattern           = 4 
  cnres@gsnAddCyclic                = False  
  cnres@mpLimitMode                 = "LatLon"		    
  cnres@mpMinLatF                   = 0		  
  cnres@mpMaxLatF                   = 15
  cnres@mpMinLonF                   = 75			   
  cnres@mpMaxLonF                   = 100              
  cnres@cnLevelSelectionMode        = "ManualLevels"
  cnres@cnMinLevelValF              = -1.5
  cnres@cnMaxLevelValF              = 1.5
  cnres@cnLevelSpacingF             = 0.15
  cnres@gsnRightString              = ""
  cnres@gsnLeftString               = ""
  cnres@tiMainString                = "Epochal change in Genesis Potential Index"
  cnres@tiMainOffsetYF              = -0.03
  cnres@tiMainFontHeightF           = 0.018
  cnres@tiMainFont                  = "helvetica"

  cnres@tiXAxisString        = " "    ; x-axis title
  cnres@tiYAxisString        = " "    ; y-axis title
  cnres@tiYAxisOffsetXF       = 0.018
  cnres@tiXAxisOffsetYF       = 0.085
  cnres@tiYAxisFont 	     = "helvetica"
  cnres@tiXAxisFont          = "helvetica"
  cnres@tiXAxisFontHeightF = 0.018
  cnres@tiYAxisFontHeightF = 0.018

  
   cnres@tmXBMajorLengthF     = 0.01
   cnres@tmYLMajorLengthF     = 0.01
; now change the size of the tickmark labels
  cnres@tmXBLabelFontHeightF = 0.022                 ; resize tick labels
  cnres@tmYLLabelFontHeightF = 0.022    
  cnres@tmYLLabelFont                  = "helvetica"
  cnres@tmXBLabelFont                  = "helvetica"

  cnres@txFontHeightF = 0.022


  cnres@lbLabelBarOn                = True            ; turn off individual cb's 
  cnres@lbBoxEndCapStyle            = "TriangleBothEnds"  
  ;cnres@lbBoxEndCapStyle           = "TriangleHighEnd" 
  cnres@pmLabelBarWidthF            = 0.4                                          
  cnres@pmLabelBarHeightF           = 0.045 
  ;cnres@lbOrientation              = "Vertical"     ; vertical label bar
  cnres@pmLabelBarOrthogonalPosF    = 0.19          ; move label bar closer
  cnres@lbLabelFont                 = "helvetica"

  cnres@pmTickMarkDisplayMode = "Always"            ; turn on built-in tickmarks


  cnres@lbLabelStride            = 2
  ;cnres@lbTitleOn          = True
  ;cnres@lbTitleString      = "10~S~-6" ;"kJ/cm^2"
  ;cnres@lbTitleFontHeightF = 0.015
  ;cnres@lbTitleFont        = "helvetica" 

  cnres@tmXBLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  cnres@tmYLLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  ;cnres@tmXTOn               = False       ; Turn off top tickmarks
  ;cnres@tmYROn               = False      ; Turn off left tickmarks
  ;cnres@tmYROn               = True      ; Turn off bottom tickmarks

  cnres@gsnLeftString        = ""  
  ;cnres@gsnLeftStringFontHeightF = 0.013
  

  
  cnres@cnFillPalette               = cmap(2:16,:)
;_________________________________________________________________________________

plotabs = gsn_csm_contour_map(wks,gpidiff,cnres)

;----------------------------------------------------------------------
	mres = True                                    ; create a resource list for contour plots
        mres@gsnDraw          = False              ; don't draw
        mres@gsnFrame         = False              ; don't advance frame
	mres@cnLevelSelectionMode = "ExplicitLevels"   ; explicitly select the levels
	mres@cnLineColor          = "blue"             ; contour the lines blue
	mres@gsnContourLineThicknessesScale = 1
	mres@cnLineLabelsOn       = False              ; turn off the line labels
	mres@cnInfoLabelOn        = False              ; don't draw the info label
	mres@cnMonoLineDashPattern = True
	mres@cnLineDashPatterns = (/0,0,2,2/)
	mres@sfXArray             = gpidiff&LONGITUDE1 ; uses lon_BEWOD as plot x-axis
	mres@sfYArray             = gpidiff&LATITUDE1   ; uses z_BEWOD as plot y-axis 
	mres@gsnYAxisIrregular2Linear = True ; converts irreg depth to linear 
	mres@trYReverse=True                 ; reverses y-axis
	mres@cnLineDashPattern  = 9
	mres@gsnRightString              = ""
	mres@gsnLeftString               = ""
	mres@tiMainString                = ""

	GPI_mres			= mres
        GPI_mres@cnLevels            	= 90           ; only draw the 564 contour

;---------------------------------------------------------------------------------
  opt1                  = True
  opt1@gsnShadeFillType = "pattern"
  opt1@gsnShadeHigh     = 17
;---------------------------------------------------------------------------------
 
 
  plot = gsn_csm_contour_map(wks,gpidiff,cnres) 




 

 ;GPI_T = gsn_contour(wks,ttest_Abs,GPI_mres)
 ;overlay(plot,GPI_T)

;------------------------------
; plot statistical significance
;------------------------------

  sgres                      = True		; significance
  sgres@gsnDraw              = False		; draw plot
  sgres@gsnFrame             = False		; advance frome
  sgres@cnInfoLabelOn        = False		; turn off info label
  sgres@cnLinesOn            = False		; draw contour lines
  sgres@cnLineLabelsOn       = False		; draw contour labels
  sgres@cnFillScaleF         = 0.7		; add extra density
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

  sig_plot = gsn_csm_contour(wks,ttest_Abs,sgres)
 

  opt                  = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeHigh     = 17
  
  sig_plot1 = gsn_contour_shade(sig_plot,-999.,90.0,opt)
 
  overlay(plot,sig_plot1)
 




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
mkres@gsMarkerSizeF =  9.0   ;Increase marker size.

mkres2 = True
mkres2@gsMarkerIndex = 16 
mkres2@gsMarkerColor = "blue"   ;Change marker color.
mkres2@gsMarkerSizeF =  9.0      ;Increase marker size.

GenesisLocation_E1 = gsn_add_polymarker(wks,plot,lonE1,latE1,mkres)
GenesisLocation_E2 = gsn_add_polymarker(wks,plot,lonE2,latE2,mkres2)
;---------------------------------------------------------
;__________________________________________________________________
  
  ;---Equator line
  lnresq                   = True
  lnresq@gsLineColor       = "black" 
  lnresq@gsLineThicknessF  = 1.0    
  lnresq@gsLineDashPattern = 1
  lonboxq = (/ 0, 180, 180, 0, 0/)
  latboxq = (/0,0,0,0,0/)

  EQLINE0    = gsn_add_polyline(wks,plot,lonboxq,latboxq,lnresq)


;---------------------------------------------------------
gres                  = True
gres@gsLineColor       = "Blue"   ; Polyline resources.
gres@gsLineThicknessF  = 1.0      ; thrice thickness
gres@gsFillColor       = "Red"    ; Polygon resources.
gres@gsFillIndex       = -1
;gres@gsFillOpacityF    = 2
gres@gsEdgesOn         = True     ; Line will be drawn around polygon.
gres@gsEdgeColor       = "black"
gres@gsEdgeThicknessF  = 1
gres@gsMarkerIndex     = 16       ; dots   # Polymarker resources.
gres@gsMarkerColor     = "HotPink"
gres@gsMarkerSizeF     = 0.014    ; twice normal size

xsquare = (/83.0,95.0,95.0,83.0,83.0/)    ; X and Y coordinates
ysquare = (/5,5, 11, 11, 5/)    ; for polygon.

Box  = gsn_add_polygon(wks,plot,xsquare,ysquare,gres)
;---------------------------------------------------------

xsquare2 = (/75,100,100,75,75/)    ; X and Y coordinates
ysquare2 = (/0,0, 15, 15, 0/)    ; for polygon.

Box2  = gsn_add_polygon(wks,plot,xsquare2,ysquare2,gres)

;************************************************
; Attach text to plot using plot coordinates.
;************************************************
  txres               = True                     ; text mods desired
  txres@txFontHeightF = 0.014                     ; font smaller. default big
  txres@txFontColor   = "red"



draw(plot)
; page coordinates are normalized. go from 0->1 not 0->8" etc.
  gsn_text_ndc(wks,"Epoch-1 : 46LLCs",0.42,0.4,txres) 
  txres@txFontColor   = "blue"
  gsn_text_ndc(wks,"Epoch-2 : 26LLCs",0.6,0.4,txres) 
frame(wks) 

end
quit









 

