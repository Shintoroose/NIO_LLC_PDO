;----------------------------------------------------------------------
; dataonmap_1.ncl
;
; Concepts illustrated:
;   - Plotting contours on a rectilinear grid
;----------------------------------------------------------------------
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;----------------------------------------------------------------------

begin
  filename1 = "/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/TC__Days_density/IMD_eAtlas/map_data_E1.nc"
  filename2 = "/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/TC__Days_density/IMD_eAtlas/map_data_E2.nc"
  
  filename3 = "/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/TC__Days_density/Correct_lat_lonband/Epoch_1_LLC.nc"
  filename4 = "/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/TC__Days_density/Correct_lat_lonband/Epoch_2_LLC.nc"
  
  
  
  a = addfile(filename1,"r")    ; open file
  b = addfile(filename2,"r")    ; open file
  c = addfile(filename3,"r")    ; open file
  d = addfile(filename4,"r")    ; open file
  
  d1 = a->density(0,:,:)          
  d2 = b->density(0,:,:)           

  d3 = c->density(0,:,:)          
  d4 = d->density(0,:,:) 

  printVarSummary(d1)           ; print info about variable

;---Plot data
;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "png"
   wks_type@wkWidth  = 7.28346
   wks_type@wkHeight = 6
   pname ="Figure_S2"
   wks = gsn_open_wks("eps",pname)
  

  res              = True
  res@gsnDraw                  = False
  res@gsnFrame                 = False
  res@cnLineLabelsOn              = False 
  res@gsnMaximize  = False     ; maximize plot in frame
  res@cnFillOn     = True     ; turn on contour fill
  res@cnLinesOn    = False    ; turn off contour fill
  res@tiMainString = ""
  res@cnFillMode            = "CellFill"
  ;res@cnFillPalette         = "perc2_9lev"
  res@cnFillPalette         = "WhiteBlue"


  res@mpLimitMode                 = "LatLon"		    
  res@mpMinLatF                   = -2.75		  
  res@mpMaxLatF                   = 24.75
  res@mpMinLonF                   = 50			   
  res@mpMaxLonF                   = 100 
  res@mpFillOn                    = False        ; turn off gray fill
  
  res@lbLabelBarOn              = False          ; turn off individual cb's 

  res@cnLevelSelectionMode        = "ManualLevels"
  res@cnMinLevelValF              = 0.2
  res@cnMaxLevelValF              = 2.0
  res@cnLevelSpacingF             = 0.05
  res@gsnRightString              = ""
  res@gsnLeftString               = ""

  res@gsnAddCyclic = False


        res@tmXBMajorLengthF = 0.01        ; set x axis major tick mark length to 0.01
        res@tmYLMajorLengthF = 0.01        ; set y axis major tick mark length to 0.01
        res@tmXBLabelFontHeightF = 0.02   ; set x axis tick label font size to 0.01
        res@tmYLLabelFontHeightF = 0.02   ; set y axis tick label font size to 0.01
        ;res@tmXBMinorOn = False          ; remove x axis tick lines
        ;res@tmYLMinorOn = False          ; remove y axis tick lines  
        res@tmXTOn = False                ; turn off x-axis tick marks
        res@tmYROn = False                ; turn off y-axis tick marks
        res@tmXBLabelFontHeightF = 0.02  ; set x-axis label font size
        res@tmYLLabelFontHeightF = 0.02  ; set y-axis label font size
        res@tmXBMajorOutwardLengthF = 0.0 ; set x-axis tick marks inward
        res@tmYLMajorOutwardLengthF = 0.0 ; set y-axis tick marks inward
        res@tmXBMinorOutwardLengthF = 0.0 ; set x-axis tick marks inward
        res@tmYLMinorOutwardLengthF = 0.0 ; set y-axis tick marks inward

plot = new(4,graphic)                       ; create a plot array


 

res@gsnLeftString   = "a          Epoch-1 (Cyclone eAtlas - IMD)"
plot(0) = gsn_csm_contour_map(wks,d1(:,:),res) 

res@gsnLeftString   = "b          Epoch-2 (Cyclone eAtlas - IMD)"
plot(1) = gsn_csm_contour_map(wks,d2(:,:),res)

res@gsnLeftString   = "c                   Epoch-2 (IBTrACS)"
plot(2) = gsn_csm_contour_map(wks,d3(:,:),res) 

res@gsnLeftString   = "d                   Epoch-2 (IBTrACS)"
plot(3) = gsn_csm_contour_map(wks,d4(:,:),res)
 
 
   ;----------------------------------------------------------------------
; Draw the lat/lon grid with markers, using black for grid locations
; where the data is not missing, and yellow for grid locations
; where the data is missing.
;---------------------------------------------------------------------
  pres                          = True
  pres@gsnCoordsNonMissingColor = "gray50"
  pres@gsnCoordsMissingColor    = "yellow"
  pres@gsMarkerSizeF            = 5.0
  ;gsn_coordinates(wks,plot(0),d1(:,:),pres)

;----------------------------------------------------------------------
; Draw lines at the lon/lon coordinate array values.
; Marker resources set above will be ignored.
;----------------------------------------------------------------------
  pres@gsnCoordsAsLines  = True
  pres@gsLineThicknessF  = 2
  ;gsn_coordinates(wks,plot(0),d1(:,:),pres)

 
;---Overlay a red box
  lnres                  = True
  lnres@gsLineColor      = "red" ; red box 
  lnres@gsLineThicknessF = 2.0   ; make box thicker
  lnres@gsLineDashPattern = 2

  lonboxb = (/ 83, 95, 95, 83, 83/)
  latboxb = (/5,5,11,11,5/)
  box1 = gsn_add_polyline(wks,plot(0),lonboxb,latboxb,lnres)
  box2 = gsn_add_polyline(wks,plot(1),lonboxb,latboxb,lnres)
  box3 = gsn_add_polyline(wks,plot(2),lonboxb,latboxb,lnres)
  box4 = gsn_add_polyline(wks,plot(3),lonboxb,latboxb,lnres)
;************************************************
; create panel
;************************************************

  resP                  	        = True                   	                ; modify the panel plot
  resP@gsnFrame                         = True                   	                ; don't advance panel plot
  resP@gsnPanelLabelBar 	        = True                   	                ; add common colorbar
  resP@lbLabelFont                      = "complex_script"                              ; label font
  ;resP@lbOrientation             = "Vertical"
  resP@lbBoxEndCapStyle         = "TriangleBothEnds" 
  ;resP@lbBoxEndCapStyle          = "TriangleHighEnd" 
  resP@lbLabelFont               = "helvetica"                              ; label font
  resP@lbLabelFontHeightF        = 0.015  
  resP@lbLabelStride             = 1             
  resP@pmLabelBarWidthF          = 0.5                                          
  resP@pmLabelBarHeightF         = 0.05
 
  ;resP@lbLabelPosition                  = "Center"                                      ; label position
  ;resP@lbLabelAlignment                 = "BoxCenters"                                  ; label orientation
  resP@txString         	        = " "  ; add common title
  resP@txFont                           = 2 
  resP@txFontHeightF                    = 0.02  
  resP@txFontColor                      = "Brown"
  ;resP@gsnPanelBottom   	        = 0.05                   	                ; add space at bottom
  ;resP@gsnPanelFigureStrings	        = (/" Epoch-1 (Cyclone eAtlas - IMD)","   Epoch-2 (Cyclone eAtlas - IMD)"/) 
  resP@gsnPanelFigureStringsFontHeightF = 0.015
  resP@gsnPanelFigureStringsBackgroundFillColor = "aliceblue"
  resP@gsnPanelFigureStringsFont        = "helvetica"
  resP@amJust   		        = "TopLeft"
  ;resP@gsnPanelRowSpec                  = True                                          ; tell panel what order to plt
  ;resP@lbTitleString         = "m s~S~-1~N~" ;""
  ;resP@lbTitleFontHeightF    = 0.015
  ;resP@lbTitleOffsetF        = -0.03
  resP@gsnPanelYWhiteSpacePercent =2.9
  gsn_panel(wks,plot,(/2,2/),resP)         	                                ; now draw as one plot


 
;  txres               = True
;  txres@txFontHeightF = 0.015
;  gsn_text_ndc(wks,"Figure 1: A nifty panel plot",0.5,0.02,txres)
;  frame(wks)


end