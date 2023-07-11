
;*************************************************
; wind_3.ncl
;*************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"    
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
;*************************************************
begin
  latS   = -30.				; spatial domain
  latN   =  30. 
  lonL   =  40.
  lonR   = 210.	

;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "png"
   wks_type@wkWidth  = 7.28346
   wks_type@wkHeight = 1
   pname ="Figure_S10"
   wks = gsn_open_wks("eps",pname)

  fu = addfile("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/OCKHI/Jan2021/Fig_6/New/925/U_CHANGE.nc","r")
  fv = addfile("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/OCKHI/Jan2021/Fig_6/New/925/V_CHANGE.nc","r")
 
print(fu)
  u = fu->DIFFAVE(:,:)
  v = fv->DIFFAVE(:,:)
  
  
    u&LATITUDE@units="degrees_north"
    u&LONGITUDE@units="degrees_east"
    v&LATITUDE@units="degrees_north"
    v&LONGITUDE@units="degrees_east"
; ==============================================================
; If dataset longitudes are -180=>180, flip to (nominally) span 0=>360   
; Only if both'lonL' and 'lonR' are positive.
; ==============================================================
  if (lonL.ge.0 .and. lonR.gt.0 .and. u&LONGITUDE(0).lt.0) then
      u    = lonFlip(u)				; flip longitudes 
     ;printVarSummary(u) 
      end if
; ==============================================================
;*************************************************
; calculate divergence on a gaussian grid
;*************************************************
  div = uv2dvG_Wrap(u,v)                ; u,v ==> divergence

;*************************************************
; calculate velocity potential
;*************************************************
  chi = ilapsG_Wrap ( div , 0)
  chi = (/chi/1e6/)                     ; arbitrary scale
  chi@long_name = "velocity potential"
  chi@units     = "m/s" 
;*************************************************
; calculate divergent wind component 
; --
; note: the calculation uses a procedure, so memory 
; must be preallocated.
;*************************************************    
  ud    = new ( dimsizes(u), typeof(u), u@_FillValue )
  vd    = new ( dimsizes(v), typeof(v), v@_FillValue )

  dv2uvg(div,ud,vd) ; div  ==> divergent  wind components

  copy_VarCoords(u, ud ) 
  copy_VarCoords(u, vd ) 
  ud@long_name  = "Zonal Divergent Wind"
  ud@units      = u@units
  vd@long_name  = "Meridional Divergent Wind"
  vd@units      = v@units
  
 ; ==============================================================
  
  
  
  fu_200 = addfile("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/OCKHI/Jan2021/Fig_6/New/200/U_CHANGE.nc","r")
  fv_200 = addfile("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/OCKHI/Jan2021/Fig_6/New/200/V_CHANGE.nc","r")
 

  u_200 = fu_200->DIFFAVE(:,:)
  v_200 = fv_200->DIFFAVE(:,:)
  


    u_200&LATITUDE@units="degrees_north"
    u_200&LONGITUDE@units="degrees_east"
    v_200&LATITUDE@units="degrees_north"
    v_200&LONGITUDE@units="degrees_east"
; ==============================================================
; If dataset longitudes are -180=>180, flip to (nominally) span 0=>360   
; Only if both'lonL' and 'lonR' are positive.
; ==============================================================
  if (lonL.ge.0 .and. lonR.gt.0 .and. u_200&LONGITUDE(0).lt.0) then
      u_200    = lonFlip(u_200)				; flip longitudes 
     ;printVarSummary(u_200) 
      end if
; ==============================================================
;*************************************************
; calculate divergence on a gaussian grid
;*************************************************
  div_200 = uv2dvG_Wrap(u_200,v_200)                ; u,v ==> divergence

;*************************************************
; calculate velocity potential
;*************************************************
  chi_200 = ilapsG_Wrap ( div_200 , 0)
  chi_200 = (/chi_200/1e6/)                     ; arbitrary scale
  chi_200@long_name = "velocity potential"
  chi_200@units     = "m/s" 
;*************************************************
; calculate divergent wind component 
; --
; note: the calculation uses a procedure, so memory 
; must be preallocated.
;*************************************************    
  ud_200    = new ( dimsizes(u_200), typeof(u_200), u_200@_FillValue )
  vd_200    = new ( dimsizes(v_200), typeof(v_200), v_200@_FillValue )

  dv2uvg(div_200,ud_200,vd_200) ; div  ==> divergent  wind components

  copy_VarCoords(u_200, ud_200 ) 
  copy_VarCoords(u_200, vd_200 ) 
  ud_200@long_name  = "Zonal Divergent Wind"
  ud_200@units      = u_200@units
  vd_200@long_name  = "Meridional Divergent Wind"
  vd_200@units      = v_200@units
  
  
  
  
  
  
  
;*************************************************
; plot results
;*************************************************    
  ;cmap = read_colormap_file("BlAqGrYeOrReVi200")
  cmap = read_colormap_file("BlueDarkRed18")  ;for color blindness
  nc   = dimsizes(cmap(:,0))

  res                      = True

  res@gsnDraw                     = False
  res@gsnFrame                    = False  
  res@mpLimitMode = "LatLon"		    ; select Region
  res@mpMinLatF   = -40			    ; Tropics
  res@mpMaxLatF   = 40
  res@mpMinLonF   = 0			    ; Indian Ocean
  res@mpMaxLonF   = 300                   ; Central Pacific

  
  
  res@cnFillOn             = True               ; color on
  res@cnLinesOn            = False              ; turn off contour lines
  res@cnLineLabelsOn              = False
  
  res@gsnScalarContour     = True               ; vectors over contours
  res@cnFillPalette        = cmap(:nc-4,:)
  res@cnLevelSelectionMode        = "ManualLevels"
  res@cnMinLevelValF              = -1.5
  res@cnMaxLevelValF              = 1.5
  res@cnLevelSpacingF             = 0.15
  
  res@vcRefMagnitudeF      = 1.                 ; make vectors larger
  res@vcRefLengthF         = 0.050              ; ref vector length
  res@vcGlyphStyle         = "CurlyVector"      ; turn on curly vectors
  res@vcMinDistanceF       = 0.012              ; thin the vectors
  res@vcRefAnnoOrthogonalPosF  = -0.34         ; Move ref anno into plot

  res@mpLandFillColor       = "grey"            ; change continent color 

  ;res@tiMainString         = "Epoch-2 minus Epoch-1" ;Velocity Potential via Spherical Harmonics"
  res@gsnCenterString      = "" ;"Chi scaled by 1e6"
  res@gsnLeftString        = "(a)" ;"Divergent Wind (925 hPa)"
  res@gsnRightString        = "925hPa"  
  
  res@lbLabelBarOn     		= False          ; turn off individual cb's 
  ;res@lbBoxLinesOn     		= False
  res@lbBoxEndCapStyle                         = "TriangleBothEnds"  
  ;res@lbBoxEndCapStyle                         = "TriangleHighEnd" 
  res@pmLabelBarWidthF                 	= 0.05                                          
  res@pmLabelBarHeightF                	= 0.2 
  res@lbOrientation            = "Vertical"     ; vertical label bar
  ;res@pmLabelBarOrthogonalPosF = -0.01          ; move label bar closer
  res@lbLabelFont                     	= "times-roman"                             
  res@lbLabelFontHeightF              	= 0.010  
  ;res@lbLabelPosition                 	= "Center"                                     
  ;res@lbLabelAlignment                	= "BoxCenters"
  res@lbLabelStride            = 2
  ;res@lbTitleOn          = True
  ;res@lbTitleString      = "kJ cm~S~-2" ;"kJ/cm^2"
  ;res@lbTitleFontHeightF = 0.012
  ;res@lbTitleFont        = "times-roman" 
  
  
  ;res@tiXAxisString        = "longitude"    ; x-axis title
  ;res@tiYAxisString        = "latitude"     ; y-axis title
  res@tiYAxisFont 	       = 25
  res@tiXAxisFont          = 25
  res@tiXAxisFontHeightF   = 0.012
  res@tiYAxisFontHeightF   = 0.012


  res@tmXBLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  res@tmYLLabelFontHeightF    = 0.012    ; Make these labels smaller. 
  ;res@tmXTOn               = True       ; Turn off top tickmarks
  ;res@tmXTLabelsOn         = True              ; no bottom labels
   res@tmXTOn               = False       ; Turn off top tickmarks
   res@tmYROn               = False      ; Turn off left tickmarks
  res@tmYROn               = True      ; Turn off bottom tickmarks
  ;res@tmYRLabelsOn         = True              ; no bottom labels
  ;res@tmXTBorderOn         = True      ; Turn off top border
  ;res@tmXBBorderOn         = True      ; Turn off top border
  ;res@tmYLBorderOn         = True      ; Turn off left border
  ;res@tmYRBorderOn         = True      ; Turn off right border
  
  
        res@tmXBMajorLengthF = 0.01        ; set x axis major tick mark length to 0.01
        res@tmYLMajorLengthF = 0.01        ; set y axis major tick mark length to 0.01
        res@tmXBLabelFontHeightF = 0.012   ; set x axis tick label font size to 0.01
        res@tmYLLabelFontHeightF = 0.012   ; set y axis tick label font size to 0.01
        ;res@tmXBMinorOn = False          ; remove x axis tick lines
        ;res@tmYLMinorOn = False          ; remove y axis tick lines  
        res@tmXTOn = False                ; turn off x-axis tick marks
        res@tmYROn = False                ; turn off y-axis tick marks
        res@tmXBLabelFontHeightF = 0.012  ; set x-axis label font size
        res@tmYLLabelFontHeightF = 0.012  ; set y-axis label font size
        res@tmXBMajorOutwardLengthF = 0.0 ; set x-axis tick marks inward
        res@tmYLMajorOutwardLengthF = 0.0 ; set y-axis tick marks inward
        res@tmXBMinorOutwardLengthF = 0.0 ; set x-axis tick marks inward
        res@tmYLMinorOutwardLengthF = 0.0 ; set y-axis tick marks inward

  ;plot=gsn_csm_vector_scalar_map_ce(wks,ud(:,:),vd(:,:),chi(:,:),res)
  
;----------------------------------------------------------------------------------

  plot = new(2,graphic)

  res@gsnLeftString   = "a"
  res@gsnRightString  = "925hPa"
  plot(0)              = gsn_csm_vector_scalar_map_ce(wks,ud(:,:),vd(:,:),chi(:,:),res)
 
  res@gsnLeftString   = "b"
  res@gsnRightString  = "200hPa"
  plot(1)              = gsn_csm_vector_scalar_map_ce(wks,ud_200(:,:),vd_200(:,:),chi_200(:,:),res)

 
  
  
  ;---Equator line
  lnresq                  = True
  lnresq@gsLineColor      = "white" 
  lnresq@gsLineThicknessF = 1.0    
  lnresq@gsLineDashPattern = 1
  lonboxq = (/ 30, 210, 210, 30, 30/)
  latboxq = (/0,0,0,0,0/)

  EQLINE0    = gsn_add_polyline(wks,plot(0),lonboxq,latboxq,lnresq)
  EQLINE1    = gsn_add_polyline(wks,plot(1),lonboxq,latboxq,lnresq)
;----------------------------------------------------------------------------------
;draw(plot)
;frame(wks)  

;************************************************
; create panel
;************************************************

  resP                           = True                ; modify the panel plot
  resP@gsnPanelMainString        = ""
  resP@gsnPanelLabelBar          = True                ; add common colorbar
  resP@lbLabelFont               = "helvetica"
  resP@lbTitleOn                 = True
  resP@lbLabelStride             = 2
  resP@lbTitleString             = "" ;"~S~o~N~C" ;"kJ/cm^2"
  resP@lbTitleFont               = "helvetica"
  resP@lbLabelFontHeightF   = 0.012     ; make labels smaller
  resP@lbTitleFontHeightF   = 0.012
  resP@lbTitlePosition           = "top"
  resP@pmLabelBarOrthogonalPosF  = 0.005
  resP@pmLabelBarWidthF            = 0.05                                         
  resP@pmLabelBarHeightF           = 0.4
  resP@lbOrientation              = "Vertical"     ; vertical label bars
  resP@gsnPanelFigureStringsFont = "helvetica"
  ;resP@gsnPanelBottom            = 0.2                 ; add space at bottom
; resP@gsnMaximize               = True                 ; use full page
  resP@amJust   	          = "TopRight"
  ;resP@pmLabelBarOrthogonalPosF = -.01
  ;resP@pmLabelBarParallelPosF = .04

  resP@gsnPanelYWhiteSpacePercent = 3.9
  resP@gsnPanelXWhiteSpacePercent = 2.5
  resP@gsnPanelMainString = ""
  ;resP@txString   = ""
  resP@gsnMaximize      = True
  resP@gsnPaperOrientation = "portrait"


  ;resP@gsnPanelFigureStrings= (/"11-May-2017","12-May-2017","13-May-2017","14-May-2017","15-May-2017","16-May-2017","17-May-2017","18-May-2017","19-May-2017","20-May-2017"/) ; add strings to panel
  ;resP@gsnPanelFigureStringsFontHeightF = 0.012
  ;resP@gsnPanelFigureStringsBackgroundFillColor = "aliceblue"
  ;resP@gsnPanelFigureStringsFont        = "times-roman"

  gsn_panel(wks,plot,(/2,1/),resP)               ; now draw as one plot
  ;draw(plot)
  ;frame(wks)



end
