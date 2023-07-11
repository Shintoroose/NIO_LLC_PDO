load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin



; Main code
;----------------------------------------------------------------------
 
;---Recreating jpeg images only works for X11 and PNG.
   wks_type = "png"
   wks_type@wkWidth  = 7.28346
   wks_type@wkHeight = 4 
   pname ="Figure_5"
   wks = gsn_open_wks("eps",pname)



;---Southern part of Africa
  minlat = -10
  maxlat =  20
  minlon = 65
  maxlon = 100

  res = True
  res@mpOutlineOn           = True
  res@gsnDraw                  = False
  res@gsnFrame                 = False
  res@mpMaxLatF             = maxlat
  res@mpMinLatF             = minlat
  res@mpMinLonF             = minlon
  res@mpMaxLonF             = maxlon
  res@pmTickMarkDisplayMode = "Always"   ; nicer tickmarks

  res@mpOutlineBoundarySets = "Geophysical"
  res@mpDataSetName         = "Earth..4" 
  res@mpDataBaseVersion     = "MediumRes"


 
  res@tmXBLabelFontHeightF    = 0.02    ; Make these labels smaller.
  res@tmYLLabelFontHeightF    = 0.02   ; Make these labels smaller.
  res@tiYAxisFont 	        = "helvetica"
  res@tiXAxisFont             = "helvetica"

  res@tmXBMajorLengthF        = 0.01
  res@tmYLMajorLengthF        = 0.01

  res@tmYLLabelFont           = "helvetica"
  res@tmXBLabelFont           = "helvetica"
  res@tmXBLabelsOn            = True

  res@tmYRLabelsOn = True
  res@tmXBMajorLengthF=0.015
  res@tmYRMajorLengthF=0.015
  ;res@tmXBOn               = True     ; Turn off top tickmarks
  ;res@tmXTOn               = False     ; Turn off top tickmarks
  ;res@tmYLOn               = False     ; Turn off left tickmarks
  ;res@tmYROn               = True      ; Turn off bottom tickmarks
  

  res@gsnStringFont = "helvetica"

resa=res
resa@gsnLeftString        = "a         El Nin~H-13V2F35~D~FV-2H3~o (Epoch-1)" 
resb=res
resb@gsnLeftString        = "b         La Nin~H-13V2F35~D~FV-2H3~a (Epoch-1)" 
resc=res 
resc@gsnLeftString        = "c         Neutral (Epoch-1)"
resd=res
resd@gsnLeftString        = "d         El Nin~H-13V2F35~D~FV-2H3~o (Epoch-2)"
rese=res
rese@gsnLeftString        = "e         La Nin~H-13V2F35~D~FV-2H3~a (Epoch-2)" 
resf=res
resf@gsnLeftString        = "f         Neutral (Epoch-2)" 
 
;_______________________________________________________________________________

  plot = new(6,graphic)                       ; create a plot array

plot(0) = gsn_csm_map(wks,resa)  
plot(1) = gsn_csm_map(wks,resb)        
plot(2) = gsn_csm_map(wks,resc)  
plot(3) = gsn_csm_map(wks,resd)
plot(4) = gsn_csm_map(wks,rese)  
plot(5) = gsn_csm_map(wks,resf)
;_______________________________________________________________________________________________________
  fu_ELNINO = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/ELNINO/Anom_ELNINO_U850_1951_1980.nc","r")
  fv_ELNINO = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/ELNINO/Anom_ELNINO_V850_1951_1980.nc","r")

  print(fu_ELNINO)
  u_ELNINO=fu_ELNINO->change(:,:)
  v_ELNINO=fv_ELNINO->change(:,:)
 
  
  wspd_ELNINO  = wind_speed(u_ELNINO,v_ELNINO)

  u_alpha_ELNINO=fu_ELNINO->alpha(:,:)
  v_alpha_ELNINO=fv_ELNINO->alpha(:,:)
  
 
   copy_VarMeta(u_ELNINO,u_alpha_ELNINO)
   copy_VarMeta(v_ELNINO,v_alpha_ELNINO)

;-------------------------------------
  u_sigu_ELNINO=where(u_alpha_ELNINO.ge.95,u_ELNINO,u_ELNINO@_FillValue)
  v_sigu_ELNINO=where(u_alpha_ELNINO.ge.95,v_ELNINO,v_ELNINO@_FillValue) 


  u_sigv_ELNINO=where(v_alpha_ELNINO.ge.95,u_ELNINO,u_ELNINO@_FillValue)
  v_sigv_ELNINO=where(v_alpha_ELNINO.ge.95,v_ELNINO,v_ELNINO@_FillValue) 


   copy_VarMeta(u_ELNINO,u_sigu_ELNINO)
   copy_VarMeta(v_ELNINO,v_sigu_ELNINO)
   
   copy_VarMeta(u_ELNINO,u_sigv_ELNINO)
   copy_VarMeta(v_ELNINO,v_sigv_ELNINO)
;_______________________________________________________________________________________________________
  fu_LANINA = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/LANINA/Anom_LANINA_U850_1951_1980.nc","r")
  fv_LANINA = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/LANINA/Anom_LANINA_V850_1951_1980.nc","r")

  print(fu_LANINA)
  u_LANINA=fu_LANINA->change(:,:)
  v_LANINA=fv_LANINA->change(:,:)
 
  
  wspd_LANINA  = wind_speed(u_LANINA,v_LANINA)

  u_alpha_LANINA=fu_LANINA->alpha(:,:)
  v_alpha_LANINA=fv_LANINA->alpha(:,:)
  
 
   copy_VarMeta(u_LANINA,u_alpha_LANINA)
   copy_VarMeta(v_LANINA,v_alpha_LANINA)

;-------------------------------------
  u_sigu_LANINA=where(u_alpha_LANINA.ge.95,u_LANINA,u_LANINA@_FillValue)
  v_sigu_LANINA=where(u_alpha_LANINA.ge.95,v_LANINA,v_LANINA@_FillValue) 


  u_sigv_LANINA=where(v_alpha_LANINA.ge.95,u_LANINA,u_LANINA@_FillValue)
  v_sigv_LANINA=where(v_alpha_LANINA.ge.95,v_LANINA,v_LANINA@_FillValue) 


   copy_VarMeta(u_LANINA,u_sigu_LANINA)
   copy_VarMeta(v_LANINA,v_sigu_LANINA)
   
   copy_VarMeta(u_LANINA,u_sigv_LANINA)
   copy_VarMeta(v_LANINA,v_sigv_LANINA)

;_______________________________________________________________________________________________________
  fu_NEUTRAL = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/Non_ENSO/Anom_NONENSO_U850_1951_1980.nc","r")
  fv_NEUTRAL = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch1_Anomaly/Non_ENSO/Anom_NONENSO_V850_1951_1980.nc","r")

  print(fu_NEUTRAL)
  u_NEUTRAL=fu_NEUTRAL->change(:,:)
  v_NEUTRAL=fv_NEUTRAL->change(:,:)
 
  
  wspd_NEUTRAL  = wind_speed(u_NEUTRAL,v_NEUTRAL)

  u_alpha_NEUTRAL=fu_NEUTRAL->alpha(:,:)
  v_alpha_NEUTRAL=fv_NEUTRAL->alpha(:,:)
  
 
   copy_VarMeta(u_NEUTRAL,u_alpha_NEUTRAL)
   copy_VarMeta(v_NEUTRAL,v_alpha_NEUTRAL)

;-------------------------------------
  u_sigu_NEUTRAL=where(u_alpha_NEUTRAL.ge.95,u_NEUTRAL,u_NEUTRAL@_FillValue)
  v_sigu_NEUTRAL=where(u_alpha_NEUTRAL.ge.95,v_NEUTRAL,v_NEUTRAL@_FillValue) 


  u_sigv_NEUTRAL=where(v_alpha_NEUTRAL.ge.95,u_NEUTRAL,u_NEUTRAL@_FillValue)
  v_sigv_NEUTRAL=where(v_alpha_NEUTRAL.ge.95,v_NEUTRAL,v_NEUTRAL@_FillValue) 


   copy_VarMeta(u_NEUTRAL,u_sigu_NEUTRAL)
   copy_VarMeta(v_NEUTRAL,v_sigu_NEUTRAL)
   
   copy_VarMeta(u_NEUTRAL,u_sigv_NEUTRAL)
   copy_VarMeta(v_NEUTRAL,v_sigv_NEUTRAL)
;_______________________________________________________________________________________________________
 
;_________________________________________________________________________________
  fu_ELNINO_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/ELNINO/Anom_ELNINO_U850_1981_2010.nc","r")
  fv_ELNINO_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/ELNINO/Anom_ELNINO_V850_1981_2010.nc","r")

  print(fu_ELNINO_EPOCH2)
  u_ELNINO_EPOCH2=fu_ELNINO_EPOCH2->change(:,:)
  v_ELNINO_EPOCH2=fv_ELNINO_EPOCH2->change(:,:)
 
  
  wspd_ELNINO_EPOCH2  = wind_speed(u_ELNINO_EPOCH2,v_ELNINO_EPOCH2)

  u_alpha_ELNINO_EPOCH2=fu_ELNINO_EPOCH2->alpha(:,:)
  v_alpha_ELNINO_EPOCH2=fv_ELNINO_EPOCH2->alpha(:,:)
  
 
   copy_VarMeta(u_ELNINO_EPOCH2,u_alpha_ELNINO_EPOCH2)
   copy_VarMeta(v_ELNINO_EPOCH2,v_alpha_ELNINO_EPOCH2)

;-------------------------------------
  u_sigu_ELNINO_EPOCH2=where(u_alpha_ELNINO_EPOCH2.ge.95,u_ELNINO_EPOCH2,u_ELNINO_EPOCH2@_FillValue)
  v_sigu_ELNINO_EPOCH2=where(u_alpha_ELNINO_EPOCH2.ge.95,v_ELNINO_EPOCH2,v_ELNINO_EPOCH2@_FillValue) 


  u_sigv_ELNINO_EPOCH2=where(v_alpha_ELNINO_EPOCH2.ge.95,u_ELNINO_EPOCH2,u_ELNINO_EPOCH2@_FillValue)
  v_sigv_ELNINO_EPOCH2=where(v_alpha_ELNINO_EPOCH2.ge.95,v_ELNINO_EPOCH2,v_ELNINO_EPOCH2@_FillValue) 


   copy_VarMeta(u_ELNINO_EPOCH2,u_sigu_ELNINO_EPOCH2)
   copy_VarMeta(v_ELNINO_EPOCH2,v_sigu_ELNINO_EPOCH2)
   
   copy_VarMeta(u_ELNINO_EPOCH2,u_sigv_ELNINO_EPOCH2)
   copy_VarMeta(v_ELNINO_EPOCH2,v_sigv_ELNINO_EPOCH2)
;_______________________________________________________________________________________________________
  fu_LANINA_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/LANINA/Anom_LANINA_U850_1981_2010.nc","r")
  fv_LANINA_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/LANINA/Anom_LANINA_V850_1981_2010.nc","r")

  print(fu_LANINA_EPOCH2)
  u_LANINA_EPOCH2=fu_LANINA_EPOCH2->change(:,:)
  v_LANINA_EPOCH2=fv_LANINA_EPOCH2->change(:,:)
 
  
  wspd_LANINA_EPOCH2  = wind_speed(u_LANINA_EPOCH2,v_LANINA_EPOCH2)

  u_alpha_LANINA_EPOCH2=fu_LANINA_EPOCH2->alpha(:,:)
  v_alpha_LANINA_EPOCH2=fv_LANINA_EPOCH2->alpha(:,:)
  
 
   copy_VarMeta(u_LANINA_EPOCH2,u_alpha_LANINA_EPOCH2)
   copy_VarMeta(v_LANINA_EPOCH2,v_alpha_LANINA_EPOCH2)

;-------------------------------------
  u_sigu_LANINA_EPOCH2=where(u_alpha_LANINA_EPOCH2.ge.95,u_LANINA_EPOCH2,u_LANINA_EPOCH2@_FillValue)
  v_sigu_LANINA_EPOCH2=where(u_alpha_LANINA_EPOCH2.ge.95,v_LANINA_EPOCH2,v_LANINA_EPOCH2@_FillValue) 


  u_sigv_LANINA_EPOCH2=where(v_alpha_LANINA_EPOCH2.ge.95,u_LANINA_EPOCH2,u_LANINA_EPOCH2@_FillValue)
  v_sigv_LANINA_EPOCH2=where(v_alpha_LANINA_EPOCH2.ge.95,v_LANINA_EPOCH2,v_LANINA_EPOCH2@_FillValue) 


   copy_VarMeta(u_LANINA_EPOCH2,u_sigu_LANINA_EPOCH2)
   copy_VarMeta(v_LANINA_EPOCH2,v_sigu_LANINA_EPOCH2)
   
   copy_VarMeta(u_LANINA_EPOCH2,u_sigv_LANINA_EPOCH2)
   copy_VarMeta(v_LANINA_EPOCH2,v_sigv_LANINA_EPOCH2)

;_________________________________________________________________________________
  fu_NEUTRAL_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/Non_ENSO/Anom_NONENSO_U850_1981_2010.nc","r")
  fv_NEUTRAL_EPOCH2 = addfile("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/ENSO/Wind_850_Epoch2_Anomaly/Non_ENSO/Anom_NONENSO_V850_1981_2010.nc","r")
 

  print(fu_NEUTRAL_EPOCH2)
  u_NEUTRAL_EPOCH2=fu_NEUTRAL_EPOCH2->change(:,:)
  v_NEUTRAL_EPOCH2=fv_NEUTRAL_EPOCH2->change(:,:)
 
  
  wspd_NEUTRAL_EPOCH2  = wind_speed(u_NEUTRAL_EPOCH2,v_NEUTRAL_EPOCH2)

  u_alpha_NEUTRAL_EPOCH2=fu_NEUTRAL_EPOCH2->alpha(:,:)
  v_alpha_NEUTRAL_EPOCH2=fv_NEUTRAL_EPOCH2->alpha(:,:)
  
 
   copy_VarMeta(u_NEUTRAL_EPOCH2,u_alpha_NEUTRAL_EPOCH2)
   copy_VarMeta(v_NEUTRAL_EPOCH2,v_alpha_NEUTRAL_EPOCH2)

;-------------------------------------
  u_sigu_NEUTRAL_EPOCH2=where(u_alpha_NEUTRAL_EPOCH2.ge.95,u_NEUTRAL_EPOCH2,u_NEUTRAL_EPOCH2@_FillValue)
  v_sigu_NEUTRAL_EPOCH2=where(u_alpha_NEUTRAL_EPOCH2.ge.95,v_NEUTRAL_EPOCH2,v_NEUTRAL_EPOCH2@_FillValue) 


  u_sigv_NEUTRAL_EPOCH2=where(v_alpha_NEUTRAL_EPOCH2.ge.95,u_NEUTRAL_EPOCH2,u_NEUTRAL_EPOCH2@_FillValue)
  v_sigv_NEUTRAL_EPOCH2=where(v_alpha_NEUTRAL_EPOCH2.ge.95,v_NEUTRAL_EPOCH2,v_NEUTRAL_EPOCH2@_FillValue) 


   copy_VarMeta(u_NEUTRAL_EPOCH2,u_sigu_NEUTRAL_EPOCH2)
   copy_VarMeta(v_NEUTRAL_EPOCH2,v_sigu_NEUTRAL_EPOCH2)
   
   copy_VarMeta(u_NEUTRAL_EPOCH2,u_sigv_NEUTRAL_EPOCH2)
   copy_VarMeta(v_NEUTRAL_EPOCH2,v_sigv_NEUTRAL_EPOCH2)
;_______________________________________________________________________________________________________
;_______________________________________________________________________________________________________


  ;cmap = read_colormap_file("wh-bl-gr-ye-re")
  cmap = read_colormap_file("CBR_wet") ; for color blindness
 


;---Set GSN resources
  vcres2                         = True             ; plot mods desired
  vcres2@gsnDraw                 = False
  vcres2@gsnFrame                = False            ; so we can draw time stamp
  vcres2@gsnMaximize             = False


  vcres2@vcVectorDrawOrder       = "PostDraw"        ; draw vectors last

  vcres2@mpLandFillColor         = "gray"              ; set land to be gray


  vcres2@vcRefMagnitudeF         = 2.5             ; define vector ref mag
  vcres2@vcRefLengthF            = 0.04            ; define length of vec ref

  vcres2@vcRefAnnoOrthogonalPosF   = -0.14
  vcres2@vcRefAnnoFontHeightF      = 0.02
  vcres2@vcRefAnnoString2On        = False 
  vcres2@vcGlyphStyle              = "CurlyVector"    ; turn on curly vectors
  vcres2@vcMinDistanceF            = 0.04

;---Set up some vector resources.

  vcres2@vcLevelPalette           = cmap
  vcres2@vcLevelSelectionMode	 = "ManualLevels"
  vcres2@vcMinLevelValF           = 0.0
  vcres2@vcMaxLevelValF           = 2.5
  vcres2@vcLevelSpacingF          = 0.1

  ;vcres2@vcLevelPalette           = "cmocean_matter"   ; assign color map to vectors WhiteBlueGreenYellowRed

  vcres2@lbLabelBarOn                = False           ; turn off individual cb's 
  ;vcres2@lbBoxEndCapStyle            = "TriangleBothEnds"  
  ;vcres2@lbBoxEndCapStyle           = "TriangleHighEnd" 
  ;vcres2@pmLabelBarWidthF            = 0.4                                          
  ;vcres2@pmLabelBarHeightF           = 0.045 
  ;vcres2@lbOrientation              = "Vertical"     ; vertical label bar
  ;vcres2@pmLabelBarOrthogonalPosF    = 0.19          ; move label bar closer
  ;vcres2@lbLabelFont                 = "helvetica"


  vcres2@vcFillArrowsOn           = True
  vcres2@vcLineArrowThicknessF    =  2.5
  ;vcres2@vcRefLengthF             = 0.045
  ;vcres2@vcRefMagnitudeF          = 0.5
  ;vcres2@vcRefAnnoOrthogonalPosF  = -0.12
  ;vcres2@vcRefAnnoParallelPosF    =  0.997
  ;vcres2@vcRefAnnoFontHeightF     = 0.015

  ;vcres2@vcMinFracLengthF         = 0.33
  ;vcres2@vcMinMagnitudeF          = 0.001
  vcres2@vcMonoFillArrowFillColor = False
  vcres2@vcMonoLineArrowColor     = False

  vcres2@gsnLeftString        = ""  
  vcres2@gsnRightString        = ""  
  ;vcres2@gsnLeftStringFontHeightF = 0.013

 


;----------------------------------------------


 uv_rc_plot_sigu_ELNINO  = gsn_csm_vector_scalar(wks,u_sigu_ELNINO,v_sigu_ELNINO,wspd_ELNINO,vcres2)
 uv_rc_plot_sigv_ELNINO  = gsn_csm_vector_scalar(wks,u_sigv_ELNINO,v_sigv_ELNINO,wspd_ELNINO,vcres2)

 uv_rc_plot_sigu_LANINA  = gsn_csm_vector_scalar(wks,u_sigu_LANINA,v_sigu_LANINA,wspd_LANINA,vcres2)
 uv_rc_plot_sigv_LANINA  = gsn_csm_vector_scalar(wks,u_sigv_LANINA,v_sigv_LANINA,wspd_LANINA,vcres2)

 uv_rc_plot_sigu_NEUTRAL  = gsn_csm_vector_scalar(wks,u_sigu_NEUTRAL,v_sigu_NEUTRAL,wspd_NEUTRAL,vcres2)
 uv_rc_plot_sigv_NEUTRAL  = gsn_csm_vector_scalar(wks,u_sigv_NEUTRAL,v_sigv_NEUTRAL,wspd_NEUTRAL,vcres2)

 uv_rc_plot_sigu_ELNINO_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigu_ELNINO_EPOCH2,v_sigu_ELNINO_EPOCH2,wspd_ELNINO_EPOCH2,vcres2)
 uv_rc_plot_sigv_ELNINO_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigv_ELNINO_EPOCH2,v_sigv_ELNINO_EPOCH2,wspd_ELNINO_EPOCH2,vcres2)
 
 uv_rc_plot_sigu_LANINA_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigu_LANINA_EPOCH2,v_sigu_LANINA_EPOCH2,wspd_LANINA_EPOCH2,vcres2)
 uv_rc_plot_sigv_LANINA_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigv_LANINA_EPOCH2,v_sigv_LANINA_EPOCH2,wspd_LANINA_EPOCH2,vcres2)
 
  uv_rc_plot_sigu_NEUTRAL_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigu_NEUTRAL_EPOCH2,v_sigu_NEUTRAL_EPOCH2,wspd_NEUTRAL_EPOCH2,vcres2)
 uv_rc_plot_sigv_NEUTRAL_EPOCH2  = gsn_csm_vector_scalar(wks,u_sigv_NEUTRAL_EPOCH2,v_sigv_NEUTRAL_EPOCH2,wspd_NEUTRAL_EPOCH2,vcres2)

  overlay(plot(0),uv_rc_plot_sigu_ELNINO)
  overlay(plot(0),uv_rc_plot_sigv_ELNINO)

  overlay(plot(1),uv_rc_plot_sigu_LANINA)
  overlay(plot(1),uv_rc_plot_sigv_LANINA)

  overlay(plot(2),uv_rc_plot_sigu_NEUTRAL)
  overlay(plot(2),uv_rc_plot_sigv_NEUTRAL)

  overlay(plot(3),uv_rc_plot_sigu_ELNINO_EPOCH2)
  overlay(plot(3),uv_rc_plot_sigv_ELNINO_EPOCH2)

  overlay(plot(4),uv_rc_plot_sigu_LANINA_EPOCH2)
  overlay(plot(4),uv_rc_plot_sigv_LANINA_EPOCH2)

  overlay(plot(5),uv_rc_plot_sigu_NEUTRAL_EPOCH2)
  overlay(plot(5),uv_rc_plot_sigv_NEUTRAL_EPOCH2)
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
  lnresq@gsLineThicknessF = 2.0    
  lnresq@gsLineDashPattern = 2
  lonboxq = (/ 0, 180, 180, 0, 0/)
  latboxq = (/0,0,0,0,0/)

  EQLINE0    = gsn_add_polyline(wks,plot(0),lonboxq,latboxq,lnresq)
  EQLINE1    = gsn_add_polyline(wks,plot(1),lonboxq,latboxq,lnresq)
  EQLINE3    = gsn_add_polyline(wks,plot(2),lonboxq,latboxq,lnresq)
  EQLINE4    = gsn_add_polyline(wks,plot(3),lonboxq,latboxq,lnresq)
  EQLINE5    = gsn_add_polyline(wks,plot(4),lonboxq,latboxq,lnresq)
  EQLINE6    = gsn_add_polyline(wks,plot(5),lonboxq,latboxq,lnresq)
;************************************************
; create panel
;************************************************

  resP                  	        = True                   	                ; modify the panel plot
  resP@gsnFrame                         = True                   	                ; don't advance panel plot
  resP@gsnPanelLabelBar 	        = True                   	                ; add common colorbar
  resP@lbLabelFont                      = "helvetica"                              ; label font
  resP@lbOrientation             = "Vertical"
  ;resP@lbBoxEndCapStyle         = "TriangleBothEnds" 
  resP@lbBoxEndCapStyle          = "TriangleHighEnd" 
  resP@lbLabelFont               = "helvetica"                              ; label font
  resP@lbLabelFontHeightF        = 0.012  
  resP@lbLabelStride             = 2             
  resP@pmLabelBarWidthF          = 0.06                                          
  resP@pmLabelBarHeightF         = 0.4
 
  ;resP@lbLabelPosition                  = "Center"                                      ; label position
  ;resP@lbLabelAlignment                 = "BoxCenters"                                  ; label orientation
  ;resP@txString         	        = "Composite of Sea Surface Temperature (TMI) (1998-2010)"  ; add common title
  resP@txFont                           = 2 
  resP@txFontHeightF                    = 0.015  
  resP@txFontColor                      = "Brown"
  ;resP@gsnPanelBottom   	        = 0.05                   	                ; add space at bottom
  ;resP@gsnPanelFigureStrings	        = (/"(a) El Nin~H-13V2F35~D~FV-2H3~o (Epoch-1)","(b) La Nin~H-13V2F35~D~FV-2H3~a  (Epoch-1)","(c) El Nin~H-13V2F35~D~FV-2H3~o (Epoch-2)","(d) La Nin~H-13V2F35~D~FV-2H3~a  (Epoch-2)"/) 
  resP@gsnPanelFigureStringsFontHeightF = 0.015
  resP@gsnPanelFigureStringsBackgroundFillColor = "aliceblue"
  resP@gsnPanelFigureStringsFont        = "helvetica"
  resP@amJust   		        = "TopLeft"
  resP@gsnPanelRowSpec                  = True                                          ; tell panel what order to plt
  resP@lbTitleString         = "m s~S~-1~N~" ;""
  resP@lbTitleFontHeightF    = 0.015
  resP@lbTitleOffsetF        = -0.03
  resP@gsnPanelYWhiteSpacePercent =2.9
  gsn_panel(wks,plot,(/3,3/),resP)         	                                ; now draw as one plot


 
;  txres               = True
;  txres@txFontHeightF = 0.015
;  gsn_text_ndc(wks,"Figure 1: A nifty panel plot",0.5,0.02,txres)
;  frame(wks)







  ;draw(plot)
  ;frame(wks)  

  delete(wks)

  cmd = "convert -geometry 2500x2500 -density 300 -trim " + pname + ".eps " + \
                                                          pname + ".png"
  system(cmd)    

end
