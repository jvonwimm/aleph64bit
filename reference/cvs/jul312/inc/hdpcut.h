      COMMON /HDPCUT/ DMIXHD,DMIYHD,BMIXHD,BMIYHD,CMIXHD,CMIYHD,
     &                PHI1HD,YSZ1HD,PHI2HD,YSZ2HD,DMI0HD,KISPHD,
     &                NSACU1,NSACU2
#if defined(DOC)
C  DMIXHD = Minimum distance in the same module and layer
C  DMIYHD = Minimum distance in the same module, different layer
C
C  BMIXHD = Min. dist. same layer, different module, central border.
C  BMIYHD = Min. dist. different layer, different modules, centr. bord.
C
C  CMIXHD = Min. dist. same layer, different module, external border
C  CMIYHD = Min. dist. different layer, diff. mod., ext. border
C
C  PHI1HD = PHI MAX to link ECLU to the PATTERNS
C  YSZ1HD = Y/Z MAX to link ECLU to the PATTERNS
C  PHI2HD = PHI MAX to link HCLU to the PATTERNS
C  YSZ2HD = Y/Z MAX to link HCLU to the PATTERNS
C
C  DMI0HD = MAX distance to include satellites
C  KISPHD = SUPERPATTERN KIND (1=E , 2=H)
C
C  NSACU1 = Maximum number of hits for a satellite
C  NSACU2 = Maximum number of hits for Pattern suppression
C
#endif
