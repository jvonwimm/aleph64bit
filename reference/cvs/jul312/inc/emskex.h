      COMMON / EMSKEX / EMINC,PMINC,ZHCAL(2),RHCAL(2),REMAX,ZEMAX
     &                 , EMIMU
C-       EMINC minimum energy for a cluster to do track association
C-       PMINC mimimum track energy to extrapolate
C-       ZHCAL entrance , end hcal forward
C-       RHCAL entrance , end hcal barrel
C-       REMAX  end ECAL radius barrel
C-       ZEMAX  end ECAL forward
C-       EMIMU  Threshold of muon definition
      COMMON / EMSKMX / EMUON , SLOPE , ENDPA(3,2) , TILT , FUDGE(3)
C-       EMUON energy deposited by a muon in HCAL at normal incidence
C-       SLOPE HCAL angular energy dependence parameter
C-       ENDPA ECAL energy dependence parameter
C-       TILT  phi tilt of ECAL barrel module
C-       FUDGE Calibration factor for ECAL stacks
