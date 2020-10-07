      SUBROUTINE MUBOOK
C
C***********************************************************************
C
C T.Wang -860808
C
C       Subroutine to book histograms of MUON
C       if FHISJO(8) .EQ. .TRUE.
C
C       Called from MUIRUN        in this .HLB
C       Calls       HBOOK1        in HBOOK
C
C***********************************************************************
C
      PARAMETER (MXPHT = 200, MXDHT =  40, MXEMD = 94)
      PARAMETER (MXTRK = 100, MXTDI = 100)
C
C
      CALL HBOOK1(801,'# OF SEGMENTS / ELEMENT$',20,0.5,20.5)
      CALL HBOOK1(802,'# OF STREAMERS / ELEMENT$',20,0.5,20.5)
      CALL HBOOK1(811,'# OF HITS / TRACK$',40,0.5,40.5)
      CALL HBOOK1(812,'# OF DIGITS / TRACK$',40,0.5,40.5)
      CALL HBOOK1(813,'# OF HITS /EVENT$',100,0.5,100.5)
      CALL HBOOK1(814,'# OF DIGITS /EVENT$',100,0.5,100.5)
      CALL HBOOK1(821,'LENGTH OF CLUSTER$',8,-0.5,7.5)
      RMXE = MXEMD + 0.5
      CALL HBOOK1(831,'# OF HITS IN EACH MODULE$',MXEMD,0.5,RMXE)

      CALL HBOOK1(841,'# OF TRACKS / EVENT$',20,0.5,20.5)
      RETURN
      END
