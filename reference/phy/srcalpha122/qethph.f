      SUBROUTINE QETHPH ( XPHI , XTET , SK ,EPHI , ETET  )
CKEY  FILL / INTERNAL
C ----------------------------------------------------------------------
C! From Space coordinate calculate PHI, TETA ,IK in Ecal numbering
C  Called from FIXRTRL
C                                    Author M.N Minard 10/11/93
C
C      Input    XPHI   Phi angle in radian
C               XTET   Theta angle in radian
C               SK     Stack number for extrapolation
C      Output   ETET   Theta in ECAL numbering
C               EPHI   Phi in ECAL numbering
C ----------------------------------------------------------------------
       EXTERNAL EFNDSC,EFNDMD,EFNDCL,EFNDLG,EFNDST
       INTEGER EFNDSC,EFNDMD,EFNDCL,EFNDST,EFNDLG
       DIMENSION PIN (3,2) , POUT (3,2) ,PVAL(3)
       DIMENSION DIR (3) , VMID (3),VEDG(3)
       CHARACTER*16 WRONG
C ----------------------------------------------------------------------
       DIR (1 ) = COS (XPHI) * SIN(XTET)
       DIR (2) = SIN(XPHI) * SIN (XTET)
       DIR ( 3 ) = COS (XTET)
       CALL EMPACT ( DIR , NSC , PIN ,POUT)
       IF (  NSC.EQ.0) NSC = 1
       DO 10 IL =1,3
       PVAL( IL ) = PIN (IL,1)+SK*(POUT(IL,NSC)-PIN(IL,1))
 10    CONTINUE
       ISC = EFNDSC(PVAL)
       IMOD = EFNDMD(ISC,PVAL)
       ICOL = EFNDCL(ISC,IMOD,PVAL,'ALEPH')
       IROW = EFNDLG(ISC,IMOD,PVAL,'ALEPH')
       IK   = EFNDST(ISC,IMOD,PVAL,'ALEPH')
       ROW = IROW
       COL = ICOL
       SK = IK
       CALL ESRPT ('ALEPH',ROW,COL,SK,VMID)
       CALL ESRPT ('ALEPH',ROW+0.5,COL+0.5,SK,VEDG)
       DTHIT = ATAN2 (SQRT(PVAL(1)**2+PVAL(2)**2),PVAL(3))-
     &         ATAN2 (SQRT(VMID(1)**2+VMID(2)**2),VMID(3))
       DTREF = ATAN2 (SQRT(VEDG(1)**2+VEDG(2)**2),VEDG(3))-
     &         ATAN2 (SQRT(VMID(1)**2+VMID(2)**2),VMID(3))
       DROW =  DTHIT/DTREF
       DCOL =(ATAN2(PVAL(2),PVAL(1))-ATAN2(VMID(2),VMID(1)))
     &      / (ATAN2(VEDG(2),VEDG(1))-ATAN2(VMID(2),VMID(1)))
       EPHI = COL + DCOL
       ETET = ROW + DROW
       RETURN
       END
