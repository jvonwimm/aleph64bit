C
         PARAMETER ( NTRMAX = 5 )
      COMMON/ECOBJC/  IFLGLO,IFLGTR,IFLGHA,IFLGMC,IFLGP0,IFLG2G,
     1                IFLGMP,IFLGBA,IFLGEL,IFLGCG,IFLGEG,IFLGCM,IFLGCH,
     2                ICLCUR,ICHRGE,ISTLVL,IFNLVL,ICDMIN,NBMOHI,
     3                XBARCL(3),ETOCLU,ENLVCL(3),
     4                NTRACK(NTRMAX),PINTPC(NTRMAX)
C
#if defined(DOC)
C!
C!           ECOBJC - COMMON USED TO ANALYSE A LOCAL CLUSTER
C!                    IFLG...  :   FLAGS OF IDENTIFICATIONS
C!                    ICLCUR   :   CLUSTER NUMBER
C!                    ICHRGE   :   CHARGE OF CLUSTER
C!                    NTRACK   :   #TRACKS ON CLUSTER
C!                    ISTLVL   :   FIRST LEVEL OF CLUSTER
C!                    IFNLVL   :   LAST LEVEL OF CLUSTER
C!                    NBMOHI   :   TO TAG CLUSTER ON 1/2 MODULES
C!                    XBARCL   :   CLUSTER BARYCENTER
C!                    ETOCLU   :    ENERGY OF CLUSTER
C!                    ENLVCL   :    ENERGY OF CLUSTER
C!                    PINTPC   :    MOMENTUM OF FIRST TRACK ON CLUSTER
C!                    NTRMAX   :    MAXIMUM NUMBER OF ASSOCIATED TRACKS(
C!
#endif
