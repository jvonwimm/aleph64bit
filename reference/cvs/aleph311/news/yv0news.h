 ! ALEPHLIB 308
    YFQERQ : Use double precision internally, keep ERQ2 non-zero (M.Cattaneo)

 ! correction file no.1 for ALEPHLIB 30.7
    YFMV0V : Protect against precision problem (M.Cattaneo)

 ! ALEPHLIB 30.6
    YMFMIN,YMFV0V : fix variable type inconsistencies in function calls, 
                    for Linux                            (A.Waananen)
 ! ALEPHLIB 21.6
    YTCONV - Use correct HAC parameter JYNMVS instead of obsolete JYNMSC
    YTRV0S - Use correct HAC parameter JYNMVS instead of obsolete JYNMSC
             (W.Manner 10/02/97)
 ! ALEPHLIB 21.3
    YV1INI - Fix multiline strings (M.Cattaneo)
 ! ALEPHLIB 21.2
   YPSIVE, YV0ONH, YMFMIN, YFPSIN - Speed up by calling NAMIND only on
           first call and remembering for subsequent calls (O. Callot)
 ! ALEPHLIB 20.8
   YDISMC - Fix calculation of symmetry point when one center
            lies inside the other circle. This happens for
            like-signed tracks (P.Rensing).
 ! ALEPHLIB 15.7
   YV1INI - replace WRITE(6) with IF (IW(6).GT.0) WRITE(IW(6))
 ! ALEPHLIB 15.5
   YNV0NF - replace the protection of 15.4 with a weaker one
 ! ALEPHLIB 15.4
   YNV0NF - add  a protection against huge values in XMT matrix or
            VT vector which make DEQINV CERNLIB routine crash
 ! ALEPHLIB 15.2
   set keywords on CKEY cards

 ! ALEPHLIB 14.6
   YDISMC - add a protection against  negative sqrt
  -------------------------------------------------------
   =====================================
    Documentation on the YMFV0V package
   =====================================
   Author:M.A.Ciocci,L.Rolandi (tel.6412)
   Long writeup : will be avalaible
   *******************
   *News from 28/1/93*
   *******************
   ************************
   * VERY IMPORTANT CHANGE*
   ************************
   From now the NR of the yv0v bank is related  to the NR of the
   YV1C bank. See later for the different meaning of the NR values
   From now the package will use the FRFT bank with the lowest NR.
   So everywhere  it will use the  refitted tracks including minivertex
   information.
   Implemented subroutine:
   ychiv2,yv1ini
   New subroutine ychi00

      FUNCTION YCHIV2 (KPOI,VPRIM,SIVPR,IFAIL)
C----------------------------------------------------------------------
C! Calculate the chi2 of a track,whith the constraint of primary vertex
C
CKEY Chi2 that a track came from primary / YV0
C    AUTHOR: M.A.CIOCCI,L.ROLANDI 24/3/88
C    MODIFIED:M.A.CIOCCI 20/2/90
C    Modified: J.Sedgbeer 19/6/91. Bug fix in ELIP1(4) = ........
C    MODIFIED:M.A.CIOCCI 28/1/93. Now the chi2 that a track comes
C                                 from the primary includes the
C                                 covariance of the primary.
C
C          DESCRIPTION
C          ===========
C  COMPUTES THE CHI2 OF A TRACK WITH THE CONSTRAINT
C   OF THE PRIMARY VERTEX, ASSUMED CLOSE TO THE ORIGIN
C
C    INPUT:
C               KPOI/I  NUMBER OF THE TRACK IN THE
C                       FRFT  BANK
C               VPRIM(3)/R COORDINATES OF THE PRIMARY
C                      VERTEX
C               SIVPR(3,3)/R COVARIANCE MATRIX OF THE PRIMARY
C                       vertex
C    OUTPUT:
C               IFAIL/I IF 0 ALL IS OK
C                       IF 1 BANK FRFT MISSING OR PROBLEM WITH COV MAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION YCHI00 (VSEC,SVSEC,VPRIM,SIVPR,IFAIL)
C----------------------------------------------------------------------
C! Calculate the c2 variation constraining a vertex to the primary vertex
C
CKEY Chi2 variation constraining  a vertex to the primary / YV0
C    AUTHOR: M.A.CIOCCI 28/1/93
C
C          DESCRIPTION
C          ===========
C  Computes the chi2 variation constraining a vertex to the primary
C  vertex
C
C    INPUT:
C               VSEC/R    COORDINATES OF A VERTEX
C               SVSEC/R   COVARIANCE OF VSEC VERTEX COORDINATES
C
C               VPRIM/R   COORDINATES OF THE PRIMARY
C                         VERTEX
C               SIVPR/R   COVARIANCE MATRIX OF THE PRIMARY
C
C    OUTPUT:
C               IFAIL/I IF 0 ALL IS OK
C                       IF 1 PROBLEM WITH COV MAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE YV1INI(LCONS,IER,NR)
C-----------------------------------------------------------------------
C! Read the cuts for the V0 reconstruction, YV1C (DBASE)
CKEY Read the cuts for V0 recon. from yv1c bank / YV0
C   Author:  M.A.Ciocci 20/2/90
C   Modified: J.Sedgbeer 09/04/91. YV1C bank and common/YV0CUT/ extended
C   Modified: M.A.Ciocci 28/1/93. YV1C bank have now several values of NR
C     The  different cuts used in  different periods
C     of Aleph data tacking (89-90 91-92 93...) are sumarized
C     by the different NR (1, 2, 3) of the yv1c bank. So on the data base
C     the yv1c is present with 3 different value for NR.
C     If the user decide to change cuts will be sufficient to give
C     by cards a bank called yv1c with NR greater than 3 and change
C     the cuts value
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   Since an Aleph Note on the subject is  available, here we will
   describe just the usage of the package. The idea is to
   give for any event some quantities relied to the V0.
   There is a single routine (ymfv0v) interfacing the package to
   the user program.
   This package drop the yv0v bank and rewrite the same bank.
   (look at yv0v bank -> sbank yv0v)
   (it can  use the mean value for run of beam beam crossing point).
   It give (if you want) the histograms for v0 candidate after
   any sequential cut (yv1c).
   And give as argument output

       IERCO/INTEGER         code for error:
                             if 0 all is ok
                             if 1, problems with covariance matrix
                             of charged tracks in ychiv2
                             if 2, problems with covariance matrix
                             of charged tracks in ynv0ve
                             if 3, problems with covariance matrix
                             of charged tracks in ynvonf
                             if 4, problems in yvcame with frft
                             if 5, problems inverting covariance
                             matrix of reconstructed V0 vertex
                             candidate
                             if 6, the number of V0 candidates is
                             greater than 20
                             if 7, no booked enlarged space for yv0v
                             if 8, problems with working banks
                             if 9 no space for yv0v bank
                             if 10 no yv1c on dbase or cards

       NUMV0/INTEGER         Number of V0 candidates (at
                             maximum 50)

       CHIOR(2,50)/REAL      chi2 increasing constraining
                             the track coming from V0 candidate
                             to the origin
      QGA(50)/REAL          Q value in the gamma hypothesis
      CHQGA(50)/REAL        chi2 of Q value in the gamma hypothesis
      QLA(50)/REAL          Q value in the lambda hypothesis
      CHQLA(50)/REAL        chi2 of Q value in the lambda hypothesis
      QAL(50)/REAL          Q value in the antilambda hypothesis
      CHQAL(50)/REAL        chi2 of Q value in the antilambda
                            hypothesis
      QK0(50)/REAL          Q value in the k0 hypothesis
      CHQK0(50)/REAL        chi2 of Q value in the k0 hypothesis
      XMGA(50)/REAL         invariant mass in the gamma hypothesis
      CXMGA(50)/REAL        chi2 of invariant mass in the gamma
                            hypothesis
      XMLA(50)/REAL         invariant mass in the lambda hypothesis
      CXMLA(50)/REAL        chi2 of the invariant mass in the \
                            lambda hypothesis
      XMAL(50)/REAL         invariant mass in the antilambda
                            hypothesis
      CXMAL(50)/REAL        chi2 of invariant mass  in the
                            antilambda hypothesis
      XMK0(50)/REAL         invariant mass in the k0 hypothesis
      CXMK0(50)/REAL        chi2 of the invariant mass in the
                            k0 hypothesis
     NCOR(2,50)/INTE        number of coordinates before the
                            reconstructed vertex for tracks
                            coming from v0
     DISCO(2,50)/REAL       radial distance for the furthest
                            coordinate from vertex
-------------------------------------------------------------

With the output quantity, histograms and bank you can study
the capability of package and it can help to make  a fine v0
identification (that can be  different for any kind of physic)

To use this package:
   a) data base must be opened
   b) frft,frtl,pyer must be present ( make unpacking
      tp,it,fi )
   c) yv1c bank (that contains cuts for v0 selection)
      must be present on data base or given by data cards
      if you want change cut for v0 analysis.
   d) fvcl,ficl,ftcl,vdco,itco,tpco must be present
      only if you check the number of coordinates
      before the reconstructed vertex
      ( make unpacking tp,it,fi)
      To understand cuts look the ddl of yv1c bank
      (-> sbank yv1c)
      Beware that the different cuts used in different period
      of Aleph data taking (89-90 91-92 93...) are sumarized
      by the different NR of the yv1c bank. So on the data base
      the yv1c is present with 3 different value for NR
YV1C 1/ 25  1  1  1  99999999
*1989-1990
*      IV     O1     O2     CT     PS         RV    ZV    DI
        1      0.0    0.    2.     0.         180. 220. 0.06
*      CF     CS     CO     MA     PM    NT   CB    NC    MD
       13.    0.92    0.    50.    0.     4    0    0      0.
*      D0     Z0     WK        WL         WG
       999.   999.   0.1E+05  0.1E+05     0.1E+05
YV1C 2/ 25  1  1  1  99999999
*1991-1992
*      IV     O1     O2     CT     PS         RV    ZV    DI
       1      0.0    0.     3.    0.0        180.  220. 0.06
*      CF     CS     CO     MA     PM    NT   CB    NC    MD
       25.    0.5    0.0    9999.  0.1   4     2    10    1.5
*      D0     Z0     WK     WL     WG
       999.   999.   9999. 9999.   9999.
YV1C 3/ 25  1  1  1  99999999
*1993 and later i hope...
*      IV     O1     O2     CT     PS         RV    ZV    DI
       1      0.0    0.     2.    0.0        180.  180. 0.06
*      CF     CS     CO     MA     PM    NT   CB    NC    MD
       13.    0.92   0.0    9999.  0.    5     0    33    0.
*      D0     Z0     WK     WL     WG
       999.   999.   0.5    0.50  0.50

If you run the package without extra cards the dafault value
of NR for the yv1c is NR=3.
In the same time the nr of the yv0v bank produced will be 3
******************************************************************
*     To  change cut change values of cut on the following cards *
*      and add these cards
******************************************************************
YV1C 4/ 25  1  1  1  99999999
*user private cards
*      IV     O1     O2     CT     PS         RV    ZV    DI
       1      0.0    0.     2.    0.0        180.  180. 0.06
*      CF     CS     CO     MA     PM    NT   CB    NC    MD
       13.    0.92   0.0    9999.  0.    5     0    33    0.
*      D0     Z0     WK     WL     WG
       999.   999.   0.5    0.50  0.50
************************************************
*                    iflag=0, primary vertex is assumed to
*                    be (0,0,0)
*   IV/I            iflag=1  primary vertex defined by pyer
*                    bank
*                    iflag=2 primary defined using the jsum bank
*                    (mean value of primary run per run)
************************************************************
   For the calling sequence and arguments
   please look at the routine header. If you want to use it
   naively look at the following example that accesses for any
   event all the information about v0.
Example for cards :
**************************************
*   ALPHA cards for V0 NEW           *
**************************************
*
* the alpha default will use the tracks fitted with minivertex.
* if you want the tracks fitted using only TPC and ITC information
* you must add the FRF0 card
* Galeph xx.x  + Julia xx.x
FILI 'AL1$SCRATCH0:[ALEPH.WEEK.CIOCCIW]QCD21_1000EVTS.POT | NATIVE'
NOV0
* Aleph Data Base
FDBA 'DBASE:ADBSCONS.DAF'
*
* Histos output file
HIST 'V0S.HIS'
*
* time limit
TIME 120
YV1C 4/ 25  1  1  1  99999999
*user private cards
*      IV     O1     O2     CT     PS         RV    ZV    DI
       1      0.0    0.     2.    0.0        180.  180. 0.06
*      CF     CS     CO     MA     PM    NT   CB    NC    MD
       13.    0.92   0.0    9999.  0.    5     0    33    0.
*      D0     Z0     WK     WL     WG
       999.   999.   0.5    0.50  0.50

ENDQ
                            *** EXAMPLE ***
      SUBROUITNE anav0
      ......................
      ......................
C
C+   output of package:dimension
C
      REAL CHIOR(2,50),QGA(50),CHQGA(50),QK0(50),CHQK0(50),QLA(50),
     $CHQLA(50),QAL(50),CHQAL(50),XMGA(50),CXMGA(50),XMK0(50),CXMK0(50),
     $     XMLA(50),CXMLA(50),XMAL(50),CXMAL(50),NCOR(2,50),DISCO(2,50)
C
C+  END OUTPUT DIMENSION
C
C
C+       logical for v0 histograms, if true you will have all histogram
C+       for v0
C
         LOGICAL IBOOK
C
C+       If ibook=.true. Histograms will stat from iv0bk+200
C
         INTEGER IV0BK
C
C+      ask histograms
C
            IBOOK=.TRUE.
C
C+          HISTOGRAM WILL START FROM 1000+200
C
            IV0BK=1000
...................................
..................................
...............................
C
C+     CALL PACKAGE
C
      CALL  YMFV0V(IBOOK,IV0BK,IERCO,NUMV0,CHIOR,QGA,CHQGA,QK0,
     $                  CHQK0,QLA,CHQLA,QAL,CHQAL,XMGA,CXMGA,XMK0,
     $                  CXMK0,XMLA,CXMLA,XMAL,CXMAL,NCOR,DISCO)
C
C+   TEST IERCO
C
         IF(IERCO.NE.0)THEN
C
C+ FIRST CALL M.A.CIOCCI...
C
            GO TO 999
          ENDIF
C
C+        THER'ARE V0 CANDIDATES ?
C
          IF(NUMV0.EQ.0)GO TO 998
             DO 10 I =1,NUMV0
C
C         MAKE YOUR ANALYSIS, FOR EXAMPLE
C         USING THE MONTECARLO TRUTH
C         STUDY HOW CHANGE YOUR PURITY AND THE EFFICIENCY
C         FOR K0 MAKING A CUT ON CHI2 OF
C         INVARIANT MASS (CXMK0)
C
C
C

   10        CONTINUE
 998
............................
............................

 999         CONTINUE
             RETURN
             END

                            *** END EXAMPLE ***
