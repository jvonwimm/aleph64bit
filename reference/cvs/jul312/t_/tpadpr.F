      SUBROUTINE TPADPR(IRLST,LEN,ICLDR,ICLST,IFLNK,IOPEN,IORD,NCLUS)
C
C----------------------------------------------------------------------
C! Pad cluster pat. recognition
C! Reference: ALEPH Note 159  24-4-86
C!
C!   Author:   P.S.Marrocchesi    1-07-85
C!   Modified: R. Johnson        18-12-86
C!
C!   Input:
C!  - IRLST( hit_addr )  =  Rowlist array containing valid HITS
C!                          information coded in 32 bit words .
C!  - LEN                =  Length of the ROWLIST in longwords :
C!                          total number of hits in the pad row
C!  - IFLNK,IOPEN        =  Work space for this routine.
C!
C!   Output:
C!  - NCLUS              =  Number of reconstructed clusters
C!  - list buffer           This is internal to the LISPAK and is
C!                          accessed when LISPAK routines are called
C!                          to get the cluster lists.
C!  - ICLDR              =  Pointer to cluster directory (for LISPAK)
C!                          This points to a list of cluster indices
C!                          for valid clusters.  These indices then
C!                          are to be used for ICLST(ic).
C!  - ICLST(ic)          =  Pointers to the list of pulses for each
C!                          cluster.
C!  - IORD(ic)           =  Order flag for each cluster.
C!                          0 = cluster is in order
C!                          1 = cluster must be ordered later
C!
C!  - Dim. of ICLST,     =  max number of clusters in one PADROW :
C!            IFLNK,        this number can be as high as
C!            IOPEN,        2048 x 3 since the  maximum number
C!                          of TPD modules in a row  is 3 and
C!                          and the hitlist memory size
C!                          is 2k longwords per module.
C!                          Set at least as large as the number of
C!                          pulses in the row (LEN).
C!  - Parameters:
C!                 NLAP  =  pulses from two adjacent pads are
C!                          said to " overlap in time " when
C!                          they have _ at least _ NLAP samples
C!                          in common .
C!                 ITHRE =  threshold value for overlap parame-
C!                          ter F to define a " backward link ".
C!
C!    Called by TISLND
C!
C!    Routines used:
C!      This program makes use of the LISPAK routines in order to
C!      form and manipulate linked lists of pointers into the rowlist
C!      for the pulses in each cluster.  At the finish, there should be
C!      one such linked list for each cluster, and the pointer to
C!      the beginning of a list may be found in ICLIST(ic) in common
C!      TCLIST.  LISPAK must be used to access the lists themselves.
C!
C!    Description
C!    ===========
C!      This routine loops over the pulses in the TPAD rowlist in
C!      order and forms clusters as it goes along by adding to lists
C!      maintained by LISPAK.  Two pulses are considered part of a
C!      cluster if they are on adjacent pads and overlap in time
C!      by at least NLAP buckets.  No pulse height information is
C!      used.  When a pulse is found to overlap with two existing
C!      clusters, then the two clusters are merged together by a
C!      call to a LISPAK routine.
C!----------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION IRLST(LEN)
      INTEGER IORD(*),ICLST(*),IFLNK(2,*),IOPEN(2,*)
      DATA NLAP/1/, ITHRE/1/
C
C---------------------------------------------------------------
C
C  initialize cluster lists and cluster directory
C
      IPNT = ICLDR
      IF( IPNT.EQ.0 ) GO TO 2
    1 CALL TLNEXT(IPNT,ICL)
      IF( IPNT.NE.0 ) THEN
        CALL TLSCRA( ICLST(ICL) )
        GO TO 1
      ENDIF
      CALL TLSCRA(ICLDR)
C
C  initialize service variables
C
 2    NCLUS = 0
      IPHIT= 1
      LNKFW = 0
      INDEX  = 2
C
C--  start ROWLIST scan :   get channel number
C
      ICHAN=IBITS(IRLST(IPHIT),24,8)
C
C--  non adjacent channel : new stripe
C
 50   ICOLD = ICHAN
      LNKFW = 0
C
C-- same channel : open a new cluster
C
 80   NCLUS = NCLUS + 1
      IORD(NCLUS)=0
      CALL TLCONS( IPHIT,ICLST(NCLUS))
      LNKFW=LNKFW+1
      IFLNK(3-INDEX,LNKFW) = IPHIT
      IOPEN(3-INDEX,LNKFW) = NCLUS
C
C--  continue scan of the rowlist :
C
C     open new clusters until channel number changes
C
 81   IPHIT = IPHIT + 1
      IF( IPHIT.GT.LEN ) GO TO 250
      ICHAN=IBITS(IRLST(IPHIT),24,8)
      IDEL = IABS(ICHAN - ICOLD) - 1
C
      IF ( IDEL) 80,85,50
C
C-- contiguous channel : loop on parent links
C
C
 85   ICOLD = ICHAN
      LINKF = LNKFW
      LNKFW = 0
C
C         swap r/w buffers ( parent links and forward links )
C
C         read buffer  :      INDEX    parent  links
C         write buffer :  3 - INDEX    forward links
C
      INDEX = 3 - INDEX
C
      LNK=0
      LINKB = 0
C
C--  Loop on parent links :
C
 88   LNK=LNK + 1
      IF( LNK.GT.LINKF ) GO TO 211
C
      GO TO 91
C
C  rowlist scan :
C
C
 90   IPHIT = IPHIT + 1
      IF(IPHIT.GT.LEN) GO TO 250
C
      ICHAN=IBITS(IRLST(IPHIT),24,8)
      IDEL = IABS(ICHAN - ICOLD) - 1
C
      IF ( IDEL ) 91,85,50
C
C  same channel : apply overlap criterion
C
C
 91   CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C*********************
C    OVERLAP TEST    *
C*********************
C
C--  unpack 2 rowlist longwords
C
      LEN1=IBITS(IRLST(IFLNK(INDEX,LNK)),16,8)
      IT01=IBITS(IRLST(IFLNK(INDEX,LNK)),0,9)
      LEN2=IBITS(IRLST(IPHIT),16,8)
      IT02=IBITS(IRLST(IPHIT),0,9)
C
      IOVL=1
      IT = IT02 - IT01
      IFFF = IT + LEN2 - LEN1
C
      IF(IT.GT.0) THEN
      IF( IT .GT. ( LEN1 - NLAP ) ) IOVL = 0
      ELSE
      IF( -IT .GT. ( LEN2 - NLAP ) ) IOVL = 0
      ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( IOVL.EQ.1 ) THEN
C
C--      overlap found
C
C      IFFF <= 0           :  continue rowlist scan    ( same parent lin
C
C      0 < IFFF <= ITHRE  :  continue rowlist scan    ( next forward lin
C
C      IFFF >  ITHRE      :  backward link
C
C
C--  current hit belongs to cluster ICL
C
      ICL = IOPEN(INDEX,LNK)
C
      IF( LINKB.EQ.0 ) GO TO 92
C
      IF( LINKB.EQ.ICL ) THEN
      LINKB=0
      GO TO 90
      ENDIF
C
C
C-- check whether the two clusters collapse into one :
C
C     . current hit overlaps with an element of cluster ICL
C
C     . the same hit was found to overlap with the preceding
C       forward link ( which belongs to cluster LINKB )
C
C-- merge the two clusters into one :
C
C     . add LINKB list to ICL list
C
C     . scratch LINKB list
C
C     . update the total number of clusters found
C
      CALL TLADD( ICLST(LINKB),ICLST(ICL) )
      CALL TLSCRA(ICLST(LINKB))
      IOPEN( 3-INDEX,LNKFW ) = ICL
      IORD(ICL)=1
      LINKB = 0
      GO TO 93
C
 92   CALL TLCONS( IPHIT,ICLST(ICL))
C
C  store :   - current hit as a forward link
C            - corresponding cluster identifier
C
      LNKFW = LNKFW + 1
      IFLNK( 3-INDEX,LNKFW ) = IPHIT
      IOPEN( 3-INDEX,LNKFW ) = ICL
C
 93      IF( IFFF.GT.ITHRE ) THEN
C       . flag occurrence of a backward link
C       . next parent link
C
         LINKB = ICL
         GO TO 88
         ENDIF
C
         IF( IFFF.LE.0 ) GO TO 90
C
C--  continue scan  :  -  next parent link
C                      -  next rowlist hit
C
         LNK=LNK+1
         IF( LNK.GT.LINKF ) GO TO 211
         GO TO 90
C
      ENDIF
C
C--   OVERLAP CRITERION NOT MET  ( IOVL = 0 )
C
C     IFFF <= 0     : CONTINUE scan ( same forward link )
C     IFFF >  0     : STOP     scan ( next forward link )
C
C
          IF( LINKB.NE.0 ) THEN
          LINKB = 0
          GO TO 90
          ENDIF
C
      IF( LNK.EQ.LINKF .OR. IFFF.LE.0 ) THEN
C
C--      open a new cluster
C
          NCLUS=NCLUS+1
          IORD(NCLUS)=0
          CALL TLCONS(IPHIT,ICLST(NCLUS))
C
          LNKFW=LNKFW+1
          IFLNK(3-INDEX,LNKFW)=IPHIT
          IOPEN(3-INDEX,LNKFW)=NCLUS
C
      ENDIF
C
      IF( IFFF.LE.0 ) GO TO 90
      GO TO 88
C
 211  CONTINUE
C
C  continue scan of the rowlist
C
      GO TO 81
C
  250 CONTINUE
C
C     End of rowlist:  set up clusters directory
C
      NC=0
      DO 300 I=1,NCLUS
        IPNT=ICLST(I)
        IF (IPNT.EQ.0) GO TO 300
        CALL TLCONS(I,ICLDR)
        NC=NC+1
  300 CONTINUE
      NCLUS=NC
C
      RETURN
      END
#endif
