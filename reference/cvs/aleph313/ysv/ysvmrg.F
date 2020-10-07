        SUBROUTINE YSVMRG(NPAIR,IER)
C--------------------------------------------------------------------------
C!    Combine pair vertices sharing tracks
C
C     Looks for pair vertices which are linked together by shared tracks
C     and are close together.  E.g. if vertex A contains tracks 1 and 2,
C     vertex B contains tracks 2 and 3, and vertex C contains tracks 3 and
C     4, and all vertices are close together, tracks 1, 2, 3, and 4 are
C     considered to form a "cluster".
C
C     Once all clusters are found, each cluster is fit to a single
C     vertex.  The fit is also done leaving out each track and fitting
C     the remaining N-1.  Tracks are removed until the chi^2 stops
C     improving significantly.
C
C       Author:  D.Casper  10/7/96
C
C       Input:  NPAIR  Number of good pairs at start of YSVX/2 bank
C       Returns: IER   0 if no error
C
C       Input banks:    YSVX/-2     Pair vertices
C
C       Output bank:    YSVX/NR(<0) Each YSVX bank has a negative NR, with
C                                   ABS(NR) = vertex multiplicity.  Hence
C                                   all three-track vertices are in NR=-3.
C
C--------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ysvxjj.h"
#include "ysvcut.h"
        LOGICAL CONNECT
        COMMON /YSMRWK/ INDR, INDZ, INDC
        INTEGER INDS(5), INDN(5), INDB(32*5), INDK(5)
        DATA INDR,INDZ,INDC /3*0/
        DATA NYSVX/0/
        SAVE NYSVX
C
#include "bmacro.h"
C
        IF (NYSVX.EQ.0) THEN
            NYSVX = NAMIND('YSVX')
        ENDIF
        IYTEV = 0
C
C+  Locate YSVX
C
        KYSVX = IW(NYSVX)
        IF (KYSVX.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
        IF (IW(KYSVX-2).NE.-2) THEN
            IER = -2
            GOTO 999
        ENDIF
C
C+  Emit a warning if we have too many pairs to deal with
C+  (The excess will just remain as pairs and cannot be merged)
C
        IF (NPAIR.GT.32) NPAIR = 32
C
C+  Make a workbank to hold the radii and z of all vertices
C
        CALL WBANK(IW,INDR,NPAIR,*999)
        IW(INDR-3) = INTCHA('YSVR')
        CALL WBANK(IW,INDZ,NPAIR,*999)
        IW(INDZ-3) = INTCHA('YSVZ')
        DO IV = 1, NPAIR
            RW(INDR+IV) = SQRT(RTABL(KYSVX,IV,JYSVVX)**2
     &                      + RTABL(KYSVX,IV,JYSVVY)**2)
            RW(INDZ+IV) = RTABL(KYSVX,IV,JYSVVZ)
        ENDDO
C
C+  Make work banks to hold the mask of all tracks connected to our seed
C
        CALL WBANK(IW,INDC,LMHLEN+6*NPAIR,*999)
        IW(INDC-3) = INTCHA('YSCL')
        IW(INDC+LMHCOL) = 6
        IW(INDC+LMHROW) = 0
C
C+  Loop over seed vertices
C
        MASKALL = 0
        DO ISEED = 1, NPAIR-1
C
C+  Was this vertex already included in another cluster?
C
          IF(IAND(MASKALL,IBSET(0,ISEED-1)).EQ.0) THEN
C
C+  Find all (higher numbered) vertices which are connected with vertex
C+  through some combination of tracks.
C
C+  Find the r and z of this vertex - only consider combinations which are
C+ vaguely consistent with this
C
            RSEED = RW(INDR+ISEED)
            ZSEED = RW(INDZ+ISEED)

C+  Load the track mask of the seed vertex
C
            CALL UCOPY(IW(KROW(KYSVX,ISEED)+JYSVTM),INDS,5)
C
C+  Create the mask of connected vertices
C
            MASKV = IBSET(0,ISEED-1)
            LMASK = 0
C
C+  Loop until no more connected vertices are found
C
            DO WHILE (MASKV .NE. LMASK)
              LMASK = MASKV
              DO IV = ISEED+1, NPAIR
C
C+  Check whether this vertex is already included in a cluster
C
                IF (IAND(MASKALL,IBSET(0,IV-1)).EQ.0 .AND.
     &              IAND(MASKV,IBSET(0,IV-1)).EQ.0) THEN
C
C+  See if it's close enough in radius
C
                  IYSVX = KROW(KYSVX,IV)
                  IF(ABS(RW(INDR+IV)-RSEED).LE.YSCTDR .AND.
     &              ABS(RW(INDZ+IV)-ZSEED).LE.YSCTDR) THEN
C
C+  If so, see if it shares any tracks with the cluster 
C
                    IWRD = 1
                    CONNECT = .FALSE.
                    DO WHILE (IWRD.LE.5 .AND. .NOT. CONNECT)
                      CONNECT = CONNECT.OR.
     &                  (IAND(IW(IYSVX+JYSVTM+IWRD-1),
     &                   INDS(IWRD)).NE.0)
                      IWRD = IWRD + 1
                    ENDDO
                    IF (CONNECT) THEN
C
C+  Add this vertex to the vertex mask, and add it's tracks to those in the
C+  cluster.
C
                      MASKV = IBSET(MASKV,IV-1)
                      DO IWRD = 1, 5
                        INDS(IWRD) = IOR(INDS(IWRD),
     &                      IW(IYSVX+JYSVTM+IWRD-1))
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
C
C+  We have a cluster.  Flag it so it is not used in a later cluster
C
            MASKALL = IOR(MASKALL,MASKV)
C
C+  Store this cluster in a list
C
            CALL UBITS(INDS,5*32,INDB,NTS)
            IF (NTS.GT.2) THEN
                IINDC = KNEXT(INDC)
                CALL UCOPY(INDS,IW(IINDC+1),5)
                IW(IINDC+6) = NTS
                IW(INDC+LMHROW) = IW(INDC+LMHROW) + 1
            ENDIF
          ENDIF
        ENDDO
C
C+  Sort clusters, taking the smallest first for reasons of memory
C+  conservation
C
        CALL SORTI(IW(INDC+LMHLEN+1),LCOLS(INDC),LROWS(INDC),6)
C
C+  Now loop over all clusters, fitting combinations of tracks
C
        DO I = 1, LROWS(INDC)
          CALL UCOPY(IW(KROW(INDC,I)+1),INDS,5)
          CALL VZERO(INDK,5)
C
C+  Get the list of tracks which could be associated
C            
          CALL UBITS(INDS,5*32,INDB,NTS)
C
C+  Loop while the seed mask has at least two tracks
C
          DO WHILE (NTS.GE.2)
C
C+  Number of tracks in mother is number in seed
C
            NTM = NTS
C
C+  Fit all tracks in seed, get BOS address of this vertex
C
            IYSVXM = 0
            IROWM = 0
            CALL YSFTLD(INDS,IER)
            IF (IER.EQ.0) CALL YSVFIT(.FALSE.,-NTM,IER,IGARB)
            IF (IER.EQ.0) THEN
              KYSVXM = NLINK('YSVX',-NTM)
              IYSVXM = KROW(KYSVXM,LROWS(KYSVXM))
              IROWM = LROWS(KYSVXM)
C
C+  Loop until we can't improve the vertex, or we don't have enough
C+  tracks left
C
              DO WHILE (NTM.GE.3)
                KYSVXD = 0
                IYSVXD = 0
                IROWD = 0
                CALL UBITS(IW(IYSVXM+JYSVTM),5*32,INDB,NTB)
C
C+  Try removing each track individually from the fit
C
                DO IBIT = 1, NTB
C
C+  Build track mask
C
                  CALL UCOPY(IW(IYSVXM+JYSVTM),INDN,5)
                  ITWRD = (INDB(IBIT)-1)/32+1
                  ITBIT = MOD(INDB(IBIT)-1,32)
                  IF (ITWRD.GT.5 .OR. ITWRD.LT.1) GOTO 999
                  INDN(ITWRD) = IBCLR(INDN(ITWRD),ITBIT)
C
C+  Set-up and fit
C
                  CALL YSFTLD(INDN,IER)
                  IF (IER.EQ.0)THEN
                    CALL YSVFIT(.FALSE.,-(NTB-1),IER,IGARB)
                    IF (IGARB.GT.0) THEN
                        KYSVXM = NLINK('YSVX',-NTM)
                        IYSVXM = KROW(KYSVXM,IROWM)
                        IF (KYSVXD.GT.0) THEN
                            KYSVXD = NLINK('YSVX',-(NTB-1))
                            IF (IROWD.GT.0) IYSVXD = KROW(KYSVXD,IROWD)
                        ENDIF
                    ENDIF
                  ENDIF
C
C+  Check the result
C
                  IF (IER.EQ.0)THEN
                    IF (KYSVXD.EQ.0)
     &                  KYSVXD = NLINK('YSVX',-(NTB-1))
                    IYSVXC = KROW(KYSVXD,LROWS(KYSVXD))
                    IF (IYSVXD.EQ.0) THEN
                      IYSVXD = IYSVXC
                      IROWD = LROWS(KYSVXD)
                    ELSE
                      IF (RW(IYSVXC+JYSVC2).LT.RW(IYSVXD+JYSVC2)) THEN
                        IW(IYSVXD+JYSVQU) = IOR(IW(IYSVXD+JYSVQU),128)
                        IYSVXD = IYSVXC
                        IROWD = LROWS(KYSVXD)
                      ELSE
                        IW(KYSVXD+LMHROW) = IW(KYSVXD+LMHROW) - 1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO
C
C+  We now know which track gives the biggest improvement in chi^2 when
C+  removed.  See if the improvement is significant.
C
                IF (IYSVXD.NE.0) THEN
                  IF (RW(IYSVXM+JYSVC2)-RW(IYSVXD+JYSVC2).GT.YSCTCR)THEN
                    IW(IYSVXM+JYSVQU) = IOR(IW(IYSVXM+JYSVQU),128)
                    DO IWRD = 1, 5
                      INDK(IWRD) = IOR(INDK(IWRD),
     &                  IEOR(IW(IYSVXM+JYSVTM+IWRD-1),
     &                       IW(IYSVXD+JYSVTM+IWRD-1)))
                    ENDDO
                    IYSVXM = IYSVXD
                    IROWM = IROWD
                    NTM = NTM - 1
                  ELSE
                    NTM = 0
                  ENDIF
                ELSE
                  NTM = 0
                ENDIF
              ENDDO
C
C+  Now use any tracks removed as seeds for a new fit
C
              CALL UCOPY(INDK,INDS,5)
              CALL VZERO(INDK,5)
              CALL UBITS(INDS,5*32,INDB,NTS)
            ELSE
C
C+  Get here if our fit of all tracks failed to produce a valid vertex.
C+  Remove one track and try again.
C
              ITWRD = (INDB(NTS)-1)/32+1
              ITBIT = MOD(INDB(NTS)-1,32)
              IF (ITWRD.GT.5 .OR. ITWRD.LT.1) GOTO 999
              INDK(ITWRD) = IBSET(INDK(ITWRD),ITBIT)
              INDS(ITWRD) = IBCLR(INDS(ITWRD),ITBIT)
              NTS = NTS - 1
            ENDIF
          ENDDO
        ENDDO
        IER = 0
 999    CONTINUE
        CALL YSVSRT
        CALL YSFCLR
C Drop work banks INDR, INDZ, and INDC
        IW(1) = 3
        CALL WDROP(IW,INDR)

        RETURN
        END
#endif
