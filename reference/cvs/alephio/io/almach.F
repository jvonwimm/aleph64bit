       SUBROUTINE  ALMACH (NAME)
C --------------------------------------------------------------------
CKEY IO MACHINE / USER
C - F.Ranjard - 921014
C! returns the Historian flags used to make the library
C - Modified by - P. Comas - 940608
C                distinguish between VAX or Alpha
C - Output : NAME  / A : Machine name
C ----------------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) NAME
      CHARACTER*8 FLAG
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      INCLUDE '($SYIDEF)'
      INTEGER*4 LIB$GETSYI
#endif
C -----------------------------------------------------------------------
      FLAG = 'UNKNOWN '
#if defined(IBM)
      FLAG = 'IBM'
#else
#if defined(ALEPH_DEC)
#if defined(UNIX)
      FLAG = 'DECS'
#else
      CALL LIB$GETSYI(SYI$_ARCH_NAME,,FLAG)
#endif
#else
#if defined(APOLLO)
      FLAG = 'APOLLO'
#else
#if defined(ALEPH_HP)
      FLAG = 'HP'
#else
#if defined(RS6K)
      FLAG = 'RS6K'
#else
#if defined(ALEPH_SGI)
      FLAG = 'SGI'
#else
#if defined(ALEPH_LINUX)
      FLAG = 'LINUX'
#else
#if defined(UNIX)
      FLAG = 'UNIX'
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
      LE = LNBLNK(FLAG)
      LM = LEN (NAME)
      NAME = ' '
      L = MIN (LE,LM)
      IF (L.GT.0) NAME = FLAG(1:L)
      END
#endif
