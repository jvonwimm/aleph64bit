#if defined(IBM)
C     erase existing file if any
         MSG = 'EXEC ERASE '//TFIL
         LG =LENOCC(MSG)
         CALL VMCMS (MSG(1:LG),IER)
#endif
