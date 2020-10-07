CREATE or REPLACE PACKAGE scanbook_version 
IS 
   FUNCTION give_ProgramsVersion RETURN VARCHAR2; 
END scanbook_version; 
/ 
show errors 
CREATE or REPLACE PACKAGE BODY scanbook_version 
IS 
FUNCTION give_ProgramsVersion 
   RETURN VARCHAR2 
IS 
BEGIN 
   RETURN 'V0060 | Tue Oct 14 10:33:28 2003' ; 
END; 
END scanbook_version; 
/ 
show errors 
alter package general_procedures compile ; 
