CREATE or REPLACE PACKAGE scanbook_dataStatus 
IS
   FUNCTION give_dataStatus RETURN VARCHAR2;

END scanbook_dataStatus;
/
show errors


CREATE or REPLACE PACKAGE BODY scanbook_dataStatus
IS

FUNCTION give_dataStatus
/*
|| Returns 'OK' if No Oracle Update is running.
|| This function will be replaced automaticly, just before 
|| an Oracle update will be submitted, to return some other value. 
|| Will be replace again to return 'OK' after the update installation ended.
*/
   RETURN VARCHAR2
IS
BEGIN

   RETURN 'OK';

END;

END scanbook_dataStatus;
/
show errors
alter package general_procedures compile ; 








