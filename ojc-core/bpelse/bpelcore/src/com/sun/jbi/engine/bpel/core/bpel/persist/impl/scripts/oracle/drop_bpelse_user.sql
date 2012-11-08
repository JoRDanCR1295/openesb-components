--Drop the user that was created earlier. Note that if you chose a different name for the 
--user while creating the user, you will have to specify that name here.
DROP USER BPELSE_USER CASCADE;

--Drop the tablespace that was created earlier. Note that if you chose a different name for 
--the tablespace while creating the user, you will have to specify that name here.
DROP TABLESPACE BPELSE_USER_DB INCLUDING CONTENTS AND DATAFILES CASCADE CONSTRAINTS;

--Manually delete the datafiles that were created. If you used the defaults while creating
--the datafiles, the names would be BPELSE_USER_DB1.dat'and 'BPELSE_USER_DB2.dat'