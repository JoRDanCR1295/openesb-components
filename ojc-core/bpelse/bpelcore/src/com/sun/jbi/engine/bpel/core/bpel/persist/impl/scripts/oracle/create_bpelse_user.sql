--Note that all the names used in this file are prefixed with BPELSE_USER. If you 
--want to create a user with different name and want that user to have separate 
--tablespace, etc, simply find and replace BPELSE_USER with the desired user name.


--You need to login to Oracle as sysdba to run these commands (actually, all of 
--these commands can be run using the system user except the last set as noted). 
--If you are using sqlplus, on command prompt enter:
--sqlplus "sys/<syspassword>@<tnsentry> as sysdba" 
--Note that you need to have sqlplus in the path and a tns entry for the oracle 
--database to connect using this command. 
--You can also connect to the database from Netbeans or other SQL clients by 
--giving the username "sys as sysdba". 
--Note that the password for sys is usually same as that of system. In case it is 
--not, contact your DBA 

--Create a tablespace named BPELSE_USER_DB. Change this value if a different name 
--is desired. This creates a two datafiles of 2GB each. Add more files if a larger 
--size is required. Note that this will create the BPELSE_USER_DB1.dat and 
--BPELSE_USER_DB2.dat in default locations which depend on the Oracle database. 
--Specify a complete path if a different location is desired. For example, 
--'C:\temp\BPELSE_USER_DB1.dat' or '/home/jsmith/BPELSE_USER_DB1.dat'
--Note, we have purposefully not specified the option 
--'EXTENT MANAGEMENT LOCAL SEGMENT SPACE MANAGEMENT AUTO' while creating the 
--tablespace because we observed that with this option performance degrades 
--gradually as more and more records are added to the database  


CREATE TABLESPACE "BPELSE_USER_DB"
 DATAFILE
  'BPELSE_USER_DB1.dat' SIZE 2000M,
  'BPELSE_USER_DB2.dat' SIZE 2000M;

--Create a new user BPELSE_USER. Change the name if so desired. Password will be same
--as the user name by default. This username and password will be used to create the 
--connection pool on the application server. Note that if you used a different 
--tablespace name above, you will have to specify that tablespace name here.
CREATE USER BPELSE_USER IDENTIFIED BY BPELSE_USER
DEFAULT TABLESPACE BPELSE_USER_DB
QUOTA UNLIMITED ON BPELSE_USER_DB
TEMPORARY TABLESPACE temp
QUOTA 0M ON system;


--Modify the user name if the default user name was changed
GRANT CREATE session to BPELSE_USER;
GRANT CREATE table to BPELSE_USER;
GRANT CREATE procedure to BPELSE_USER;


--To run these commands you need to be logged in as "sys as sysdba"
grant select on sys.dba_pending_transactions to BPELSE_USER;
grant select on sys.pending_trans$ to BPELSE_USER;
grant select on sys.dba_2pc_pending to BPELSE_USER;
grant execute on sys.dbms_system to BPELSE_USER;
grant select on SYS.dba_2pc_neighbors to BPELSE_USER;
grant force any transaction to BPELSE_USER;
