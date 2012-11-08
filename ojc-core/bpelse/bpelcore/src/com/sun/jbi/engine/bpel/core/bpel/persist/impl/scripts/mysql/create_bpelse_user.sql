-- Note that all the names used in this file are prefixed with BPELSE_USER. If you 
-- want to create a user with different name and want that user to have separate 
-- tablespace, etc, simply find and replace BPELSE_USER with the desired user name.


-- You need to login to MySql as root to run these commands. 

-- Create a database named BPELSE_USER_DB. Change this value if a different name 
-- is desired.  


CREATE DATABASE BPELSE_USER_DB;

-- Create a new user BPELSE_USER. Change the name if so desired. Password will be same
-- as the user name by default. This username and password will be used to create the 
-- connection pool on the application server. Note that if you used a different 
-- tablespace name above, you will have to specify that tablespace name here.
GRANT ALL ON BPELSE_USER_DB.* TO 'BPELSE_USER'@'%' IDENTIFIED BY 'BPELSE_USER';
