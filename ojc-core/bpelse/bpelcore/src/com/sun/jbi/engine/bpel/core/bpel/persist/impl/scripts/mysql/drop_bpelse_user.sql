-- Drop the user that was created earlier. Note that if you chose a different name for the 
-- user while creating the user, you will have to specify that name here.
DROP USER 'BPELSE_USER'@'%';

-- Drop the database that was created earlier. Note that if you chose a different name for 
-- the database while creating the user, you will have to specify that name here.
DROP DATABASE BPELSE_USER_DB;

-- Manually delete the datafiles that were created. If you used the defaults while creating
-- the datafiles, the delete the directory 'bpelse_user_db' under the data directory of the server.