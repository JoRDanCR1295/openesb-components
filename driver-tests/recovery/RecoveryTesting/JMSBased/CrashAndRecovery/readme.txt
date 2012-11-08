If you are using Oracle:
-Run java_service1_oracle.sql to create the required tablespace, the schema JAVA_SERVICE1_USER and the tables.
-Create a JDBC connection pool with any name (for example JavaService1Pool). Use the username and password as JAVA_SERVICE1_USER. Ping to make sure that the configurations are correct.
-Create a JDBC Resource named "jdbc/JavaService1Pool" using the connection pool created above. 

If you are using Derby:
-Start the Derby database from the location where you are going to run the test from. IMP NOTE: If the Derby database is up, stop it and start it from this location.
For example,
>cd C:\open-esb\open-jbi-components\driver-tests\recovery\RecoveryTesting\recoverydriver\win
>call scripts/StartDatabaseServer.bat

-Create a JDBC connection pool with any name (for example JavaService1Pool). Use the username and password as JAVA_SERVICE1_USER. Use any database name and prefix ";create=true". For example, "JavaService1DB;create=true". Ping to make sure that the configurations are correct. This will also create the database "JavaService1DB" if it is not already there.
-Create a JDBC Resource named "jdbc/JavaService1Pool" using the connection pool created above. 
-If you are using Derby run java_service1_derby.sql the create required tables.
 