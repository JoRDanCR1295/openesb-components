write nested query

query :

INSERT INTO drivertest_emp VALUES ((SELECT MAX(empid) FROM drivertest_emp) + 1 , 'newemp' , (SELECT MAX(deptid) FROM drivertest_dept))