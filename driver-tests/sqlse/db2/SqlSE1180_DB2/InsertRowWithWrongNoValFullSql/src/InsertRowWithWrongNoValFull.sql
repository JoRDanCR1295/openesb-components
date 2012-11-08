INSERT INTO db2Admin.drivertest_emp VALUES ( (SELECT MAX(empid) FROM db2Admin.drivertest_emp) + 1, ? , ?)
