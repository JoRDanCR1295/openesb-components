CREATE TABLE drivertest_emp (empid int not null primary key, ename varchar(20), deptid integer 
REFERENCES drivertest_dept(deptid))