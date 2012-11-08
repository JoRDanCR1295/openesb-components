CREATE TABLE db2Admin.drivertest_emp (empid integer not null primary key, ename varchar(20), deptid 
integer REFERENCES drivertest_dept(deptid))