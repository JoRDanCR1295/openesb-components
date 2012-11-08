select empid, empname from employee where exists (select * from department where employee.deptid=department.deptid)
