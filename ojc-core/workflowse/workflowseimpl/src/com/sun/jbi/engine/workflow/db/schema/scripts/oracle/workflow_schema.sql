CREATE TABLE Task_Instance (       
	Id NUMBER(32) NOT NULL 
     , TaskDef_Id VARCHAR2(150) NOT NULL
     , MessageExchange_Id VARCHAR2(200) NOT NULL
     , Title VARCHAR2(4000)
     , Keyword VARCHAR2(4000)     
     , Status NUMBER(8) NOT NULL
     , Priority NUMBER(8) DEFAULT 2 NOT NULL
     , Owner VARCHAR2(30)
          , AssignedTo VARCHAR(500)
     , CompletedBY VARCHAR2(30)
     , Failed_Code VARCHAR2(20)
     , Failed_Reason VARCHAR2(150)
     , InputData CLOB
     , OutputData CLOB
     , CreateDate DATE
     , EndDate DATE
     , Deadline DATE
     , PRIMARY KEY (Id)
);
CREATE TABLE Task_Timer (
       Id NUMBER(32) NOT NULL 
     , Xpath VARCHAR2(150)       
     , Task_Id NUMBER(32) NOT NULL
     , DueDate DATE NOT NULL
     , Status NUMBER(8) DEFAULT 1 NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Timer_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Assignee (
       Id NUMBER(32) NOT NULL 
     , Task_Id NUMBER(32) NOT NULL
     , Assignee VARCHAR2(30) NOT NULL
     , AssignedStatus NUMBER(8)
     , Active_Assignee NUMBER(1) DEFAULT 1
     , UpdateDate DATE NOT NULL
	 , StartDate DATE NOT NULL  
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Excluded_Assignee (
       Id NUMBER(32) NOT NULL 
     , Task_Id NUMBER(32) NOT NULL
     , Assignee VARCHAR2(30) NOT NULL
     , Active_Assignee NUMBER(1) DEFAULT 1
     , UpdateDate DATE NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Excluded_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE INDEX IND_Assignee ON Task_Assignee (Assignee);
CREATE INDEX IND_Excluded_Assignee ON Task_Excluded_Assignee (Assignee);
CREATE INDEX IND_TaskStatus ON Task_Instance (Status);
CREATE SEQUENCE TASKASSIGNEEID
  START WITH 1
  MINVALUE 1
  NOCYCLE
  NOCACHE
  NOORDER;
  
  CREATE SEQUENCE TASKEXCLUDEDASSIGNEEID
  START WITH 1
  MINVALUE 1
  NOCYCLE
  NOCACHE
  NOORDER;
  
CREATE SEQUENCE TASKINSTANCEID
  START WITH 1
  MINVALUE 1
  NOCYCLE
  NOCACHE
  NOORDER;
CREATE SEQUENCE TASKTIMERID
  START WITH 1
  MINVALUE 1
  NOCYCLE
  NOCACHE
  NOORDER;
  
  