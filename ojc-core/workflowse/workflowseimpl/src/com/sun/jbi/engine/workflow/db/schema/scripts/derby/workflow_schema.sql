CREATE TABLE  Task_Instance (
       Id BIGINT NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1) 
     , TaskDef_Id VARCHAR(150) NOT NULL
     , MessageExchange_Id VARCHAR(200) NOT NULL
     , Title VARCHAR(5000)
     , Keyword VARCHAR(5000)
     , Status INTEGER NOT NULL
     , Priority INTEGER DEFAULT 2 NOT NULL
     , Owner VARCHAR(30)     
     , AssignedTo VARCHAR(500)
     , CompletedBY VARCHAR(30)
     , Failed_Code VARCHAR(20)
     , Failed_Reason VARCHAR(150)
     , InputData CLOB
     , OutputData CLOB
     , CreateDate TIMESTAMP     
      , EndDate TIMESTAMP
      , Deadline TIMESTAMP
     , PRIMARY KEY (Id)
);
CREATE TABLE Task_Timer (
       Id BIGINT  NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1)
     , Xpath VARCHAR(150)       
     , Task_Id BIGINT NOT NULL
     , DueDate TIMESTAMP NOT NULL
     , Status INTEGER DEFAULT 1 NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Timer_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Assignee (
       Id BIGINT  NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1)
     , Task_Id BIGINT  NOT NULL
     , Assignee VARCHAR(30) NOT NULL
--     , AssigneeType VARCHAR(10)
     , AssignedStatus INTEGER
     , Active_Assignee NUMERIC(1) DEFAULT 1     
--     , Excluded_Assignee NUMERIC(1) DEFAULT 0
     , UpdateDate TIMESTAMP NOT NULL     
     , StartDate TIMESTAMP NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Excluded_Assignee (
       Id BIGINT  NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1)
     , Task_Id BIGINT  NOT NULL
     , Assignee VARCHAR(30) NOT NULL
     , Active_Assignee NUMERIC(1) DEFAULT 1     
     , UpdateDate TIMESTAMP NOT NULL     
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Excluded_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE INDEX IND_Assignee ON Task_Assignee (Assignee);
CREATE INDEX IND_Excluded_Assignee ON Task_Excluded_Assignee (Assignee);
CREATE INDEX IND_TaskStatus ON Task_Instance (Status);
