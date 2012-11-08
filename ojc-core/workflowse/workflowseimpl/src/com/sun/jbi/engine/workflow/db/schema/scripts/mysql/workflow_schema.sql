CREATE TABLE Task_Instance (
       Id BIGINT NOT NULL AUTO_INCREMENT
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
     , InputData TEXT
     , OutputData TEXT
     , CreateDate DATETIME
     , EndDate DATETIME
     , Deadline DATETIME
     , PRIMARY KEY (Id)
);
CREATE TABLE Task_Timer (
       Id BIGINT  NOT NULL AUTO_INCREMENT
     , Xpath VARCHAR(150)  
     , Task_Id BIGINT NOT NULL
     , DueDate DATETIME NOT NULL
     , Status INTEGER DEFAULT 1 NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Timer_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Assignee (
       Id BIGINT  NOT NULL AUTO_INCREMENT
     , Task_Id BIGINT  NOT NULL
     , Assignee VARCHAR(30) NOT NULL
     , AssignedStatus INTEGER
     , Active_Assignee NUMERIC(1) DEFAULT 1
     , UpdateDate DATETIME NOT NULL    
     , StartDate DATETIME NOT NULL     
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE TABLE Task_Excluded_Assignee (
       Id BIGINT  NOT NULL AUTO_INCREMENT
     , Task_Id BIGINT  NOT NULL
     , Assignee VARCHAR(30) NOT NULL
     , Active_Assignee NUMERIC(1) DEFAULT 1 
     , UpdateDate DATETIME NOT NULL  
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Excluded_Assignee_1 FOREIGN KEY (Task_Id)
                  REFERENCES Task_Instance (Id)
);
CREATE INDEX IND_Assignee USING HASH ON Task_Assignee (Assignee);
CREATE INDEX IND_Excluded_Assignee USING HASH ON Task_Excluded_Assignee (Assignee);
CREATE INDEX IND_TaskStatus ON Task_Instance (Status);
