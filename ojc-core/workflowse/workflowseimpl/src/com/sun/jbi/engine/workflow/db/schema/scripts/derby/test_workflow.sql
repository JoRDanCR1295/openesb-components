
DROP TABLE Workflow.Task_Assignee;
DROP TABLE Workflow.Task_Timer;
DROP TABLE Workflow.Task_Change_Log;
DROP TABLE Workflow.Task_Instance;
DROP TABLE Workflow.Task_Status;
DROP TABLE Workflow.Assign_Status;
DROP TABLE Workflow.Task_Priority;

DROP SCHEMA Workflow RESTRICT;
CREATE SCHEMA Workflow;
set schema Workflow;


CREATE TABLE Workflow.Task_Priority (
       Id CHAR(10) NOT NULL
     , Description VARCHAR(20) NOT NULL
     , PRIMARY KEY (Id)
);

CREATE TABLE Workflow.Assign_Status (
       Id CHAR(10) NOT NULL
     , Description VARCHAR(20) NOT NULL
     , PRIMARY KEY (Id)
);

CREATE TABLE Workflow.Task_Status (
       Id CHAR(10) NOT NULL
     , Description VARCHAR(20) NOT NULL
     , PRIMARY KEY (Id)
);

CREATE TABLE Workflow.Task_Instance (
       Id CHAR(10) NOT NULL
     , TaskDef_Id VARCHAR(50) NOT NULL
     , MessageExchange_Id VARCHAR(20) NOT NULL
     , Title VARCHAR(80)
     , Status_Id CHAR(10) NOT NULL
     , Priority_Id CHAR(10) DEFAULT '3' NOT NULL
     , Owner VARCHAR(30)
     , Failed_Code VARCHAR(20)
     , Failed_Reason VARCHAR(50)
     , InputData CLOB
     , OutputData CLOB
     , CreateDate TIMESTAMP
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Instance_1 FOREIGN KEY (Status_Id)
                  REFERENCES Workflow.Task_Status (Id)
     , CONSTRAINT FK_Task_Instance_2 FOREIGN KEY (Priority_Id)
                  REFERENCES Workflow.Task_Priority (Id)
);

CREATE TABLE Workflow.Task_Change_Log (
       Task_Id CHAR(10) NOT NULL
     , EventDate TIMESTAMP NOT NULL
     , EventDetails VARCHAR(50) NOT NULL
     , Initiator VARCHAR(30) NOT NULL
     , PRIMARY KEY (Task_Id, EventDate)
     , CONSTRAINT FK_Task_Change_Log_1 FOREIGN KEY (Task_Id)
                  REFERENCES Workflow.Task_Instance (Id)
);

CREATE TABLE Workflow.Task_Timer (
       Id CHAR(10) NOT NULL
     , Task_Id CHAR(10) NOT NULL
     , DueDate TIMESTAMP NOT NULL
     , Status INT NOT NULL
     , PRIMARY KEY (Id)
     , CONSTRAINT FK_Task_Timer_1 FOREIGN KEY (Task_Id)
                  REFERENCES Workflow.Task_Instance (Id)
);

CREATE TABLE Workflow.Task_Assignee (
       id CHAR(10) NOT NULL
     , Task_Id CHAR(10) NOT NULL
     , Assignee_User VARCHAR(30)
     , Assignee_Role VARCHAR(30)
     , AssignedStatus_Id CHAR(10)
     , UpdateDate TIMESTAMP NOT NULL
     , PRIMARY KEY (id)
     , CONSTRAINT FK_Task_Assignee_1 FOREIGN KEY (AssignedStatus_Id)
                  REFERENCES Workflow.Assign_Status (Id)
);

