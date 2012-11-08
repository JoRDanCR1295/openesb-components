CREATE TABLE ai_load_tracker_EAP
    (load_id                        VARCHAR2(30),
    batch_id                       NUMBER,
    interface_type                 VARCHAR2(10),
    file_creation_date             DATE,
    source_info                    VARCHAR2(50),
    destination_info               VARCHAR2(50),
    source_status                  VARCHAR2(10),
    destination_status             VARCHAR2(10),
    quantity                       VARCHAR2(1000),
    interface_status               VARCHAR2(50),
    bp_status                      VARCHAR2(250));


CREATE TABLE ai_load_tracker_Update (
    Load_ID  VARCHAR2(30),
    Batch_ID number,
    poll_Time TIMESTAMP,
    BP_Status VARCHAR2(50),
    primary key (Load_ID, Batch_ID,poll_Time)
);
