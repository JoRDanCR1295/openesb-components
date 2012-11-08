package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import java.sql.Connection;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;

/**
 * InvokeStream.java
 *
 * 
 *
 * @author Ritesh Adval
 * 
 */
public class InvokeStream extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(InvokeStream.class);

    private List<String> mStaticInputIdList;
    private List<String> mFromColumnList;
    private List<String> mToColumnList; // exp -> asName
    private String mFromClause;
    private String mWhereClause;
    private String mExternalIEPProcessQualifiedName;
    private String mExternalOperatorName;
    private Properties mConfigProp = null;
    
    public InvokeStream(Map prop) {
        initialize(prop);
        mStaticInputIdList = getStrList((String)prop.get(PROP_STATIC_INPUT_ID_LIST));
        mFromColumnList = getStrList((String)prop.get(PROP_FROM_COLUMN_LIST));
        mToColumnList = getStrList((String)prop.get(PROP_TO_COLUMN_LIST));
        mFromClause = " " + (String)prop.get(PROP_FROM_CLAUSE) + " ";
        mWhereClause = (String)prop.get(PROP_WHERE_CLAUSE);
        mExternalIEPProcessQualifiedName = (String) prop.get(PROP_EXTERNAL_IEP_PROCESS_QUALIFIED_NAME);
        mExternalOperatorName = (String) prop.get(PROP_EXTERNAL_OPERATOR_NAME);
        
        mConfigProp = (Properties)prop.get(PROP_CONFIG_PROPERTIES);
    }
    
    public String getOutputType() {
        return IO_TYPE_NONE;
    }

    @Override
    protected void createCleanOutputStmt(Connection con) throws Exception {
        // TODO Auto-generated method stub
//        super.createCleanOutputStmt(con);
    }
    
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        // TODO Auto-generated method stub
//        super.createOutputQueue(con);
    }
    
    
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        // TODO Auto-generated method stub
//        super.dropOutputQueue(con);
    }
    
    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        
        mOperateStmt = mDbSpecial.getInvokeStreamDb().createOperateStatements(con, this);
    }
    
    public List<String> getStaticInputIdList() {
        return mStaticInputIdList;
    }
    
    public List<String> getFromColumnList() {
        return mFromColumnList;
    }

    public List<String> getToColumnList() {
        return mToColumnList;
    }
    
    public String getFromClause() {
        return mFromClause;
    }
    
    public String getWhereClause() {
        return mWhereClause;
    }
    
    public String getExternalIEPProcessQualifiedName() {
        return mExternalIEPProcessQualifiedName;
    }
    
    public String getExternalOperatorName() {
        return mExternalOperatorName;
    }
    
    
    public String findExternalOperatorTableName(Connection con) throws Exception {
        String tableName = null;
        
        
        String externalIEPProcessQualifiedName = getExternalIEPProcessQualifiedName();
        QueryPlan externalIEPProcessPlan = Util.getPlanByInstanceId(con, mConfigProp, externalIEPProcessQualifiedName);
        if(externalIEPProcessPlan != null) {
            //found external process, now find operator
            String externalOperatorName = getExternalOperatorName();
            
            if(externalOperatorName != null) {
                Operator operator = externalIEPProcessPlan.getOperatorByName(externalOperatorName);
                if(operator != null) {
                    String operatorTableName = operator.getQueueName();
                    tableName = operatorTableName;
                } else {
                    throw new Exception(mMessages.getString("InvokeStream.External_IEP_Process_Does_Not_Have_Operator", new Object[]{externalIEPProcessQualifiedName, externalOperatorName}));
                }
            }
        } else {
            //if external iep process is not deployed throw exception
            throw new Exception(mMessages.getString("InvokeStream.External_IEP_Process_Not_Deployed", new Object[]{externalIEPProcessQualifiedName}));
        }
        
        
        return tableName;
    }
}
