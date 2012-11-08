/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TestDBConnImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Timestamp;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection;
import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EngineCorrelationDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EngineDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.UpdateDanglingInstancesDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.WaitingIMADBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.SimpleVariableDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 */
public class TestDBConnImpl extends AbstractDBConnection implements Connection {
    /**
     * DOCUMENT ME!
     * 
     * @author Sun Microsystems
     * @version 
     */
    static class DBOperation {
        /** DOCUMENT ME! */
        static final String INSERT = "INSERT"; //$NON-NLS-1$

        /** DOCUMENT ME! */
        static final String DELETE = "DELETE"; //$NON-NLS-1$

        /** DOCUMENT ME! */
        static final String UPDATE = "UPDATE"; //$NON-NLS-1$
        
        /** DOCUMENT ME! */
        String op;

        /** DOCUMENT ME! */
        DBObject dbobj;

        /**
         * Creates a new DBOperation object.
         * 
         * @param o DOCUMENT ME!
         * @param obj DOCUMENT ME!
         */
        DBOperation(String o, DBObject obj) {
            op = o;
            dbobj = obj;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        String getOP() {
            return op;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        DBObject getObject() {
            return dbobj;
        }
    }

    public static final String COMP_IN_FLOW = "COMP.IN.FLOW"; //$NON-NLS-1$
    public static final String bTrue = "true"; //$NON-NLS-1$
    public static final String bFalse = "false"; //$NON-NLS-1$

    private static String mEngineID;

    private static String mModelID;

    private static boolean mDone;

    private static int mCurrentCrashPoint = 0;

    private static int mCrashPoint;
    
    private static String mTestMode;
    
    private List mDBOperationList;

    /** Even though TestDBConnImpl extends AbstractConnection, we shouldn't be calling super, 
     * instead should be delegating to this "mConn" since the underlying AbstractConnection
     * could be DerbyDBConnImpl or OrclDBConnImpl or some other  */
    AbstractDBConnection mConn;

    public static final int SUCCESSFUL_EXIT_ID = 49000;

    /**
     * Creates a new instance of TestDBConnImpl
     * 
     * @param conn jdbc connection
     * @throws SQLException SQLException
     */
    public TestDBConnImpl(AbstractDBConnection conn) throws SQLException {
        // This super call is a dummy call since there is no other constructor defined.
        super(conn.getUnderlyingConnection());
        mConn = conn;
        mDBOperationList = new ArrayList();
        
        if (mTestMode == null) {
            mCrashPoint = Integer.valueOf(System.getProperty("PERSIST.CRASHPOINT")).intValue(); //$NON-NLS-1$
            mTestMode = System.getProperty("TEST.MODE");
            if (mTestMode.equals("RECOVER")) {
            	mCurrentCrashPoint = mCrashPoint;
            }
        }
    }

    /**
     * insert new row to database
     * 
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public void insert(DBObject obj) throws SQLException {
        if (!(obj instanceof WaitingIMADBO)) {
            mDBOperationList.add(new DBOperation(DBOperation.INSERT, obj));
            mDone = false;

            if ((mEngineID == null) && obj instanceof EngineDBO) {
                mEngineID = ((EngineDBO) obj).getId();
            }
        }

        mConn.insert(obj);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#insert(java.util.List)
     */
    public void insert(List objs) throws SQLException {
        for (int i = 0; i < objs.size(); i++) {
            DBObject obj = (DBObject) objs.get(i);
            if (!(obj instanceof WaitingIMADBO)) {
                mDBOperationList.add(new DBOperation(DBOperation.INSERT, obj));
            }
        }

        mConn.insert(objs);
    }

    /**
     * executes database update
     * 
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public int update(DBObject obj) throws SQLException {
        if (!(obj instanceof UpdateDanglingInstancesDBO)) {
            mDBOperationList.add(new DBOperation(DBOperation.UPDATE, obj));
        }
        return mConn.update(obj);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#update(java.util.List)
     */
    public void update(List objs) throws SQLException {
        for (int i = 0; i < objs.size(); i++) {
            mDBOperationList.add(new DBOperation(DBOperation.UPDATE, (DBObject) objs.get(i)));
        }

        mConn.update(objs);
    }

    /**
     * @see DBConnection#delete(com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject)
     */
    public void delete(DBObject obj) throws SQLException {
        mDBOperationList.add(new DBOperation(DBOperation.DELETE, obj));
        mConn.delete(obj);

        if (obj instanceof StateDBO) {
            mDone = true;
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#delete(java.util.List)
     */
    public void delete(List objs) throws SQLException {
        for (int i = 0; i < objs.size(); i++) {
            mDBOperationList.add(new DBOperation(DBOperation.DELETE, (DBObject) objs.get(i)));
        }

        mConn.delete(objs);
    }
    
    
    private boolean isValidCrashpoint(){
        for (int i = 0; i < mDBOperationList.size(); i++) {

            if (((DBOperation) mDBOperationList.get(i)).getObject() instanceof VariableDBO
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof LastCheckPointDBO
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof ScopeDBO
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof ForEachDBO            		
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof StateDBO            		
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof PartnerLinkDBO
            		|| ((DBOperation) mDBOperationList.get(i)).getObject() instanceof InstanceCorrelationDBO) {
                return true;
            }        	
        }
        
        return false;
    }
    
    /**
     * ends transaction
     * 
     * @throws SQLException SQLException
     */
    private void endTx() throws SQLException {
    	
    	if (!isValidCrashpoint()) {
    		return;
    	}
    	
        if (mTestMode.equals("CRASH") && mCurrentCrashPoint == mCrashPoint) {
            crash();
        }
    	
        try {
            String path = System.getProperty("PERSIST.OUTPUT"); //$NON-NLS-1$
            File output = new File(path);
            FileOutputStream fos = new FileOutputStream(output, true);
            StringBuffer fileOutput = new StringBuffer();
            
            if(mCurrentCrashPoint > 0) {
                fileOutput.append("CRASHPOINT : " + mCurrentCrashPoint + "\n");
            } else {
                fileOutput.append("CRASHPOINT : Not a valid crashpoint since no data has been persisted in the database at this point.\n");
            }
            
            mCurrentCrashPoint++;
            
            for (int i = 0; i < mDBOperationList.size(); i++) {

                DBOperation dbo = (DBOperation) mDBOperationList.get(i);
                DBObject dbobj = dbo.getObject();
                
                if (dbobj instanceof CRMPDBO) {
                	continue;
                }
                
                StringBuffer buff = new StringBuffer();
                if (dbobj instanceof StateDBO && ((StateDBO)dbobj).getBPELId() == null) {
                    // TODO 
                    // BPEL ID is null for update of state dbo for owner lock during activation
                    // and passivation. Need to study the existing test cases and change 
                    // the output files.
                    continue;
                }
                
               	buff.append(dbo.getOP()).append(": ");
                
                if (dbobj instanceof EngineCorrelationDBO) {
                    EngineCorrelationDBO ecDBO = (EngineCorrelationDBO) dbobj;
                    //String bpelId = getLocationFreePath(ecDBO.getBpelId());
                    //QName bpelId = getLocationFreePath(QName.valueOf(ecDBO.getBpelId()));
                    buff.append(ecDBO.getClass().getSimpleName()).append("[ID: ").append(
                            ecDBO.getId()).append("][BPEL ID: ").append(ecDBO.getBpelId().toString())
                    /* .append("][ENGINE ID: ").append(ecDBO.getEngineId()) */
                    .append("][ENGINE ID: ").append("DYNAMIC_ENGINE_ID").append("][VALUE: ").append(
                            ecDBO.getValue()).append("]");
                } else if (dbobj instanceof EngineDBO) {
                    /*
                     * buff.append(dbobj.getClass().getSimpleName()) .append("[ID:
                     * ").append(((EngineDBO) dbobj).getId()) .append("][LOCATION: ")
                     * .append(((EngineDBO) dbobj).getLocation()).append("]");
                     */
                    continue;
                } else if (dbobj instanceof InstanceCorrelationDBO) {
                    InstanceCorrelationDBO icDBO = (InstanceCorrelationDBO) dbobj;
                    //String corrVal = getLocationFreePathForCorrVal(icDBO.getValue());
                    buff.append(icDBO.getClass().getSimpleName()).append("[ID: ").append(
                            icDBO.getId()).append("][VALUE: ").append(icDBO.getValue()).append("]");
                } else if (dbobj instanceof LastCheckPointDBO) {
                    LastCheckPointDBO lcpDBO = (LastCheckPointDBO) dbobj;
                    String timerVal = (lcpDBO.getTimerValue() == null) ? null : "SOME_VAL";
                    long branchInvokeCounter = lcpDBO.getBranchInvokeCounter();
                    
                    if (branchInvokeCounter == LastCheckPointDBO.DEFAULT_BRANCH_INVOKE_COUNTER) {
                        // Branch Counter value is obtained from the DB and not the DBO object.
                        //ResultSet rs = getRow(dbobj);
                        //rs.next();
                        //branchInvokeCounter = rs.getLong(7);
                    	List list = getRow(dbobj);
                    	if (!list.isEmpty()) {
                    		LastCheckPointDBO lcpdbo = (LastCheckPointDBO) list.get(0);
                    		branchInvokeCounter = lcpdbo.getBranchInvokeCounter();
                    	}
                    }
                    
                    buff.append(lcpDBO.getClass().getSimpleName()).append("[ACTIVITY ID: ").append(
                            lcpDBO.getActivityId()).append("][OLD ACTIVITY ID: ").append(
                            lcpDBO.getOldActivityId()).append("][TIMER VALUE: ").append(timerVal)
                            .append("][PICK COMPOSITE ACT ID: ").append(
                            lcpDBO.getPickCompositeActId()).append("][BRANCH INVOKE COUNTER: ").append(
                            branchInvokeCounter).append("]");
                } else if (dbobj instanceof StateDBO) {
                    StateDBO stDBO = (StateDBO) dbobj;
                    // During the instance (state) activation and passivation the bpelid is
                    // not passed to the statedbo object.
                    if (stDBO.getBPELId() != null) {
                        //String bpelId = getLocationFreePath(stDBO.getBPELId());
                        //QName bpelId = getLocationFreePath(stDBO.getBPELId());

                        //buff.append(dbobj.getClass().getSimpleName()).append("[BPEL ID: ").append(bpelId)
                        buff.append(dbobj.getClass().getSimpleName()).append("[BPEL ID: ").append(stDBO.getBPELId().toString())
                        /* .append("][ENGINE ID: ").append(stDBO.getEngineId()) */
                        .append("][ENGINE ID: ").append("DYNAMIC_ENGINE_ID").append("]");
                    } else {
                        // TODO not checking for the update of owner lock of the state
                        // as the current driver test cases need to be looked into carefully first
                        /*String dbobjOwnerlock = stDBO.getMode() == State.PASSIVATE_MODE ? StateDBO.OWNERLOCK_NO
                                : StateDBO.OWNERLOCK_YES;
                        buff.append(dbobj.getClass().getSimpleName()).append("[OWNER LOCK: ").append(
                                dbobjOwnerlock)
                                 .append("][ENGINE ID: ").append(stDBO.getEngineId()) 
                                .append("][ENGINE ID: ").append("DYNAMIC_ENGINE_ID").append("]");*/
                    }
                } else if (dbobj instanceof VariableDBO) {
                    buff.append(dbobj.getClass().getSimpleName()).append("[ID: ").append(
                            ((VariableDBO) dbobj).getId())
                            .append("]");
                    if (dbobj instanceof SimpleVariableDBOImpl) { 
                        if (((VariableDBO) dbobj).getValue() instanceof java.util.Date) {
                            buff.append("[Value: ").append("some date value]");
                        } else {
                        	Object value = ((VariableDBO) dbobj).getValue();
                        	
                        	if(value instanceof Number) {
                        		NumberFormat nf = NumberFormat.getInstance();
                        		nf.setMinimumFractionDigits(5);
                        		value = nf.format(value);
                        	}
                        	
                            buff.append("[Value: ").append(value).append("]");
                        }
                    }
                } else if (dbobj instanceof ForEachDBO) {
                    ForEachDBO fedbo = (ForEachDBO) dbobj;
                    buff.append(fedbo.getClass().getSimpleName()).append("[FOREACH ID: ").append(
                            fedbo.getForEachId()).append("][INDEX: ").append(fedbo.getCounter()).append(
                            "][SUCCESSES: ").append(fedbo.getSuccesses()).append("]");
                } 
                /*CR #6514814 optimization of the CRMP persistence for DB performance
                 * has led to CRMP persistence point entries not being deterministic
                 * and this leads to the junit test framework comparision of static 
                 * '.out' file failing.
                 * TODO: need to revisit the junit test framework for optimal solution
                 */ 
                if (dbobj instanceof CRMPDBO) {
                	/*CRMPDBO cdbo = (CRMPDBO) dbobj;
                    buff.append(cdbo.getClass().getSimpleName()).append("][PARTNERLINK: ").append(
                            cdbo.getPartnerLink()).append("][OPERATION: ").append(
                            cdbo.getOperation()).append("][REPLYVARIABLEID: ").append(
                            cdbo.getReplyVariableId()).append("]"); */
                	continue;
                } 
                else if (dbobj instanceof WaitingIMADBO) {
                    WaitingIMADBO waitingIMADbo = (WaitingIMADBO) dbobj;
                    buff.append(waitingIMADbo.getClass().getSimpleName()).append("][PARTNERLINK: ").append(
                            waitingIMADbo.getPartnerLinkName()).append("][OPERATION: ").append(
                            waitingIMADbo.getOperationName()).append("]");
                } else if (dbobj instanceof EventHandlerDBO) {
                    EventHandlerDBO eveDBO = (EventHandlerDBO) dbobj;
                    buff.append(eveDBO.getClass().getSimpleName()).append("[Event Model ID: ")
                    .append(eveDBO.getEventModelId()).append("][Update Flag: ")
                    .append(eveDBO.isUpdated());

                    if (eveDBO.getScopeGuid() != null) {
                    	buff.append("][Scope ID: GUID");
                    }

                    if (!eveDBO.isUpdated() && eveDBO.getTimerValue() != null) {
                        buff.append("][Timer Value: ").append("SOME_VAL")
                        .append("][Repeat Every: ");
                        String repeatEveryVal;
                        if (eveDBO.getRepeatEveryVal() > 0) {
                            repeatEveryVal = "SOME_VAL";
                        } else {
                            repeatEveryVal = String.valueOf(eveDBO.getRepeatEveryVal());
                        }
                        buff.append(repeatEveryVal);
                    }
                    
                    buff.append("]");
                    
                } else if (dbobj instanceof PartnerLinkDBO) {
                    buff.append(dbobj.getClass().getSimpleName()).append("[ID: ").append(
                            ((PartnerLinkDBO) dbobj).getId()).append("]");
                } else if (dbobj instanceof ScopeDBO) {
                	ScopeDBO scopeDBO = (ScopeDBO) dbobj;
                	String isInFlowStr = System.getProperty(COMP_IN_FLOW);
                	boolean isInFlow = false;
                	if (isInFlowStr != null) {
                		isInFlow = isInFlowStr.equals(bTrue);
                	}
                	buff.append(scopeDBO.getClass().getSimpleName())
                		.append("[Scope ID: ").append(scopeDBO.getScopeId())
                		.append("][ScopeState: ").append(scopeDBO.getScopeState());
                	if (!isInFlow) {
                		buff.append("][CompletionOrder: ").append(scopeDBO.getCompletionOrder());
                	}
                	buff.append("][FaultName: ").append(scopeDBO.getFaultName())
                		.append("][FaultActivity ID: ").append(scopeDBO.getFaultActivityId())
                		.append("][Compensate Id: ").append(scopeDBO.getCompensateId())
                		.append("]");
                }

                buff.append("\n");
                String out = buff.toString();
                if (mEngineID != null) {
                    out = out.replaceAll(mEngineID, "ENGINE-ID"); //$NON-NLS-1$
                }

                if (mModelID != null) {
                    out = out.replaceAll(mModelID, "MODEL-ID"); //$NON-NLS-1$
                }

                if (dbo.getOP().equals(DBOperation.INSERT)) {
                    if (!validateValues(getRow(dbobj), dbobj)) {
                        fail("INSERT validation failed"); //$NON-NLS-1$
                    }
                } else if (dbo.getOP().equals(DBOperation.DELETE)) {
                    if (!validateDelete(getRow(dbobj), dbobj)) {
                        fail("DELETE validation failed"); //$NON-NLS-1$
                    }
                } else if (dbo.getOP().equals(DBOperation.UPDATE)) {
                    // exempt CRMPDO from the update check as the Query parameter is
                    // null for the update DBO object...
                    if (!(dbobj instanceof CRMPDBO) && !validateValues(getRow(dbobj), dbobj)) {
                        fail("UPDATE validation failed"); //$NON-NLS-1$
                    }
                }

                // fos.write(out.getBytes());
                fileOutput.append(out);
            }

            // append delimiter
            fileOutput.append("======================================================================\n");
            // write to file AFTER we've not crashed
            fos.write(fileOutput.toString().getBytes());
            fos.flush();
            fos.close();

            if (mDone) {
                // mCounter = 0;
                mModelID = null;
            }
        } catch (FileNotFoundException ex) {
            fail("can not create execution output"); //$NON-NLS-1$
        } catch (IOException ex) {
            fail("failed to write to execution output"); //$NON-NLS-1$
        }

        mDBOperationList.clear();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#get(java.lang.String,
     *      java.util.List)
     */
    public ResultSet get(String query, List vals) throws SQLException {
        return mConn.get(query, vals);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#getRow(com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject)
     */
    public List getRow(DBObject obj) throws SQLException {
        return mConn.getRow(obj);
    }

    static boolean compBlobs(Blob blob1, Blob blob2) {
        if ((blob1 == null) && (blob2 != null)) {
            return false;
        }

        if ((blob1 != null) && (blob2 == null)) {
            return false;
        }

        if ((blob1 == null) && (blob2 == null)) {
            return true;
        }

        try {
            InputStream is1 = blob1.getBinaryStream();
            InputStream is2 = blob2.getBinaryStream();

            if (is1.available() != is2.available()) {
                return false;
            }

            byte[] buf1 = new byte[is1.available()];
            byte[] buf2 = new byte[is2.available()];

            for (int i = 0; i < is1.available(); i++) {
                if (buf1[i] != buf2[i]) {
                    return false;
                }
            }
        } catch (SQLException ex) {
            return false;
        } catch (IOException ex) {
            return false;
        }

        return true;
    }

    /**
     * compares two object instances
     * 
     * @param obj1 Object
     * @param obj2 Object
     * @return boolean true: if two objects are equal false; if two objects are not equal
     */
    static boolean compObject(Object obj1, Object obj2) {
        if (obj1 == null) {
            return ((obj2 != null) ? false : true);
        }

        return obj1.equals(obj2);
    }

    static void crash() {
        System.exit(SUCCESSFUL_EXIT_ID);
    }

    static void fail(String msg) {
        throw new RuntimeException(msg);
    }

    //static String getLocationFreePath(String bpelId) {
    static QName getLocationFreePath(QName bpelId) {
        // this is to avoid hard test data file dependency on the location path
        // of the deployed BPEL file.
        //TODO 
        //bpelId = bpelId.substring(bpelId.indexOf("bpelse"));
        //return bpelId;
        return null;
    }

    static String getLocationFreePathForCorrVal(String corrVal) {
        // this is to avoid hard test data file dependency on the location path
        // of the deployed BPEL file.
        int index = corrVal.indexOf("bpelse");
        String prefix = corrVal.substring(0, index);
        prefix = prefix.substring(0, prefix.lastIndexOf('{') + 1);
        String suffix = corrVal.substring(index);
        return (prefix + suffix);
    }

    static boolean validateDelete(List list, DBObject dbobj) throws SQLException {
        if (dbobj instanceof EngineCorrelationDBO) {
            return !validateEngineCorrelationValues(list, (EngineCorrelationDBO) dbobj);
        } else if (dbobj instanceof EngineDBO) {
            return !validateEngineValues(list, (EngineDBO) dbobj);
        } else if (dbobj instanceof InstanceCorrelationDBO) {
            return !validateInstanceCorrelationValues(list, (InstanceCorrelationDBO) dbobj);
        } else if (dbobj instanceof LastCheckPointDBO) {
            return !validateLastCheckPointValues(list, (LastCheckPointDBO) dbobj);
        } else if (dbobj instanceof StateDBO) {
            return validateStateDelete(list, (StateDBO) dbobj);
        } else if (dbobj instanceof VariableDBO) {
            return !validateVariableValues(list, (VariableDBO) dbobj);
        } else if (dbobj instanceof CRMPDBO) {
            return true; // TODO: think if this is needed...
        } else if (dbobj instanceof ScopeDBO) {
        	//return validateScopeDBOValues(list, (ScopeDBO)dbobj);
        	return true;
        }

        return false;
    }

    static boolean validateEngineCorrelationValues(List list, EngineCorrelationDBO dbobj)
            throws SQLException {
    	Iterator iter = list.iterator();
    	while (iter.hasNext()) {
    		EngineCorrelationDBO dbo = (EngineCorrelationDBO) iter.next();
            long id = dbo.getId();
            String bpelid = dbo.getBpelId().toString();
            String engid = dbo.getEngineId();
            String value = dbo.getValue();

            if (dbobj.getId() != id) {
                continue;
            }

            if (!compObject(bpelid, dbobj.getBpelId().toString())) {
                continue;
            }

            if (!compObject(engid, dbobj.getEngineId())) {
                continue;
            }

            if (!compObject(value, dbobj.getValue())) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateEngineValues(List list, EngineDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while (iter.hasNext()) {
    		EngineDBO dbo = (EngineDBO) iter.next();
    		String id = dbo.getId();
            String loc = dbo.getLocation();

            if (!compObject(id, dbobj.getId())) {
                continue;
            }

            if (!compObject(loc, dbobj.getLocation())) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateInstanceCorrelationValues(List list, InstanceCorrelationDBO dbobj)
            throws SQLException {
    	Iterator iter = list.iterator();
    	while (iter.hasNext()){
    		InstanceCorrelationDBO dbo = (InstanceCorrelationDBO) iter.next();
            String bpid = dbo.getBPId();
            long id = dbo.getId();
            String value = dbo.getValue();

            if (id != dbobj.getId()) {
                continue;
            }

            if (!compObject(bpid, dbobj.getBPId())) {
                continue;
            }

            if (!compObject(value, dbobj.getValue())) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateLastCheckPointValues(List list, LastCheckPointDBO dbobj)
            throws SQLException {
    	Iterator iter = list.iterator();
    	while (iter.hasNext()) {
    		LastCheckPointDBO dbo = (LastCheckPointDBO) iter.next();
            String bpid = dbo.getBPId();
            long actid = dbo.getActivityId();


            if (!compObject(bpid, dbobj.getBPId())) {
                continue;
            }

            if (actid != dbobj.getActivityId()) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateStateDelete(List list, StateDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		StateDBO dbo = (StateDBO) iter.next();
    		String id = dbo.getId();
            String bpelid = dbo.getBPELId().toString();
            String engid = dbo.getEngineId();
            String status = dbo.getStatus();

            if (!compObject(id, dbobj.getId())) {
                continue;
            }

            if (!compObject(bpelid, dbobj.getBPELId().toString())) {
                continue;
            }

            /*
             * if (!compObject(engid, dbobj.getEngineId())) { continue; }
             */

            if (!status.equals(dbobj.COMPLETE_STATUS)) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateStateValues(List list, StateDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		StateDBO dbo = (StateDBO) iter.next();
    		String id = dbo.getId();
            String bpelid = dbo.getBPELId().toString();
            String engid = dbo.getEngineId();

            if (!compObject(id, dbobj.getId())) {
                continue;
            }

            if (!compObject(bpelid, dbobj.getBPELId().toString())) {
                continue;
            }

            if (!compObject(engid, dbobj.getEngineId())) {
                continue;
            }

            return true;
        }

        return false;
    }

    static boolean validateStateValues2(List list, StateDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		StateDBO dbo = (StateDBO) iter.next();
    		String id = dbo.getId();
            String ownerlock = dbo.getOwnerLock();
            String dbobjOwnerlock = dbobj.getMode() == State.Mode.PASSIVATE ? StateDBO.OWNERLOCK_NO
                    : StateDBO.OWNERLOCK_YES;

            if (!compObject(id, dbobj.getId().toString())) {
                continue;
            }

            if (!compObject(ownerlock, dbobjOwnerlock)) {
                continue;
            }

            return true;
        }
        return false;
    }

    static boolean validateValues(List list, DBObject dbobj) throws SQLException {
        if (dbobj instanceof EngineCorrelationDBO) {
            return validateEngineCorrelationValues(list, (EngineCorrelationDBO) dbobj);
        } else if (dbobj instanceof EngineDBO) {
            return validateEngineValues(list, (EngineDBO) dbobj);
        } else if (dbobj instanceof InstanceCorrelationDBO) {
            return validateInstanceCorrelationValues(list, (InstanceCorrelationDBO) dbobj);
        } else if (dbobj instanceof LastCheckPointDBO) {
            return validateLastCheckPointValues(list, (LastCheckPointDBO) dbobj);
        } else if (dbobj instanceof StateDBO) {
            if (((StateDBO) dbobj).getBPELId() == null) {
                return validateStateValues2(list, (StateDBO) dbobj);
            } else {
                return validateStateValues(list, (StateDBO) dbobj);
            }
        } else if (dbobj instanceof VariableDBO) {
            return validateVariableValues(list, (VariableDBO) dbobj);
        } else if (dbobj instanceof ForEachDBO) {
            return validateForEachValues(list, (ForEachDBO) dbobj);
        } else if (dbobj instanceof CRMPDBO) {
            return validateCRMPValues(list, (CRMPDBO) dbobj);
        } else if (dbobj instanceof WaitingIMADBO) {
            return validateWaitingIMADBOValues(list, (WaitingIMADBO) dbobj);
        } else if (dbobj instanceof EventHandlerDBO) {
            return validateEventhdlrDBOValues(list, (EventHandlerDBO) dbobj);
        } else if (dbobj instanceof PartnerLinkDBO) {
            return validatePartnerLinkDBOValues(list, (PartnerLinkDBO) dbobj);
        } else if (dbobj instanceof ScopeDBO) {
        	return validateScopeDBOValues(list, (ScopeDBO) dbobj);
        }

        return false;
    }
    
    static boolean validateScopeDBOValues(List list, ScopeDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while (iter.hasNext()) {
    		ScopeDBO dbo = (ScopeDBO) iter.next();
    		String bpid = dbo.getBPId();
    		String scopeid = dbo.getScopeGuid();
    		
    		if (!compObject(bpid, dbobj.getBPId()))
    			continue;
    		if (!compObject(scopeid, dbobj.getScopeGuid()))
    			continue;
    		
    		return true;
    	}
    	return false;
    }

    static boolean validateVariableValues(List list, VariableDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		VariableDBO dbo = (VariableDBO) iter.next();
            String bpid = dbo.getBPId();
            String scopeid = dbo.getScopeGuid();
            long id = dbo.getId();

            if (!compObject(bpid, dbobj.getBPId()))
                continue;
            if(!compObject(scopeid, dbobj.getScopeGuid()))
            	continue;
            if (id != dbobj.getId())
                continue;

            return true;
        }

        return false;
    }

    static boolean validateForEachValues(List list, ForEachDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		ForEachDBO dbo = (ForEachDBO) iter.next();
            long id = dbo.getForEachId();
            String bpid = dbo.getBPId();

            if (!compObject(bpid, dbobj.getBPId()))
                continue;
            if (id != dbobj.getForEachId())
                continue;

            return true;
        }

        return false;
    }

    static boolean validateWaitingIMADBOValues(List list, WaitingIMADBO dbobj)
            throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		WaitingIMADBO dbo = (WaitingIMADBO) iter.next();
    		String id = dbo.getId();
            String mPartnerLinkName = dbo.getPartnerLinkName();
            String mOperationName = dbo.getOperationName();

            if (!compObject(id, dbobj.getId()))
                continue;
            if (!compObject(mPartnerLinkName, dbobj.getPartnerLinkName()))
                continue;
            if (!compObject(mOperationName, dbobj.getOperationName()))
                continue;

            return true;
        }

        return false;
    }

    static boolean validateCRMPValues(List list, CRMPDBO dbobj) throws SQLException {
    	Iterator iter = list.iterator();
    	while(iter.hasNext()) {
    		CRMPDBO dbo = (CRMPDBO) iter.next();
            String bpid = dbo.getBPId();
            String pl = dbo.getPartnerLink();
            String oper = dbo.getOperation();
            long replyvariableid = dbo.getReplyVariableId();

            if (!compObject(bpid, dbobj.getBPId()))
                continue;
            if (replyvariableid != dbobj.getReplyVariableId())
                continue;
            if (!compObject(pl, dbobj.getPartnerLink()))
                continue;
            if (!compObject(oper, dbobj.getOperation()))
                continue;

            return true;
        }

        return false;
    }

    static boolean validateEventhdlrDBOValues(List list, EventHandlerDBO dbobj) throws SQLException {
        Iterator iter = list.iterator();
        while(iter.hasNext()) {
            EventHandlerDBO dbo = (EventHandlerDBO) iter.next();
            String bpid = dbo.getBPId();
            String eventId = dbo.getId();
            String scopeid = dbo.getScopeGuid();
            long eventModelId = dbo.getEventModelId();
            Timestamp ts = dbo.getTimerValue();
            boolean update = dbo.isUpdated();
            long repeatVal = dbo.getRepeatEveryVal();

            if (!compObject(bpid, dbobj.getBPId())) {
                continue;
            }
            if (!compObject(eventId, dbobj.getId())) {
                continue;
            }
            if (!compObject(scopeid, dbobj.getScopeGuid())) {
                continue;
            }
            if (!compObject(eventModelId, dbobj.getEventModelId())) {
                continue;
            }
            // DEVNOTE: MySQL has a bug which causes it to lose millisecond information. Using date format to get 
            // around that bug.
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            Object object1 = (ts != null) ? sdf.format(ts) : null;
            Object object2 = (dbobj.getTimerValue() != null) ? sdf.format(dbobj.getTimerValue()) : null;
            if (!update && !compObject(object1, object2)) {
                continue;
            }
            if (!(update == dbobj.isUpdated())) {
                continue;
            }
            if (!update && !compObject(repeatVal, dbobj.getRepeatEveryVal())) {
                continue;
            }            
            return true;
        }

        return false;
    }
    
    
    static boolean validatePartnerLinkDBOValues(List list, PartnerLinkDBO dbobj) throws SQLException {
        Iterator iter = list.iterator();
        while(iter.hasNext()) {
            PartnerLinkDBO dbo = (PartnerLinkDBO) iter.next();
            String bpid = dbo.getBPId();
            long id = dbo.getId();

            if (!compObject(bpid, dbobj.getBPId()))
                continue;
            if (id != dbobj.getId())
                continue;

            return true;
        }
        return false;
    }
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.connection.impl.AbstractDBConnectionImpl#getUnderlyingConnection()
     */
    public Connection getUnderlyingConnection() {
        return this;
    }
    
    /**
     * NOTE: post change for the testing framework to support two connection pools.
     * This is overridden here for the specific case of when the underlyind connection is a non-XA connection.
     * endTx() should be called as this method will write out to the persistence .out file the persisitence points,
     * and this has to be synchronized with the commit. If the crashpoint is met then the test JVM would be killed
     * and the commit would not happen to the database, thereby ensuring synchronization between the entries in the 
     * .out file and the entries in the database.
     * DB operations using the XA connection should not use this method call as the commit/rollback should always be 
     * called on the Transaction object and not the underlying connection.
     */
    public void commit() throws SQLException {
    	endTx();
        mConn.getUnderlyingConnection().commit();
    }
    

    public void clearWarnings() throws SQLException {
        mConn.getUnderlyingConnection().clearWarnings();
    }

     //The following method is there in AbstractConnection and using it from there
    public void close() throws SQLException {
        // let the underlying DummyXA or NonXA Connection objects handle this. 
        // XA will not close the underlying connection till the transaction is 
        // committed, while nonXA will close the underlying connection immediately.
        mConn.close();
    }

    public Statement createStatement() throws SQLException {
        return mConn.getUnderlyingConnection().createStatement();
    }

    public Statement createStatement(int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        return mConn.getUnderlyingConnection().createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    public Statement createStatement(int resultSetType, int resultSetConcurrency) throws SQLException {
        return mConn.getUnderlyingConnection().createStatement(resultSetType, resultSetConcurrency);
    }

    public boolean getAutoCommit() throws SQLException {
        return mConn.getUnderlyingConnection().getAutoCommit();
    }

    public String getCatalog() throws SQLException {
        return mConn.getUnderlyingConnection().getCatalog();
    }

    public int getHoldability() throws SQLException {
        return mConn.getUnderlyingConnection().getHoldability();
    }

    public DatabaseMetaData getMetaData() throws SQLException {
        return mConn.getUnderlyingConnection().getMetaData();
    }

    public int getTransactionIsolation() throws SQLException {
        return mConn.getUnderlyingConnection().getTransactionIsolation();
    }

    public Map<String, Class<?>> getTypeMap() throws SQLException {
        return mConn.getUnderlyingConnection().getTypeMap();
    }

    public SQLWarning getWarnings() throws SQLException {
        return mConn.getUnderlyingConnection().getWarnings();
    }

    public boolean isClosed() throws SQLException {
        return mConn.getUnderlyingConnection().isClosed();
    }

    public boolean isReadOnly() throws SQLException {
        return mConn.getUnderlyingConnection().isReadOnly();
    }

    public String nativeSQL(String sql) throws SQLException {
        return mConn.getUnderlyingConnection().nativeSQL(sql);
    }

    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        return mConn.getUnderlyingConnection().prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        return mConn.getUnderlyingConnection().prepareCall(sql, resultSetType, resultSetConcurrency);
    }

    public CallableStatement prepareCall(String sql) throws SQLException {
        return mConn.getUnderlyingConnection().prepareCall(sql);
    }

    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql, resultSetType, resultSetConcurrency);
    }

    public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql, autoGeneratedKeys);
    }

    public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql, columnIndexes);
    }

    public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql, columnNames);
    }

    public PreparedStatement prepareStatement(String sql) throws SQLException {
        return mConn.getUnderlyingConnection().prepareStatement(sql);
    }

    public void releaseSavepoint(Savepoint savepoint) throws SQLException {
        mConn.getUnderlyingConnection().releaseSavepoint(savepoint);
    }

    public void rollback() throws SQLException {
        mConn.getUnderlyingConnection().rollback();
    }

    public void rollback(Savepoint savepoint) throws SQLException {
        mConn.getUnderlyingConnection().rollback(savepoint);
    }

    public void setAutoCommit(boolean autoCommit) throws SQLException {
        mConn.getUnderlyingConnection().setAutoCommit(autoCommit);
    }

    public void setCatalog(String catalog) throws SQLException {
        mConn.getUnderlyingConnection().setCatalog(catalog);
    }

    public void setHoldability(int holdability) throws SQLException {
        mConn.getUnderlyingConnection().setHoldability(holdability);
    }

    public void setReadOnly(boolean readOnly) throws SQLException {
        mConn.getUnderlyingConnection().setReadOnly(readOnly);
    }

    public Savepoint setSavepoint() throws SQLException {
        return mConn.getUnderlyingConnection().setSavepoint();
    }

    public Savepoint setSavepoint(String name) throws SQLException {
        return mConn.getUnderlyingConnection().setSavepoint(name);
    }

    public void setTransactionIsolation(int level) throws SQLException {
        mConn.getUnderlyingConnection().setTransactionIsolation(level);
    }

    public void setTypeMap(Map<String, Class<?>> map) throws SQLException {
        mConn.getUnderlyingConnection().setTypeMap(map);
    }
   
}
