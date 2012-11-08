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
 * @(#)NoOp.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.sql.*;
import java.util.*;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;

/*
 * NoOp.java
 *
 * Created on August 22, 2005, 3:57 PM
 *
 * @author Bing Lu
 */

public class NoOp implements Operator {

    protected Map opProperties;
    protected String mId;
    protected String mName;
    protected String mOperation;
    protected int mTopoScore;
    protected Timestamp mTimestampToProcess = new Timestamp(0);
    protected Properties mProcessingState = new Properties(); 


    protected void initialize(Map prop) {
        opProperties = prop;
        mId = (String)prop.get(PROP_ID);
        mName = (String)prop.get(PROP_NAME);
        mOperation = NameUtil.makeJavaId(mName);
        mTopoScore = PropertyUtil.getint(prop, PROP_TOPO_SCORE, 0);
    }

    NoOp(Map prop) {
        initialize(prop);
    }

    public String getId() {
        return mId;
    }
    
    public String getName() {
        return mName;
    }
    
    public String getOperation() {
        return mOperation;
    }
    
    public QueryPlan getPlan() {
        return null;
    }
    
    public Schema getOutputSchema() {
        return null;
    }
    
    public String getOutputType() {
        return IO_TYPE_NONE;
    }
    
    public String getQueueName() {
        return null;
    }
    
    public int getTopoScore() {
        return mTopoScore;
    }
    
    public List getInputSchemaIdList() {
        return new ArrayList();
    }

    public List<Operator> getInputOperatorList() {
        return new ArrayList<Operator>();
    }
    
    public List<Operator> getStaticInputOperatorList() {
        return new ArrayList<Operator>();
    }

    public void setRuntimeConnection(Connection con) {
    }
    
    public void unsetRuntimeConnection() {
    }
    
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        return false;
    }
    
    public Properties getProcessingState() {
        return mProcessingState;
    }
    
    public void initProcessingState(Properties processingState) {
        mProcessingState = processingState;
    }
    
    public void operate(Timestamp timestampToProcess) {
        mTimestampToProcess = timestampToProcess;
    }
    
    public void deploy(Connection con) {
    }
    
    public void undeploy(Connection con) {
    }
    
    public void deployExternalResource() {
    }
    
    public void undeployExternalResource() {
    }
    
    public boolean hasReport() {
        return false;
    }
    
    public String getReport(int indentation) {
        return "";
    }
    
    public void resetReport() {
    }

    public boolean equals(Object o) {
        if (o instanceof NoOp) {
            NoOp op = (NoOp)o;
            return op.getId().equals(getId());
        }
        return false;
    }
    
    public int hashCode() {
        return 3*mId.hashCode() + 1;
    }
    
    //===============================
    public boolean isGlobal() {
        return false;
    }
    
    public String getGlobalId() {
        return "";
    }
   
    public boolean getGarbageCollectionEnabled() {
        return true;
    }

    public Map<String, Object> getAdministrableProperties() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setAdministrableProperty(String propName, Object propValue) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public PreparedStatement getPreparedStatmentForDebugResult(Connection con) throws Exception {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setDataAccessEnabled(boolean daEnabled){
        throw new UnsupportedOperationException("Not supported yet.");
    }
    public boolean isDataAccessEnabled(){
        throw new UnsupportedOperationException("Not supported yet.");
    }


    public Map getOperatorProperties() {
        return this.opProperties;
    }

    public void setExtTable(String tabName) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getExtTable() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean needsReset() {
        throw new UnsupportedOperationException("Not supported yet.");
    }



}
