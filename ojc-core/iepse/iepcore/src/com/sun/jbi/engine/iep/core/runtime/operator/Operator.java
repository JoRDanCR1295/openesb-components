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
 * @(#)Operator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/*
 * Operator.java
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 * @author IEP Team
 */
public interface Operator extends OperatorConstants {
    
    public Map getOperatorProperties();
    
    public String getId();
    
    public String getName();
    
    // return NameUtil.makeJavaId(getName());
    public String getOperation();
    
    public QueryPlan getPlan();
    
    public Schema getOutputSchema();
    
    public String getOutputType();
    
    public String getQueueName();
    
    public int getTopoScore();
    
    public List<Operator> getInputOperatorList();

    public List<Operator> getStaticInputOperatorList();
    
    public void setRuntimeConnection(Connection con);
    
    public void unsetRuntimeConnection();
    
    public boolean hasWorkToDo(Timestamp timestampToProcess);
    
    public Properties getProcessingState();
    
    public void initProcessingState(Properties processingState);
    
    public void operate(Timestamp timestampToProcess);
    
    public void deploy(Connection con) throws Exception;
    
    public void undeploy(Connection con);
    
    /**
     * Some operators connect to external resources 
     * ex SaveStream, External Polling Stream
     * we need to clean up (close connection etc) on those resource
     * 
     *
     */
    public void undeployExternalResource();
    
    /**
     * Some operators connect to external resources 
     * ex SaveStream, External Polling Stream
     * we need to create tables etc on those resource
     * 
     *
     */
    public void deployExternalResource() throws Exception;
    
    
    public boolean hasReport();
    
    public void resetReport();
    
    public String getReport(int indentation);
    
    //========================================
    public boolean isGlobal();
    
    public String getGlobalId();
    
    public boolean getGarbageCollectionEnabled();
    
    //Util method for runtime administration
    public Map<String,Object> getAdministrableProperties() ;
    public void setAdministrableProperty(String propName,Object propValue);
    public PreparedStatement getPreparedStatmentForDebugResult(Connection con) throws Exception ;
    public void setDataAccessEnabled(boolean daEnabled);
    public boolean isDataAccessEnabled();
    public void setExtTable(String tabName) ;
    public String getExtTable() ;
    public boolean needsReset() ;
}
