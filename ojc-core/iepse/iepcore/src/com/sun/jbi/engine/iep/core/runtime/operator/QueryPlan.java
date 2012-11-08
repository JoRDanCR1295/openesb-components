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
 * @(#)QueryPlan.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.util.List;
import java.util.Properties;
import java.io.Serializable;

/*
 * QueryPlan.java
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 */
import java.sql.Connection;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStream;

public interface QueryPlan extends OperatorConstants, Serializable {
    public String getId();
    
    public String getInstanceId();
    
    public String getName();
    
    public List<Operator> getOperatorList();
    
    public List<InvokeStream> getInvokeStreamOperatorList();
    
    public List<Notifier> getNotifierList();
    
    public List<ExternalTablePollingStream> getExternalTablePollingStreamList();
    
    public List<ReplayStream> getReplayStreamList();
    
    public void addOperator(Operator operator);
    
    public void removeOperator(Operator operator);
    
    public Operator getOperatorById(String id);
    
    public Operator getOperatorByName(String name);
    
    // operation = NameUtil.makeJavaId(operator.getName())
    public Operator getOperatorByOperation(String operation);
    
    public Schema getSchema(String id);
    
    public boolean hasSchema(String id);
    
    public void deploy(Connection con) throws Exception;
    
    public void undeploy(Connection con) throws Exception;

    /**
     * Some operators connect to external resources 
     * ex SaveStream, External Polling Stream
     * we need to clean up (close connection etc) on those resource
     * 
     * @throws Exception
     */
    public void undeployExternalResource() throws Exception;
    
    
    /**
     * Some operators connect to external resources 
     * ex SaveStream, External Polling Stream
     * we need to create table etc on those resource
     * @throws Exception
     */
    public void deployExternalResource() throws Exception;
    
    public Properties getConfigProperties();
    
    public void changeAndPersistOperatorProperty(Connection con , Operator opr,
            String propName, String propValue) throws Exception ;

    public List<String> getDependencyIdList();
}
    
