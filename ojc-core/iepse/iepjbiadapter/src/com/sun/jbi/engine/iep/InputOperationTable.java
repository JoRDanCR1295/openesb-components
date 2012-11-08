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
 * @(#)InputOperationTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.IEPSEInOnlyThread.FaultCode;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.internationalization.Messages;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;

/**
 *
 * @author Bing Lu
 */
public class InputOperationTable {
    private static final Messages mMessages = Messages.getMessages(InputOperationTable.class);
    
    private ExtendedComponentContext mExtendedContext;
    private HashMap<String, InputOperation> mInputOpTable = new HashMap<String, InputOperation>();

    private InputOperation getInputOperation(QueryPlan plan, String operation) {
        InputOperation opr = null;
        String key = plan.getInstanceId() + "~" + operation;
        opr = (InputOperation) mInputOpTable.get(key);
        if (opr == null) {
            opr = new InputOperation(mExtendedContext, plan, operation);
            mInputOpTable.put(key, opr);
        }
        return opr;
    }
    
    private List<InputOperation> getInputOperations(String instanceId) {
        LinkedList<InputOperation> ret = new LinkedList<InputOperation>();
        Set<String> keys = mInputOpTable.keySet();
        String keyStart = instanceId + "~";
        for (String key : keys) {
            if (key.startsWith(keyStart)) {
                ret.add(mInputOpTable.get(key));
            }
        }
        return ret;
    }
    
    private void sendError(String instanceId, FaultCode code, Exception e) {
        List<InputOperation> opList = getInputOperations(instanceId);
        for (InputOperation op : opList) {
            op.sendError(code, e);
        }
    }
    
    private boolean contains(String instanceId) {
        Set<String> keys = mInputOpTable.keySet();
        String keyStart = instanceId + "~";
        for (String key : keys) {
            if (key.startsWith(keyStart)) {
                return true;
            }
        }
        return false;
    }
    
    public InputOperationTable(ExtendedComponentContext extendedContext) {
        mExtendedContext = extendedContext;
    }
    
    public void process(InOnly inOnly) {
        String instanceId = inOnly.getEndpoint().getServiceName().getNamespaceURI();
        String operation = inOnly.getOperation().getLocalPart();
        DeploymentRecord dr = mExtendedContext.getDeploymentTable().getRecordByDeployName(instanceId);
        if (dr == null) {
            String msg = mMessages.getString("InputOperationTable.Deployment_cannot_be_found", instanceId);                
            Exception e = new ServerException(msg);
            if (contains(instanceId)) {
                sendError(instanceId, FaultCode.Server, e);
            }
            DeliveryChannel channel = mExtendedContext.getDeliveryChannel(); 
            String actor = mExtendedContext.getComponentContext().getComponentName();
            InputOperation.sendError(channel, inOnly, actor, FaultCode.Server, e);
            return;
        }
        if (!dr.isStarted()) {
            String msg = mMessages.getString("InputOperationTable.Deployment_is_not_started", dr.getDeployName());
            Exception e = new ServerException(msg);
            if (contains(instanceId)) {
                sendError(instanceId, FaultCode.Server, e);
            }
            DeliveryChannel channel = mExtendedContext.getDeliveryChannel(); 
            String actor = mExtendedContext.getComponentContext().getComponentName();
            InputOperation.sendError(channel, inOnly, actor, FaultCode.Server, e);
        }
        QueryPlan plan = dr.getPlan();
        InputOperation inOp = getInputOperation(plan, operation);
        inOp.process(inOnly);
    }
    
    public void batchProcess() {
        Iterator<InputOperation> itr = mInputOpTable.values().iterator();
        while (itr.hasNext()) {
            InputOperation op = itr.next();
            op.batchProcess();
        }
    }
    
    public void clear() {
        mInputOpTable.clear();
    }
}
