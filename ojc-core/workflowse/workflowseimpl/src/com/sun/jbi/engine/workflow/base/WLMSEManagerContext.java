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
 * @(#)$Id: WLMSEManagerContext.java,v 1.1 2010/02/15 19:25:10 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.DefaultManagerContext;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;

public class WLMSEManagerContext extends DefaultManagerContext {
    
    private WorkflowMapEntryTable mEntryMap;

    public WLMSEManagerContext(ComponentContext ctx, EndpointManager emgr, ExchangeHandler handler, ComponentConfig config, CustomClassLoaderUtil cclu, WorkflowMapEntryTable entryMap) throws JBIException {
        super(ctx, emgr, handler, config, cclu);
        // TODO Auto-generated constructor stub
        mEntryMap = entryMap;
    }
    
    public WorkflowMapEntryTable getEntryTable () {
        return mEntryMap;
    }
    
    public WorkflowMapEntry findEntry(MessageExchange msg) throws JBIException {
        ServiceEndpoint se = msg.getEndpoint();
    
        WorkflowMapEntry entry = mEntryMap.findWorkflowEntry(se);
        if (entry == null) {
            throw new JBIException("Missing WorkflowSE entry for: "+ 
                                   msg.getEndpoint().getEndpointName());
        } else if (!entry.isStarted()) {
            // The containing service unit is not started
            throw new JBIException("WorkflowSE entry not started for: "+ 
                                   msg.getEndpoint().getEndpointName());
        }    
        return entry;
    }    

}
