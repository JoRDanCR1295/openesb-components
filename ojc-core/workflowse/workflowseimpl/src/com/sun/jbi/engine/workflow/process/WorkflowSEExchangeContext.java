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
 * @(#)WorkflowSEExchangeContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.impl.DefaultExchangeContext;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;

public class WorkflowSEExchangeContext extends DefaultExchangeContext {
    private WorkflowMapEntryTable mTable = null;

    /**
     * Constructs an WorkflowSE {@link ExchangeContext}.
     * @param pool
     * @param lctx
     * @param table
     */
    public WorkflowSEExchangeContext(ListenerContext lctx, WorkflowMapEntryTable table) {
        super(null, lctx);
        mTable = table;
    }
    
    public WorkflowMapEntry findEntry(MessageExchange msg) throws JBIException {
        ServiceEndpoint se = msg.getEndpoint();
    
        WorkflowMapEntry entry = mTable.findWorkflowEntry(se);
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
