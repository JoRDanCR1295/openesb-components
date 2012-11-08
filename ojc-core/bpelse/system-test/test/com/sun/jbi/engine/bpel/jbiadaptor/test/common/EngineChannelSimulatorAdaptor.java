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
 * @(#)EngineChannelSimulatorAdaptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import com.sun.jbi.engine.bpel.core.bpel.engine.EPReferenceComposer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;

import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Channel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.EngineDriver;

public class EngineChannelSimulatorAdaptor implements Channel {
	public Object invoke(MessageContainer msgContainer,
            RuntimePartnerLink partnerLink, QName operation, boolean oneWay,
            RBPELProcess process) {
        // TODO Auto-generated method stub
        return null;
    }

    public void reply(MessageContainer msgContainer) {
        // TODO Auto-generated method stub
        
    }

    public void sendFault(QName faultName, MessageContainer msgContainer) {
        // TODO Auto-generated method stub
        
    }

    public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
        Transaction transaction = EngineDriver.lookupTransaction(msgExchangeId);
        if(transaction == null){
            return;
        }
        TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
        try {
            tm.resume(transaction);
            transaction.commit();
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#sendRequestError(java.lang.String, java.lang.Exception)
     */
    public void sendRequestError(String msgExchangeId, Exception error) {
        Transaction transaction = EngineDriver.lookupTransaction(msgExchangeId);
        if(transaction == null){
            return;
        }
        TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
        try {
            tm.resume(transaction);
            transaction.rollback();
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public void sendResponseDoneStatus(String msgExchangeId) {
    }

    public void sendResponseErrorStatus(String msgExchangeId, Exception ex) {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
     */
    public DocumentFragment getExternalEndPoint(PartnerLink partnerLink) {
        return null;
    }

    public void sendKPIMEx(QName interfaceName, String operation, String doc)
            throws MessagingException {
        // no need to implement for Testdriver
    }

    public void installServiceQualities(String suName, String suPath)
            throws DeploymentException {
        // no need to implement for Testdriver
    }

    public void uninstallServiceQualities(String suName) {
        // no need to implement for Testdriver
    }

    public EPReferenceComposer getEPRComposer() {
        // no need to implement for Testdriver
        return null;
    }

    public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
        // no need to implement for Testdriver
        return null;
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment docFrag) {
        // no need to implement for Testdriver
        return null;
    }
}
