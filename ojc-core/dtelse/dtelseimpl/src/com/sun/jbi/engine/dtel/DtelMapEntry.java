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
 * @(#)DtelMapEntry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.Output;

import com.sun.jbi.eManager.provider.EndpointStatus;

public class DtelMapEntry {
    public static final String REQUEST_REPLY_SERVICE = "requestReplyService";
    public static final String FILTER_ONE_WAY = "filterOneWay";
    public static final String FILTER_REQUEST_REPLY = "filterRequestReply";
    
    private String mServiceUnitName;
    private String mType;
    private QName mPartnerLink;
    private QName mPortType;
    private QName mOperation;
    private QName mService;
    private String mFile;
    private DTELEngine mDTELEngine;
    private String mEndpointStatusId;
    private EndpointStatus mEndpointStatus;
    
    private QName mOutPartnerLink;
    private QName mOutPortType;
    private QName mOutOperation;
    private QName mOutService;
    private String mReplyFile;
    private Transformer mReplyTransformer;
    private String mOutEndpointStatusId;
    private EndpointStatus mOutEndpointStatus;

    private boolean mStarted;
    private ServiceEndpoint mServiceEndpoint;

    private Definition mWsdl;
    private Output mOutput;

    private static final Logger mLogger = Logger.getLogger(DtelMapEntry.class.getName());
    
    /**
     * Creates a new instance of PortMapEntry
     */
    private DtelMapEntry(String serviceUnitName,
                         String type, 
                         QName partnerLink, 
                         QName portType, 
                         QName operation, 
                         String file, 
                         QName outPartnerLink, 
                         QName outPortType, 
                         QName outOperation, 
                         String replyFile,
                         Definition wsdl)
    {
        mServiceUnitName = serviceUnitName;
        mType = type;
        mPartnerLink = partnerLink;
        mPortType = portType;
        mOperation = operation;
        mFile = file;
        mDTELEngine = new DTELEngine();
        mDTELEngine.loadDecisionTable(mFile);
        
        mOutPartnerLink = outPartnerLink;
        mOutPortType = outPortType;
        mOutOperation = outOperation;
        mReplyFile = replyFile;
        mWsdl= wsdl;
        if (wsdl != null) {
            try {
                PortType pType = wsdl.getPortType(portType);
                Operation outop = pType.getOperation(operation.toString(), null, null);
                mOutput = outop.getOutput();
            } catch (Exception ex) {
                // failed to get out messages...
                mOutput = null;
            }
        }
    }
    
    public static DtelMapEntry newRequestReplyService(String serviceUnitName,
                                                      QName partnerLink, 
                                                      QName portType, 
                                                      QName operation, 
                                                      String file,
                                                      Definition wsdl)
    {
        return new DtelMapEntry(serviceUnitName,
                                REQUEST_REPLY_SERVICE, 
                                partnerLink, 
                                portType, 
                                operation, 
                                file, 
                                null, null, null, null, wsdl);
    }
    
    public static DtelMapEntry newFilterOneWay(String serviceUnitName,
                                               QName partnerLink, 
                                               QName portType, 
                                               QName operation, 
                                               String file,
                                               QName outPartnerLink, 
                                               QName outPortType, 
                                               QName outOperation,
                                               Definition wsdl)
    {
        return new DtelMapEntry(serviceUnitName,
                                FILTER_ONE_WAY, 
                                partnerLink, 
                                portType, 
                                operation, 
                                file, 
                                outPartnerLink, 
                                outPortType, 
                                outOperation, 
                                null, wsdl);
    }
    
    public static DtelMapEntry newFilterRequestReply(String serviceUnitName,
                                                     QName partnerLink, 
                                                     QName portType, 
                                                     QName operation, 
                                                     String file,
                                                     QName outPartnerLink, 
                                                     QName outPortType, 
                                                     QName outOperation,
                                                     String replyFile,
                                                     Definition wsdl)
    {
        return new DtelMapEntry(serviceUnitName,
                                FILTER_REQUEST_REPLY, 
                                partnerLink, 
                                portType, 
                                operation, 
                                file, 
                                outPartnerLink, 
                                outPortType, 
                                outOperation, 
                                replyFile, wsdl);
    }
    
    public String getServiceUnitName() {
        return mServiceUnitName;
    }
    
    public String getType() {
        assert mType != null;
        return mType;
    }
    
    public QName getPartnerLink() {
        assert mPartnerLink != null;
        return mPartnerLink;
    }
    
    public QName getPortType() {
        return mPortType;
    }
    
    public QName getOperation() {
        return mOperation;
    }
    
    public void setService(QName service) {
        assert service != null;
        assert mService == null; // can only set once
        mService = service;
    }
    
    public QName getService() {
        return mService;
    }

    public String getFile() {
        assert mFile != null;
        return mFile;
    }
    
    public DTELEngine getDTELEngine() {
        return mDTELEngine;
    }
    
    public QName getOutPartnerLink() {
        return mOutPartnerLink;
    }
    
    public QName getOutPortType() {
        return mOutPortType;
    }
    
    public QName getOutOperation() {
        return mOutOperation;
    }
    
    public void setOutService(QName outService) {
        assert outService != null;
        assert mOutService == null; // can only set once
        mOutService = outService;
    }
    
    public QName getOutService() {
        return mOutService;
    }

    public boolean hasReplyFile() {
        return mReplyFile != null;
    }
    
    public String getReplyFile() {
        assert mReplyFile != null;
        return mFile;
    }

    public boolean hasReplyTransformer() {
        return mReplyTransformer != null;
    }
    
    public Transformer getReplyTransformer() {
        return mReplyTransformer;
    }

    public void setStarted(boolean started) {
        mStarted = started;
    }
    
    public boolean isStarted() {
        return mStarted;
    }
    
    public void setServiceEndpoint(ServiceEndpoint endpointRef) {
        mServiceEndpoint = endpointRef;
    }
    
    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }
        
    public void setEndpointStatus(String endpointStatusId, EndpointStatus endpointStatus) {
        mEndpointStatusId = endpointStatusId;
        mEndpointStatus = endpointStatus;
    }
    public String getEndpointStatusId() {
        return mEndpointStatusId;
    }
    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    public void setOutEndpointStatus(String outEndpointStatusId, EndpointStatus outEndpointStatus) {
        mOutEndpointStatusId = outEndpointStatusId;
        mOutEndpointStatus = outEndpointStatus;
    }
    public String getOutEndpointStatusId() {
        return mOutEndpointStatusId;
    }
    public EndpointStatus getOutEndpointStatus() {
        return mOutEndpointStatus;
    }

    public Definition getWsdl() {
        return mWsdl;
    }

    public void setWsdl(Definition wsdl) {
        this.mWsdl = wsdl;
    }

    public Output getOutput() {
        return mOutput;
    }

    public void setOutput(Output output) {
        this.mOutput = output;
    }}
