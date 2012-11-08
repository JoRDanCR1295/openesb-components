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
 * @(#)SQLMapEntry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;


/**
 * Used by SQLSE.
 * Data structure to hold an entry in the sqlmap file.
 */
public final class SQLMapEntry {
    public static final String REQUEST_REPLY_SERVICE = "requestReplyService";
    public static final String FILTER_ONE_WAY = "filterOneWay";
    public static final String FILTER_REQUEST_REPLY = "filterRequestReply";
    private static final Messages mMessages = Messages.getMessages(SQLMapEntry.class);
    private static final Logger mLogger = Messages.getLogger(SQLMapEntry.class);
    private String mServiceUnitName;
    private String mType;
    private QName mPartnerLink;
    private QName mPortType;
    private QName mOperation;
    private QName mService;
    private String mSQLFileName;
    private String mWSDLFileName;
    private String mEndpointStatusId;
    private EndpointStatus mEndpointStatus;
    private QName mOutPartnerLink;
    private QName mOutPortType;
    private QName mOutOperation;
    private QName mOutService;
    private String mReplyFile;
    private String mOutEndpointStatusId;
    private EndpointStatus mOutEndpointStatus;
    private boolean mStarted;
    private ServiceEndpoint mServiceEndpoint;
    private Definition mWsdl;
    private Output mOutput;

    /**
     * Creates a new instance of SQLMapEntry
     */
    private SQLMapEntry(final String serviceUnitName, final String type, final QName partnerLink,
        final QName portType, final QName operation, final String sqlfileName,
        final String wsdlFileName, final QName outPartnerLink, final QName outPortType,
        final QName outOperation, final String replyFile, final Definition wsdl) {
        mServiceUnitName = serviceUnitName;
        mType = type;
        mPartnerLink = partnerLink;
        mPortType = portType;
        mOperation = operation;
        mSQLFileName = sqlfileName;
        mWSDLFileName = wsdlFileName;

        mOutPartnerLink = outPartnerLink;
        mOutPortType = outPortType;
        mOutOperation = outOperation;
        mReplyFile = replyFile;
        mWsdl = wsdl;
        String operationName = null;
        if (wsdl != null) {
            try {
                final PortType pType = wsdl.getPortType(portType);
                String opName = operation.toString();
                if(opName.lastIndexOf(".sql")>0){
                	StringBuffer sb = new StringBuffer(operation.toString());
                	operationName = sb.replace(opName.lastIndexOf(".sql"),opName.length(),"").toString().trim();
                }else
                	operationName = operation.toString();
                final Operation outop = pType.getOperation(operationName,
                        null, null);
                mOutput = outop.getOutput();
            } catch (final Exception ex) {
                mLogger.log(Level.SEVERE,"Could not parse the SQLMap",ex);
                // failed to get out messages...
                mOutput = null;
            }
        }
    }

    static SQLMapEntry newRequestReplyService(final String serviceUnitName,
        final QName partnerLink, final QName portType, final QName operation, final String sqlfileName,
        final String wsdlFileName, final Definition wsdl) {
        return new SQLMapEntry(serviceUnitName, SQLMapEntry.REQUEST_REPLY_SERVICE,
            partnerLink, portType, operation, sqlfileName, wsdlFileName, null,
            null, null, null, wsdl);
    }

    static SQLMapEntry newFilterOneWay(final String serviceUnitName,
        final QName partnerLink, final QName portType, final QName operation, final String sqlFileName,
        final String wsdlFileName, final QName outPartnerLink, final QName outPortType,
        final QName outOperation, final Definition wsdl) {
        return new SQLMapEntry(serviceUnitName, SQLMapEntry.FILTER_ONE_WAY, partnerLink,
            portType, operation, sqlFileName, wsdlFileName, outPartnerLink,
            outPortType, outOperation, null, wsdl);
    }

    static SQLMapEntry newFilterRequestReply(final String serviceUnitName,
        final QName partnerLink, final QName portType, final QName operation, final String sqlFileName,
        final String wsdlFileName, final QName outPartnerLink, final QName outPortType,
        final QName outOperation, final String replyFile, final Definition wsdl) {
        return new SQLMapEntry(serviceUnitName, SQLMapEntry.FILTER_REQUEST_REPLY,
            partnerLink, portType, operation, sqlFileName, wsdlFileName,
            outPartnerLink, outPortType, outOperation, replyFile, wsdl);
    }

    protected String getServiceUnitName() {
        return mServiceUnitName;
    }

    private String getType() {
        assert mType != null;

        return mType;
    }

    QName getPartnerLink() {
        assert mPartnerLink != null;

        return mPartnerLink;
    }

    QName getPortType() {
        return mPortType;
    }

    QName getOperation() {
        return mOperation;
    }

    protected void setService(final QName service) {
        assert service != null;
        assert mService == null; // can only set once
        mService = service;
    }

    QName getService() {
        return mService;
    }

    QName getOutPartnerLink() {
        return mOutPartnerLink;
    }

    QName getOutPortType() {
        return mOutPortType;
    }

    QName getOutOperation() {
        return mOutOperation;
    }

    protected void setOutService(final QName outService) {
        assert outService != null;
        assert mOutService == null; // can only set once
        mOutService = outService;
    }

    private QName getOutService() {
        return mOutService;
    }

    private boolean hasReplyFile() {
        return mReplyFile != null;
    }

    private String getReplyFile() {
        assert mReplyFile != null;

        return mReplyFile;
    }

    protected void setStarted(final boolean started) {
        mStarted = started;
    }

    private boolean isStarted() {
        return mStarted;
    }

    protected void setServiceEndpoint(final ServiceEndpoint endpointRef) {
        mServiceEndpoint = endpointRef;
    }

    ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }

    protected void setEndpointStatus(final String endpointStatusId,
        final EndpointStatus endpointStatus) {
        mEndpointStatusId = endpointStatusId;
        mEndpointStatus = endpointStatus;
    }

    String getEndpointStatusId() {
        return mEndpointStatusId;
    }

    private EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    protected void setOutEndpointStatus(final String outEndpointStatusId,
        final EndpointStatus outEndpointStatus) {
        mOutEndpointStatusId = outEndpointStatusId;
        mOutEndpointStatus = outEndpointStatus;
    }

    String getOutEndpointStatusId() {
        return mOutEndpointStatusId;
    }

    private EndpointStatus getOutEndpointStatus() {
        return mOutEndpointStatus;
    }

    Definition getWsdl() {
        return mWsdl;
    }

    private void setWsdl(final Definition wsdl) {
        mWsdl = wsdl;
    }

    private Output getOutput() {
        return mOutput;
    }

    private void setOutput(final Output output) {
        mOutput = output;
    }

    String getSQLFileName() {
        return mSQLFileName;
    }

    private void setSQLFileName(final String mSQLFileName) {
        this.mSQLFileName = mSQLFileName;
    }

    String getWSDLFileName() {
        return mWSDLFileName;
    }

    private void setWSDLFileName(final String mWSDLFileName) {
        this.mWSDLFileName = mWSDLFileName;
    }
}
