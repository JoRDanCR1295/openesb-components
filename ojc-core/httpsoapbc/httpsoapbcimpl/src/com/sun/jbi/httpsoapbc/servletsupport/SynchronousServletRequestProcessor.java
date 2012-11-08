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
 * @(#)SynchronousServletRequestProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.servletsupport;

import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import com.sun.jbi.httpsoapbc.InboundMessageProcessor;
import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.SynchronousReplySupport;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 *
 */
public class SynchronousServletRequestProcessor {

    private static final Messages mMessages =
        Messages.getMessages(SynchronousServletRequestProcessor.class);
    private final static Logger mLogger =
        Messages.getLogger(SynchronousServletRequestProcessor.class);
    
    final static String INIT_TARGET_DEPLOYMENT_ID = "TargetDeploymentId";    
    
    Normalizer mNormalizer;
    Denormalizer mDenormalizer;
    SynchronousReplySupport mReplyListener;
    HttpSoapBindingLifeCycle lifeCycle;
    InboundMessageProcessor mInboundProcessor;
  
    /** 
     * Creates a new instance of SynchronousServletRequestProcessor 
     * @param deploymentComponentId the deployment id of the "target" HTTP SOAP binding component,
     */
    public SynchronousServletRequestProcessor(HttpSoapBindingLifeCycle lifeCycle) throws MessagingException {
        this.lifeCycle = lifeCycle;
        initialize();
        
    }

    /** 
     * Initialize the request processor with the servlet specific configuration
     * @param deploymentComponentId the deployment id of the "target" HTTP SOAP binding component,
     * i.e. the BC deployment to send the requests to.
     */
    void initialize() throws MessagingException {
        mNormalizer = new HttpServletNormalizer();
        mDenormalizer = new HttpServletDenormalizer();
        mReplyListener = new SynchronousReplySupport();

        //mInboundProcessor = new InboundMessageProcessor(mDeploymentComponentId, mNormalizer, mReplyListener, lifeCycle);        
        mInboundProcessor = new InboundMessageProcessor(mNormalizer, mReplyListener);
    }
    
    /** 
     * Can process requests for both HTTP <code>GET</code> (debugging purposes)
     * and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    public void processRequest(HttpServletRequest request, HttpServletResponse response) {
        // TODO: should the content type be SOAP 1.2 with application/soap+xml ?
        response.setContentType("text/xml");
               
        //String context = request.getPathInfo();
        String context = request.getContextPath() + request.getPathInfo();
        int port = request.getServerPort();
	Endpoint targetEndpoint = lifeCycle.getEndpointBeanForContext(context, port);
        OperationMetaData operationMetaData = null;

	if (targetEndpoint == null) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, "HTTPBC-W00600.No_webservice_mapping_for_url",
                        new Object[] {context, new Integer(port)});
            }
            // TODO: reply with fault.
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Web service mapping found for the requested URL, context " + context + " at port " + port);
            }
            mInboundProcessor.setInput(request);
            mInboundProcessor.setTargetEndpoint(targetEndpoint);
        
            try {        
                mInboundProcessor.execute(null);
        
                // TODO: this only makes sense for InOut 
                MessageExchange exchange = mReplyListener.waitForReply();
        
                NormalizedMessage outMsg = null;
  
                if (exchange instanceof InOut) {
                    InOut inout = (InOut) exchange;
                    outMsg = inout.getOutMessage();
                }
                
                Map nameToMeta = targetEndpoint.getOperationNameToMetaData();
                operationMetaData = (OperationMetaData) nameToMeta.get(exchange.getOperation());
                if (operationMetaData == null) {
                    throw new MessagingException(mMessages.getString("HTTPBC-E00667.No_opmeta_for_operation",
                                                                    exchange.getOperation()));
                }
    
                response = (HttpServletResponse) mDenormalizer.denormalize(outMsg, exchange, response, operationMetaData);
        
            } catch (MessagingException ex) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, "HTTPBC-WOO605.Exception_during_request_processing", ex);
                }
                // TODO: reply with fault.
            }
        }
    }
}
