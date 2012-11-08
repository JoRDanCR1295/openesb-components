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
 * @(#)ChannelManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.msmq;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.msmqbc.Endpoint;
import com.sun.jbi.msmqbc.EndpointImpl;
import com.sun.jbi.msmqbc.Endpoint.EndpointType;
import com.sun.jbi.msmqbc.extensions.MSMQOperation;
import com.sun.jbi.msmqbc.util.MSMQUtil;
import com.sun.jbi.msmqbc.exception.MSMQException;
import com.sun.jbi.msmqbc.exception.ChannelManagerException;

/**
 * Channel management utility class.  Generates different types of
 * channels depending on the binding operation information.  Retains
 * the generated channels for management: lookup, remove, etc.
 * 
 * @author Sun Microsystems
 */
public class ChannelManagerImpl implements ChannelManager {

    static final Messages mMessages = Messages.getMessages(ChannelManagerImpl.class);

    static final Logger mLogger = Messages.getLogger(ChannelManagerImpl.class);

    private Map mChannelMap; // <serviceName+endpointName+msmqBindingOpQNameStr, Channel>    

    private ComponentContext mContext = null;

    /** Creates a new instance of ChannelManager */
    public ChannelManagerImpl(ComponentContext context) {
        mContext = context;
        mChannelMap = Collections.synchronizedMap(new HashMap());
    }

    /////////////////////
    //
    // public methods
    //
    /////////////////////

    /**
     * Creates a Channel and returns it.
     * Retains the created Channel for management.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException {

        String key = getUniqueKey(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                msmqBindingOpName);

        MSMQOperation msmqOperation = (MSMQOperation) endpoint.getMSMQOperations().get(msmqBindingOpName);

        synchronized (this) {
            Channel achannel = null;

            if (!mChannelMap.containsKey(key)) {

                // Create the channel
                if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {
                    achannel = new SendChannelImpl(endpoint, msmqOperation, mContext, true);
                } else { // inbound
                    achannel = new ReceiveChannelImpl(endpoint, msmqOperation, mContext, false);
                }

                // Add it to the channel map
                mChannelMap.put(key, achannel);

                mLogger.log(Level.INFO, "ChannelManagerImpl_CHANNEL_ADDED", new Object[] {
                        endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });
            } else {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl_CHANNEL_ALREADY_EXISTS", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl_CHANNEL_ALREADY_EXISTS", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });
                throw new ChannelManagerException(errMsg);
            }
            return achannel;
        }
    }

    /**
     * Removes a previously added Channel.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException {

        String key = getUniqueKey(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                msmqBindingOpName);

        Channel achannel = null;
        synchronized (this) {
            if (mChannelMap.containsKey(key)) {
                achannel = (Channel) mChannelMap.remove(key);
                mLogger.log(Level.INFO, "ChannelManagerImpl_CHANNEL_REMOVED", new Object[] {
                        endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });
            } else {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl_CHANNEL_NOT_FOUND", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl_CHANNEL_NOT_FOUND", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), msmqBindingOpName });
                throw new ChannelManagerException(errMsg);
            }

            return achannel;
        }
    }

    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the MSMQ binding operation
     * @param msmqBindingOpName The MSMQ binding operation name for which the Channel is to model
     *
     * @throws ChannelManagerException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, QName msmqBindingOpName) throws ChannelManagerException {

        return lookup(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                msmqBindingOpName);
    }

    /**
     * Lookup a previously created Channel.
     *
     * @param serviceName The Service QName.
     * @param endpointName The endpoint name.
     * @param endpointType The endpoint type; if provisioning endpoint then Endpoint.EndpointType.OUTBOUND
     *                     otherwise it is a consuming endpoint then Endpoint.EndpointType.INBOUND.
     * @param operation The MSMQ binding operation QName.
     *
     * @throws ChannelManagerException if a Channel can not be found.
     */
    public Channel lookup(QName serviceName, String endpointName, int endpointType, QName msmqBindingOpName)
            throws ChannelManagerException {

        String key = getUniqueKey(serviceName, endpointName, endpointType, msmqBindingOpName);

        synchronized (this) {
            Channel achannel = (Channel) mChannelMap.get(key);
            if (achannel == null) {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl_CHANNEL_NOT_FOUND", new Object[] {
                        serviceName.toString(), endpointName, EndpointImpl.endpointTypeToString(endpointType),
                        msmqBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl_CHANNEL_NOT_FOUND", new Object[] {
                        serviceName.toString(), endpointName, EndpointImpl.endpointTypeToString(endpointType),
                        msmqBindingOpName });
                throw new ChannelManagerException(errMsg);
            }

            return achannel;
        }
    }

    /////////////////////
    //
    // private methods
    //
    /////////////////////

    private String getUniqueKey(QName serviceName, String endpointName, int endpointType, QName operation) {
        return serviceName.toString() + endpointName + EndpointImpl.endpointTypeToString(endpointType)
                + operation.toString();

    }

    public void removeChannels() {

        mLogger.log(Level.INFO, "ChannelManagerImpl_REMOVING_CHANNELS");

        synchronized (this) {
            Iterator keyIter = mChannelMap.keySet().iterator();
            while (keyIter.hasNext()) {
                String key = (String) keyIter.next();
                Channel msmqChannel = (Channel) mChannelMap.get(key);
                try {
                    msmqChannel.close();
                } catch (MSMQException e) {
                    // we are not going to handle the exception here
                    mLogger.log(Level.INFO, "ChannelManagerImpl_CHANNEL_REMOVED_EX",
                            new Object[] { MSMQUtil.getStackTraceAsString(e) });

                }
                //mOutboundEndpointOpChannelMap.remove (key);
                if (msmqChannel.getMSMQOperationInput() != null) {
                    mLogger.log(Level.INFO, "ChannelManagerImpl_OUTBOUND_REMOVED", new Object[] {
                            msmqChannel.getMSMQOperationInput().getMsmqMessage().getDestination(),
                            msmqChannel.getEndpoint().getServiceName(), msmqChannel.getEndpoint().getEndpointName(),
                            msmqChannel.getMSMQOperation().getBindingOperation().toString() });
                }
            }
        }

        mChannelMap.clear();
    }

}
