/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.dcom;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.EndpointImpl;
import com.sun.jbi.dcombc.DCOMException;
import com.sun.jbi.dcombc.Endpoint.EndpointType;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.util.DCOMUtil;

/**
 * Channel management utility class.  Generates different types of
 * channels depending on the binding operation information.  Retains
 * the generated channels for management: lookup, remove, etc.
 * 
 * @author Chandrakanth Belde
 */
public class ChannelManagerImpl implements ChannelManager {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(ChannelManagerImpl.class);

    private static final Logger mLogger = Messages.getLogger(ChannelManagerImpl.class);

    private Map mChannelMap; // <serviceName+endpointName+dcomBindingOpQNameStr, Channel>    

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
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     *
     * @throws DCOMException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException {

        String key = getUniqueKey(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                dcomBindingOpName);

        DCOMOperation dcomOperation = (DCOMOperation) endpoint.getDCOMOperations().get(dcomBindingOpName);

        synchronized (this) {
            Channel achannel = null;

            if (!mChannelMap.containsKey(key)) {

                // Create the channel
                if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {
                    achannel = new SendChannelImpl(endpoint, dcomOperation, mContext);
                } else { // inbound
                   // achannel = new ReceiveChannelImpl(endpoint, dcomOperation, mContext);
                }

                // Add it to the channel map
                mChannelMap.put(key, achannel);

                mLogger.log(Level.INFO, "ChannelManagerImpl.CHANNEL_ADDED", new Object[] {
                        endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });
            } else {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl.CHANNEL_ALREADY_EXISTS", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl.CHANNEL_ALREADY_EXISTS", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });
                throw new DCOMException(errMsg);
            }
            return achannel;
        }
    }

    /**
     * Removes a previously added Channel.
     *
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     *
     * @throws DCOMException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException {

        String key = getUniqueKey(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                dcomBindingOpName);

        Channel achannel = null;
        synchronized (this) {
            if (mChannelMap.containsKey(key)) {
                achannel = (Channel) mChannelMap.remove(key);
                mLogger.log(Level.INFO, "ChannelManagerImpl.CHANNEL_REMOVED", new Object[] {
                        endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });
            } else {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl.CHANNEL_NOT_FOUND", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl.CHANNEL_NOT_FOUND", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(),
                        EndpointImpl.endpointTypeToString(endpoint.getEndpointType()), dcomBindingOpName });
                throw new DCOMException(errMsg);
            }

            return achannel;
        }
    }

    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     *
     * @throws DCOMException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException {

        return lookup(endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType(),
                dcomBindingOpName);
    }

    /**
     * Lookup a previously created Channel.
     *
     * @param serviceName The Service QName.
     * @param endpointName The endpoint name.
     * @param endpointType The endpoint type; if provisioning endpoint then Endpoint.EndpointType.OUTBOUND
     *                     otherwise it is a consuming endpoint then Endpoint.EndpointType.INBOUND.
     * @param operation The DCOM binding operation QName.
     *
     * @throws DCOMException if a Channel can not be found.
     */
    public Channel lookup(QName serviceName, String endpointName, int endpointType, QName dcomBindingOpName)
            throws DCOMException {

        String key = getUniqueKey(serviceName, endpointName, endpointType, dcomBindingOpName);

        synchronized (this) {
            Channel achannel = (Channel) mChannelMap.get(key);
            if (achannel == null) {
                mLogger.log(Level.SEVERE, "ChannelManagerImpl.CHANNEL_NOT_FOUND", new Object[] {
                        serviceName.toString(), endpointName, EndpointImpl.endpointTypeToString(endpointType),
                        dcomBindingOpName });

                String errMsg = mMessages.getString("ChannelManagerImpl.CHANNEL_NOT_FOUND", new Object[] {
                        serviceName.toString(), endpointName, EndpointImpl.endpointTypeToString(endpointType),
                        dcomBindingOpName });
                throw new DCOMException(errMsg);
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

        mLogger.log(Level.INFO, "ChannelManagerImpl.REMOVING_CHANNELS");

        synchronized (this) {
            Iterator keyIter = mChannelMap.keySet().iterator();
            while (keyIter.hasNext()) {
                String key = (String) keyIter.next();
                Channel dcomChannel = (Channel) mChannelMap.get(key);
                try {
                    dcomChannel.close();
                } catch (DCOMException e) {
                    // we are not going to handle the exception here
                    mLogger.log(Level.INFO, "ChannelManagerImpl.CHANNEL_REMOVED_EX",
                            new Object[] { DCOMUtil.getStackTraceAsString(e) });

                }
                //mOutboundEndpointOpChannelMap.remove (key);
                if (dcomChannel.getDCOMOperationInput() != null) {
                    mLogger.log(Level.INFO, "ChannelManagerImpl.OUTBOUND_REMOVED", new Object[] {
                            dcomChannel.getDCOMConnInfo().getInterface(),
                            dcomChannel.getEndpoint().getServiceName(), dcomChannel.getEndpoint().getEndpointName(),
                            dcomChannel.getDCOMOperation().getBindingOperation().toString() });
                }
            }
        }

        mChannelMap.clear();
    }

}
