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

import javax.xml.namespace.QName;

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.DCOMException;

/**
 * Channel management interface
 *
 * @author Chandrakanth Belde
 */
public interface ChannelManager {

    /**
     * Creates a Channel and returns it.
     * Retains the created Channel for management.
     *
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     * @return The channel added
     *
     * @throws DCOMException if a Channel already exists for the given Endpoint
     *         and operation QName.
     */
    public Channel addChannel(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException;

    /**
     * Removes a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     * @return The channel removed
     *
     * @throws DCOMException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel removeChannel(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException;

    /**
     * Lookup a previously created Channel.
     *
     * @param endpoint The Endpoint instance containing the DCOM binding operation
     * @param dcomBindingOpName The DCOM binding operation name for which the Channel is to model
     *
     * @throws DCOMException if a Channel does not exist for the given Endpoint
     *         and operation QName.
     */
    public Channel lookup(Endpoint endpoint, QName dcomBindingOpName) throws DCOMException;

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
            throws DCOMException;

    /**
     * Removes all previously created Channel(s) if any.
     */
    public void removeChannels();

}
