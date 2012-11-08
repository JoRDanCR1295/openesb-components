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

package com.sun.jbi.imsbc.ims;

import com.sun.jbi.imsbc.Endpoint;
import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSOutput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.extensions.IMSOperation;

/**
 * A channel allows for sending and receiving IMS messages
 * It is associated with the web services endpoint and operation
 * 
 * @author Sun Microsystems
 */
public interface Channel {

    /**
     * Get the Endpoint associated with this channel
     *
     * @return The assoicated endpoint
     */
    public Endpoint getEndpoint();

    /**
     * Get the IMS operation associated with this channel
     *
     * @return The associated IMS operation
     */
    public IMSOperation getIMSOperation();

    /**
     * Get the IMS Input associated with this channel
     *
     * @return The associated IMS Input message
     */
    public IMSInput getIMSOperationInput();

    /**
     * Get the IMS Output associated with this channel
     *
     * @return The associated IMS Output message
     */
    public IMSOutput getIMSOperationOutput();

    /**
     * Get the IMS Message associated with this channel
     *
     * @return The associated IMS message
     */
    public IMSMessage getIMSMessage();

    /**
     * Opens the channel by doing the following:
     *
     * Creates connection to IMS for Outbound 
     *
     * @throws IMSException upon error
     */
    public void connect() throws IMSException;

    /**
     * Disconnects the channel by doing the following:
     *
     * Closes any opened connection.
     * 
     */
    public void disconnect() throws IMSException;

    /**
     * send ims Message on this channel
     * Connection is created if not already created.
     *
     * @param msg the ims message to be send
     * @throws IMSException upon error
     */
    public String send(String msg) throws IMSException;
       
    public String receive() throws IMSException;
    
}
