/*
 * @(#)EmptyMessageExchange.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import java.net.URI;
import java.util.Set;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

/**
 * This is a terminal message that signals the worker threads that
 * they should stop processing messages and shutdown gracefully.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @since 0.1
 */
public final class EmptyMessageExchange implements InOut {
    
    private static final String ERROR_MESSAGE = "This method should never get invoked";

    public NormalizedMessage getInMessage() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public NormalizedMessage getOutMessage() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setInMessage(NormalizedMessage arg0) throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setOutMessage(NormalizedMessage arg0) throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Fault createFault() throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public NormalizedMessage createMessage() throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public ServiceEndpoint getEndpoint() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Exception getError() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public String getExchangeId() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Fault getFault() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public QName getInterfaceName() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public NormalizedMessage getMessage(String arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public QName getOperation() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public URI getPattern() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Object getProperty(String arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Set getPropertyNames() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public Role getRole() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public QName getService() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public ExchangeStatus getStatus() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public boolean isTransacted() {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setEndpoint(ServiceEndpoint arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setError(Exception arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setFault(Fault arg0) throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setInterfaceName(QName arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setMessage(NormalizedMessage arg0, String arg1) throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setOperation(QName arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setProperty(String arg0, Object arg1) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setService(QName arg0) {
        throw new AssertionError(ERROR_MESSAGE);
    }

    public void setStatus(ExchangeStatus arg0) throws MessagingException {
        throw new AssertionError(ERROR_MESSAGE);
    }
}
