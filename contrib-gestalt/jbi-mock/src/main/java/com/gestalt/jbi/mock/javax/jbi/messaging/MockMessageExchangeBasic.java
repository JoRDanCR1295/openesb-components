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
 * MockMessageExchangeBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import java.net.URI;
import java.util.HashMap;
import java.util.Set;


/**
 * No need to create a Mock interface for this class because
 * the class already has the necessary accessor methods.
 */
public class MockMessageExchangeBasic implements MessageExchange {
    private static final String IN = "in";
    private static final String OUT = "out";
    private static final String FAULT = "fault";
    static int instanceCounter = 0;
    private NormalizedMessage normalizedMessage;
    private NormalizedMessage in;
    private NormalizedMessage out;
    private Exception errorException;
    private ExchangeStatus exchangeStatus = ExchangeStatus.ACTIVE;
    private HashMap properties = new HashMap();
    int instanceNumber = 0;

    public MockMessageExchangeBasic() {
        instanceCounter++;
        this.instanceNumber = instanceCounter;
    }

    public URI getPattern() {
        return null;
    }

    public String getExchangeId() {
        return "mock exchange # " + this.instanceNumber;
    }

    public ExchangeStatus getStatus() {
        return this.exchangeStatus;
    }

    public void setStatus(ExchangeStatus exchangeStatus) {
        this.exchangeStatus = exchangeStatus;
    }

    public void setError(Exception exception) {
        this.errorException = exception;
    }

    public Exception getError() {
        return this.errorException;
    }

    public Fault getFault() {
        return null;
    }

    public void setFault(Fault fault) throws MessagingException {
    }

    public NormalizedMessage createMessage() throws MessagingException {
        return normalizedMessage;
    }

    public Fault createFault() throws MessagingException {
        return null;
    }

    public NormalizedMessage getMessage(String name) {
        if (IN.equals(name)) {
            return in;
        } else if (OUT.equals(name)) {
            return out;
        } else if (FAULT.equals(name)) {
            return null;
        } else {
            return null;
        }
    }

    public void setMessage(NormalizedMessage message, String name)
        throws MessagingException {
        if (message == null) {
            throw new IllegalArgumentException("message should not be null");
        }

        if (name == null) {
            throw new IllegalArgumentException("name should not be null");
        }

        if (IN.equals(name)) {
            in = message;
        } else if (OUT.equals(name)) {
            out = message;
        } else if (FAULT.equals(name)) {
            return;
        } else {
            throw new MessagingException(
                "Message name must be in, out, or fault");
        }
    }

    public Object getProperty(String string) {
        return properties.get(string);
    }

    public void setProperty(String name, Object value) {
        if (value == null) {
            if (properties != null) {
                properties.remove(name);
            }
        } else {
            properties.put(name, value);
        }
    }

    public void setEndpoint(ServiceEndpoint serviceEndpoint) {
    }

    public void setService(QName qName) {
    }

    public void setInterfaceName(QName qName) {
    }

    public void setOperation(QName qName) {
    }

    public ServiceEndpoint getEndpoint() {
        return null;
    }

    public QName getInterfaceName() {
        return null;
    }

    public QName getService() {
        return null;
    }

    public QName getOperation() {
        return null;
    }

    public boolean isTransacted() {
        return true;
    }

    public Role getRole() {
        return null;
    }

    public Set getPropertyNames() {
        return properties.keySet();
    }

    public void setNormalizedMessage(NormalizedMessage normalizedMessage) {
        this.normalizedMessage = normalizedMessage;
    }
}
