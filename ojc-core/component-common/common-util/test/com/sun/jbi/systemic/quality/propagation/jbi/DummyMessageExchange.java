/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.jbi;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

/**
 *
 * @author radval
 */
public class DummyMessageExchange implements MessageExchange {

	private ExchangeStatus mStatus = ExchangeStatus.ACTIVE;
	
	private Map<String, Object> propertyMap = new HashMap<String, Object>();
	
    public URI getPattern() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getExchangeId() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public ExchangeStatus getStatus() {
        return mStatus;
    }

    public void setStatus(ExchangeStatus arg0) throws MessagingException {
        this.mStatus = arg0;
    }

    public void setError(Exception arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Exception getError() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Fault getFault() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setFault(Fault arg0) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public NormalizedMessage createMessage() throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Fault createFault() throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public NormalizedMessage getMessage(String arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setMessage(NormalizedMessage arg0, String arg1) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Object getProperty(String arg0) {
        return propertyMap.get(arg0);
    }

    public void setProperty(String key, Object value) {
    	propertyMap.put(key, value);
    }

    public void setEndpoint(ServiceEndpoint arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setService(QName arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setInterfaceName(QName arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setOperation(QName arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public ServiceEndpoint getEndpoint() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public QName getInterfaceName() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public QName getService() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public QName getOperation() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean isTransacted() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Role getRole() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Set getPropertyNames() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
