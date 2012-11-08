package com.sun.jbi.systemic.quality.propagation.jbi;

import java.util.Set;
import javax.activation.DataHandler;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;
import javax.xml.transform.Source;

public class DummyNormalizedMessage implements NormalizedMessage {

	private Subject mSecurity;
	
    public void addAttachment(String arg0, DataHandler arg1) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Source getContent() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public DataHandler getAttachment(String arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Set getAttachmentNames() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void removeAttachment(String arg0) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setContent(Source arg0) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setProperty(String arg0, Object arg1) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setSecuritySubject(Subject arg0) {
        this.mSecurity = arg0;
    }

    public Set getPropertyNames() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Object getProperty(String arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Subject getSecuritySubject() {
        return mSecurity;
    }

	
}
