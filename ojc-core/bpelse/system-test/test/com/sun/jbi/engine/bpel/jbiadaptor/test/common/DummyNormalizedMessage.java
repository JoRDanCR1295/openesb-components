package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.util.Set;

import javax.activation.DataHandler;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;
import javax.xml.transform.Source;

public class DummyNormalizedMessage implements NormalizedMessage {

	public void addAttachment(String arg0, DataHandler arg1)
			throws MessagingException {
		// TODO Auto-generated method stub

	}

	public DataHandler getAttachment(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	public Set getAttachmentNames() {
		// TODO Auto-generated method stub
		return null;
	}

	public Source getContent() {
		// TODO Auto-generated method stub
		return null;
	}

	public Object getProperty(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	public Set getPropertyNames() {
		// TODO Auto-generated method stub
		return null;
	}

	public Subject getSecuritySubject() {
		// TODO Auto-generated method stub
		return null;
	}

	public void removeAttachment(String arg0) throws MessagingException {
		// TODO Auto-generated method stub

	}

	public void setContent(Source arg0) throws MessagingException {
		// TODO Auto-generated method stub

	}

	public void setProperty(String arg0, Object arg1) {
		// TODO Auto-generated method stub

	}

	public void setSecuritySubject(Subject arg0) {
		// TODO Auto-generated method stub

	}

}
