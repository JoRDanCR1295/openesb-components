package com.sun.jbi.jmsbc.extensions;

import java.io.ByteArrayInputStream;
import java.io.Serializable;
import java.util.Properties;
import java.util.Map.Entry;

import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

public class JMSJCAOptions implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    private QName fieldElementType = JMSConstants.QNAME_JMSJCAOPTIONS;
    private Boolean fieldRequired = null;
    private String options = null;

    public QName getElementType() {
		return fieldElementType;
	}

	public Boolean getRequired() {
		return fieldRequired;
	}

	public void setElementType(QName arg0) {
		fieldElementType = arg0;
	}

	public void setRequired(Boolean arg0) {
		fieldRequired = arg0;
	}

	public String getOptions() {
		return options;
	}

	public void setOptions(String value) {
		if(value == null)
			return;
		
		Properties props = new Properties();
		try {
			props.load(new ByteArrayInputStream(value.getBytes()));
		} catch (Exception e) {
			throw new RuntimeException("Invalid JMSJCAOptions");
		}
		StringBuffer buf = new StringBuffer();
		for(Entry<Object, Object> entry : props.entrySet()){
			buf.append(entry.getKey());
			buf.append("=");
			buf.append(entry.getValue());
			buf.append('\n');
		}
		this.options = buf.toString();
	}
	
	

}
