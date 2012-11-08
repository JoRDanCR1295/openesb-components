package com.sun.mock.participant1;

import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

public class Entry {

	private QName mServiceName;
	private String mEndpointName;
	private Operation mOperation;
	private PortType mPortType;
	
	public Entry(QName serviceName, 
				 String endpointName, 
				 Operation operation,
				 PortType portType) {
		this.mServiceName = serviceName;
		this.mEndpointName = endpointName;
		this.mOperation = operation;
		this.mPortType = portType;
	}
	
	public QName getService() {
		return this.mServiceName;
	}
	
	public String getEndpoint() {
		return this.mEndpointName;
	}
	
	public Operation getOperation() {
		return this.mOperation;
	}
	
	public PortType getPortType() {
		return this.mPortType;
	}
	
	
}
