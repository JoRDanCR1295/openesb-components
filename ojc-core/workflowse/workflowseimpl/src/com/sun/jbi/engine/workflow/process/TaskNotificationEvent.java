package com.sun.jbi.engine.workflow.process;

import java.util.EventObject;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;


public class TaskNotificationEvent extends EventObject { 
	
	private Element mElement;
	
	
	private QName mPortTypeQName;
	
	private String mOperationName;
	
	public TaskNotificationEvent(Object source, 
							     Element element,
							     QName portTypeQName,
							     String operationName) {
		super(source);
	
		this.mElement = element;
		this.mPortTypeQName = portTypeQName;
		this.mOperationName = operationName;
	}
	
	public Element getElement() {
		return this.mElement;
	}
	
	public QName getPortTypeQName() {
		return this.mPortTypeQName;
	}
	
	public String getOperationName() {
		return this.mOperationName;
	}
	
}
