package com.sun.jbi.engine.workflow.process;

import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.engine.workflow.util.XmlUtil;



public class TimeoutSystemException extends Exception {

	private DOMSource mSource;
	
	public TimeoutSystemException(DOMSource source) {
		this.mSource = source;
	}
	
	@Override
	public String getMessage() {
		if(this.mSource != null && this.mSource.getNode() != null) {
			String faultXml = XmlUtil.toXml(this.mSource.getNode(), "UTF-8", false);
			
			return faultXml;
		}
		
		return super.getMessage();
	}
}
