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
 * @(#)VariableContextImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import javax.jbi.messaging.ExchangeStatus;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.sun.jbi.common.qos.messaging.WrapperUtil;
import com.sun.jbi.common.util.Util;
import com.sun.transform.engine.runtime.VariableContext;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * Default implementation of {@link VariableContext}.
 * @author Kevan Simpson
 */
public class VariableContextImpl implements VariableContext {
	private Map<String, WSMessage> mWsdlMsgs = new HashMap<String, WSMessage>();
	private Map<String, Map<String, Element>> mVariables = 
			new HashMap<String, Map<String,Element>>();
	private Map<String, Properties> mProperties =
	        new HashMap<String, Properties>();
	
	/** @see com.sun.transform.engine.runtime.VariableContext#getType(java.lang.String) */
	public VariableType getType(String name) {
		Object var = getVariable(name);
		if (var != null) {
			if (var instanceof Node) return VariableType.NODE;
			if (var instanceof WSMessage) return VariableType.MESSAGE;
			if (var instanceof ExchangeStatus) return VariableType.STATUS;
			else return VariableType.UNKNOWN;
		}

		return VariableType.NULL;
	}

	/**
	 * Returns a {@link WSMessage}, <code>org.w3c.dom.Element</code>,
	 * <code>javax.jbi.messaging.ExchangeStatus</code>, or <code>null</code>.
	 * 
	 * @see com.sun.transform.engine.runtime.VariableContext#getVariable(java.lang.String)
	 */
	public Object getVariable(String name) {
		if (!Util.isEmpty(name)) {
			int dot = name.indexOf(".");
			WSMessage msg = (dot < 0) ? mWsdlMsgs.get(name) 
									  : mWsdlMsgs.get(name.substring(0, dot));
			if (msg != null) {
				return (dot < 0) ? msg : msg.getPart(name.substring(dot + 1));
			}
		}
		return null;
	}

	/** @see com.sun.transform.engine.runtime.VariableContext#setVariable(java.lang.String, java.lang.Object) */
	public void setVariable(String name, Object var) {
		/*
		 * 'var' should only be 1) WSMessage, 2) Element, or 3) ExchangeStatus
		 */
		if (var instanceof WSMessage) {
			WSMessage msg = (WSMessage) var;
			mWsdlMsgs.put(name, msg);
			updateMessage(name, msg);	// updates msg with part values previously set
		}
		else if (var instanceof Element) {
			Element elem = (Element) var;
			int dot = name.indexOf(".");
			boolean noDot = (dot < 0);
			String nm, part;
			WSMessage msg;
			
			if (noDot) {
				nm = name.substring(0);
				part = nm;
				if (WrapperUtil.isMessageWrapped(elem.getOwnerDocument())) {
					// transform JBI occurred
					msg = mWsdlMsgs.get(nm);
					if (msg == null) {
						// TODO this could be a problem...does it happen?
						throw new IllegalStateException("undefined message var: "+ nm);
					}
					else {	
						// use shortcut I put in to avoid exposing msg model
						// this updates all the parts of the message
						msg.setPart(null, elem);
					}
					return;
				}
			}
			else {
				nm = name.substring(0, dot);
				part = name.substring(dot + 1);
			}
			
			msg = mWsdlMsgs.get(nm);
			if (msg == null) {
				// store for later use...possible invoke variable, may not be declared
				storeElement(nm, part, elem);
			}
			else {
				msg.setPart(part, elem);
			}
		}
	}

	/** @see com.sun.transform.engine.runtime.VariableContext#storeMessageProperty(java.lang.String, java.lang.String, java.lang.String) */
	public void storeMessageProperty(String varName, String prop, String value) {
	    if (!Util.isEmpty(varName) && !Util.isEmpty(prop) && value != null) {
            Properties p = mProperties.get(varName);
            if (p == null) {
                p = new Properties();
                mProperties.put(varName, p);
            }
            
            p.setProperty(prop, value);
	    }
    }

    /*
	 * Stores element values for message parts belong to uninitialized messages.
	 */
	private void storeElement(String msgName, String partName, Element value) {
		Map<String, Element> vars = mVariables.get(msgName);
		if (vars == null) {
		    vars = new HashMap<String, Element>();
		    mVariables.put(msgName, vars);
		}
		
		vars.put(partName, value);
	}
	
	/*
	 * Updates a WSMessage with stored message part element values and NM properties.
	 */
	private void updateMessage(String name, WSMessage msg) {
		Map<String, Element> vars = mVariables.remove(name);
		if (vars != null) {
			for (String part : vars.keySet()) {
				msg.setPart(part, vars.get(part));
			}
		}
		
		Properties props = mProperties.remove(name);
		if (props != null) {
		    for (Object obj : props.keySet()) {
		        String key = String.valueOf(obj);
		        msg.setProperty(key, props.getProperty(key));
		    }
		}
	}
}
