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
 * @(#)JbiMessage.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.qos.messaging.WrapperUtil;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * Default implementation of the JBI message wrapper.
 * @author Kevan Simpson
 */
public class JbiMessage implements WSMessage {
	private Map<String, Element> mParts;
	private Message mModel;
	private NormalizedMessage mNMessage;
	private Properties mProperties;
	
	public JbiMessage(Message msg) {
		mModel = msg;
		mParts = new HashMap<String, Element>();
		mProperties = new Properties();
	}

	/** @see com.sun.transform.engine.runtime.WSMessage#getName() */
	public QName getName() {
		return mModel.getQName();
	}

	/** @see com.sun.transform.engine.runtime.WSMessage#getPart(java.lang.String) */
	public Element getPart(String name) {
		return mParts.get(name);
	}

	/** @see com.sun.transform.engine.runtime.WSMessage#getProperty(java.lang.String) */
	public String getProperty(String key) {
	    if (Util.isEmpty(key)) {
	        return "";
	    }
	    else if (mNMessage == null) {
	        return mProperties.getProperty(key, "");
	    }
	    else {
	        Object val = mNMessage.getProperty(key);
	        return (val == null) ? "" : String.valueOf(val);
	    }
	}
	
	/** @see com.sun.transform.engine.runtime.WSMessage#initMessage(javax.jbi.messaging.NormalizedMessage) */
	public void initMessage(NormalizedMessage nm) {
	    if (nm != null) {
	        mNMessage = nm;
	        for (Object obj : mProperties.keySet()) {
	            String key = String.valueOf(obj);
	            mNMessage.setProperty(key, mProperties.getProperty(key));
	        }
	        mProperties.clear();
	    }
	}
	
	/** @see com.sun.transform.engine.runtime.WSMessage#setProperty(java.lang.String, java.lang.String) */
	public void setProperty(String key, String value) {
	    if (!Util.isEmpty(key) && value != null) {
	        if (mNMessage == null) {
	            mProperties.setProperty(key, value);
	        }
	        else {
	            mNMessage.setProperty(key, value);
	        }
	    }
	}
	
	/** @see com.sun.transform.engine.runtime.WSMessage#setPart(java.lang.String, org.w3c.dom.Element) */
	public void setPart(String name, Element part) {
		if (part != null) {
			// shortcut to assign new values for all parts to this message 
			if (name == null && WrapperUtil.isMessageWrapped(part.getOwnerDocument())) {
				List parts = mModel.getOrderedParts(null);
				for (int i = 0, n = parts.size(); i < n; i++) {
					Part p = (Part) parts.get(i);
					setPart(p.getName(), WrapperUtil.getPartElement(part, i));
				}
			}
			else if (WrapperUtil.isPartWrapped(part)) {
				mParts.put(name, WrapperUtil.unwrapPartElement(part));
			}
			else {
				mParts.put(name, part);
			}
		}
	}

	/** @see com.sun.transform.engine.runtime.WSMessage#toSource() */
	public Source toSource() {
		Document doc = XmlUtil.newDocument();
		Element msg = WrapperUtil.createJBIMessageWrapper(doc, getName(), null);
		List parts = mModel.getOrderedParts(null);
		for (int i = 0, n = parts.size(); i < n; i++) {
			msg.appendChild(
			        WrapperUtil.importJBIWrappedPart(doc, 
													 getPart(((Part) parts.get(i)).getName())));
		}
		return new DOMSource(doc);
	}
}
