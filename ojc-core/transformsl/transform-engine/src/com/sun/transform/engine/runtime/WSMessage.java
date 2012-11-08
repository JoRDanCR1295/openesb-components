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
 * @(#)WSMessage.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.w3c.dom.Element;

/**
 * Describes a WSDL-defined message.
 * @author Kevan Simpson
 */
public interface WSMessage {
	/**
	 * Returns the content of the name-specified message part.
	 * @param name The message part name.
	 * @return the content of the name-specified message part.
	 */
	public Element getPart(String name);
	
	/**
	 * Returns the qualified name of the WSDL message.
	 * @return the qualified name of the WSDL message.
	 */
	public QName getName();
	
	/**
	 * Fetchs a property from this message.
	 * @param key The property key.
	 * @return The property value or an empty string.
	 */
	public String getProperty(String key);
	   
	/**
	 * Initializes this message with its runtime JBI {@link NormalizedMessage}.
	 * <p>
	 * This method is expected to copy any properties set on this message (prior 
	 * to initialization) to the specified {@link NormalizedMessage}.
	 * 
	 * @param nm The runtime normalized message.
	 */
	public void initMessage(NormalizedMessage nm);
	    
	/**
	 * Sets the content of the name-specified message part.
	 * @param name The message part name.
	 * @param part The new content.
	 */
	public void setPart(String name, Element part);

	/**
	 * Sets a {@link String} property on this message. 
	 * @param key The property key.
	 * @param value The property value.
	 */
    public void setProperty(String key, String value);

	/**
	 * Returns a {@link Source} representation of this message.
	 * @return a <code>Source</code> representation of this message.
	 */
	public Source toSource();
}
