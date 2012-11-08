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
 * @(#)ExchangeUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;

import java.util.Iterator;
import java.util.Map;

import javax.jbi.messaging.Fault;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

import com.sun.jbi.crl.util.Util;
import com.sun.jbi.crl.xml.XmlUtil;

/**
 * Utility for handling faults.
 * @author Kevan Simpson
 */
public class FaultUtil {
	/**
	 * Determines if the specified {@link Fault} is defined on the specified
	 * {@link Operation}. This method first looks for the (optional) &quot;name&quot;
	 * attribute in the specified fault. If the fault cannot be identified by name,
	 * the first defined <code>Fault</code> with a matching type (i.e. QName) will 
	 * be returned, if such a fault exists.
	 * 
	 * @param fault A fault instance.
	 * @param op An operation definition.
	 * @return <code>true</code> if the specified <code>Fault</code> is defined
	 * 		   on the operation, <code>false</code> otherwise.
	 * @throws Exception If the <code>Fault</code> cannot be parsed.
	 */
	public static boolean isFaultDefined(Fault fault, Operation op) throws Exception {
		if (fault == null || op == null) return false;
		
		// look for fault by name first, then type
		DOMSource content = XmlUtil.toDOMSource(fault.getContent());
		NamedNodeMap attrMap = content.getNode().getAttributes();
		Node node = attrMap.getNamedItem("name");
		String name = node.getNodeValue();
		if (Util.isEmpty(name)) {
			node = attrMap.getNamedItem("type");
			QName type = QName.valueOf(node.getNodeValue());
			if (type != null) {
				// look for fault with same type...
				Map map = op.getFaults();
				for (Iterator iter = map.values().iterator(); iter.hasNext();) {
					javax.wsdl.Fault def = (javax.wsdl.Fault) iter.next();
					if (def.getMessage().getQName().equals(type)) {
						return true;
					}
				}
			}
		}
		else {	// found name
			return (op.getFault(name) != null);
		}
		
		return false;
	}
}
