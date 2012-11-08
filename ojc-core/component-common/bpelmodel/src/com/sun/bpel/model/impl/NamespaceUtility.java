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
 * @(#)NamespaceUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.Map;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class NamespaceUtility {

	private static final String NAMESPACE_PREFIX_START  = "ns";
	
	/**
	 * Generate a prefix which is not available in prefix to namespace map.
	 * If a prefix already exist for given namespace, it will not be considered.
	 * This method always generate a new prefix.
	 * @param namespace namespace for which a prefix needs to be generated.
	 * @param prefixToNamespaceMap map of prefix as the key and namespace as the value.
	 * @return a new generated prefix for given namespace.
	 */
	public static String generatePrefix(String namespace, Map prefixToNamespaceMap) {
		String prefixToGenerate = null;
		
		String tempPrefix = NAMESPACE_PREFIX_START;
		int counter = 0;
		if(prefixToNamespaceMap != null) {
			while(prefixToGenerate == null) {
				String prefix = (String) prefixToNamespaceMap.get(tempPrefix);
				if(prefix == null) {
					prefixToGenerate = tempPrefix; 
				} else {
					tempPrefix = NAMESPACE_PREFIX_START + counter++;
				}
			}
		}
		
		return prefixToGenerate;
	}
	
	
	public static boolean isPrefixExists(String prefix, Map prefixToNamespaceMap) {
		boolean prefixExists = false;
		if(prefixToNamespaceMap != null) {
			String ns = (String) prefixToNamespaceMap.get(prefix);
			if(ns != null) {
				prefixExists = true;
			}
		}
		
		return prefixExists;
	}
	
}
