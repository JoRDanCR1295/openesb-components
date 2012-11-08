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
 * @(#)Util.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import java.io.ByteArrayInputStream;
import java.util.Properties;
import java.util.Map.Entry;

/**
 *
 * Utility methods
 */
public class Util {
    
    /**
     * Returns the scheme portion of the connection url. 
     * No validation is done to verify the correctness of the connection URL.
     *
     * @returns The scheme as a String or null if not scheme is provided
     */
    public static String getScheme (String connectionURL) {
        String scheme = null;
        if (!isEmpty(connectionURL)) {
            int colonIndex = connectionURL.indexOf(':');
            if (colonIndex > 0) {
                scheme = connectionURL.substring(0, colonIndex);
            }
        }
        return scheme;
    }
    
    /**
     * Determines if the String is null or zero length.
     *
     * @param str The String to test
     * @return True if "empty", false otherwise.
     */
    public static boolean isEmpty (String str) {
        return str == null || str.length() == 0;
    }
    
    
    /**
     * Throws a RT exception if false
     *
     * @param b value to test
     */
    public static void xassert(boolean b) {
        if (!b) {
            RuntimeException e = new IllegalStateException("Assertion failure");
            throw e;
        }
    }

    /**
     * Throws a RT exception
     */
    public static void notImplemented() {
        throw new RuntimeException("Not implemented");
    }
    
    public static String mergeOptions(String options, String jmsjcaOptions, boolean isTopic){
		Properties jmsjcaProps = new Properties();
    	if(jmsjcaOptions != null && jmsjcaOptions.trim().length() > 0){
    		try {
    			jmsjcaProps.load(new ByteArrayInputStream(jmsjcaOptions.getBytes()));
    		} catch (Exception e) {
    			throw new RuntimeException("Invalid Options");
    		}
    		
    	}
		
		Properties optionsProps = new Properties();
		try {
			optionsProps.load(new ByteArrayInputStream(options.getBytes()));
		} catch (Exception e) {
			throw new RuntimeException("Invalid Options");
		}

		//Add new entries in jmsjcaProps
		for(Entry<Object, Object> entry : optionsProps.entrySet()){
			if(jmsjcaProps.get(entry.getKey()) == null){
				jmsjcaProps.put(entry.getKey(), entry.getValue());
			}
		}
		
		if (isTopic) {
			if (jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_UnifiedCF) == null
					&& jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_TopicCF) != null) {
				jmsjcaProps.put(Constants.JMSJCA_RA_OPTION_UnifiedCF,
						jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_TopicCF));
			}
		} else {
			if (jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_UnifiedCF) == null
					&& jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_QueueCF) != null) {
				jmsjcaProps.put(Constants.JMSJCA_RA_OPTION_UnifiedCF,
						jmsjcaProps.get(Constants.JMSJCA_RA_OPTION_QueueCF));
			}
		}
		StringBuffer buf = new StringBuffer();
		for(Entry<Object, Object> entry : jmsjcaProps.entrySet()){
			buf.append(entry.getKey());
			buf.append("=");
			buf.append(entry.getValue());
			buf.append('\n');
		}
		return buf.toString();
    	
    }
}
