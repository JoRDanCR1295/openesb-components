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
 * @(#)SNMPAdaptation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;


import javax.xml.namespace.QName;

/**
 * representation of an adaptation, there can be one Endpoint to many adaptations
 *
 * @author echou
 */
public class SNMPAdaptation extends GenericOperation {
    
    private String adaptationId;
    
    
    /** Creates a new instance of SNMPListener 
     * @param adaptationId 
     * @param operationName 
     * @param endpoint 
     */
    public SNMPAdaptation(String adaptationId, QName operationName, Endpoint endpoint) {
        super(operationName, endpoint);
        
        this.adaptationId = adaptationId;
    }
    
    public String getId() {
        return adaptationId;
    }
    
}
