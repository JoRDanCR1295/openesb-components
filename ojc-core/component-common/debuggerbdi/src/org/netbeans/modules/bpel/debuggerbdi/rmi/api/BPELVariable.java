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
 * @(#)BPELVariable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;

/**
 * Variables in the target BPEL engine for debugger to query its values.
 * @author Sun Microsystems
 * @version 
 * @see BPELDebugger
 */
public interface BPELVariable {

    /**
     * Checks if this BPEL variable is WSDL Message type.
     * @return true if this variable is WSDL Message; false if this variable is XML
     * Schema type.
     */
    boolean isWSDLMessage();
    
    /**
     * Returns the WSDL message held in this variable. 
     * @return  the WSDL message content.
     */
    WSDLMessage getWSDLMessage();
    
    /**
     * Returns the XSD type string serialization. For XSD simple type variable, it returns
     * the CII which represents the simple type value. For complex type and element, it
     * returns the string value of the EII.
     * @return xsd data
     */
    String getXSDData();
    
    /**
     * Check if this BPEL variable is a simple XSD type
     * @return true if this variable is a simple XSD type
     */
    boolean isSimpleType ();
    
    /**
     * Check if this BPEL variable is a boolean XSD type
     * @return true if this variable is a boolean XSD type
     */
    boolean isBoolean ();
    
    /**
     * Check if this BPEL variable is a string XSD type
     * @return true if this variable is a string XSD type
     */
    boolean isString ();
    
    /**
     * Check if this BPEL variable is a number XSD type
     * @return true if this variable is a number XSD type
     */
    boolean isNumber ();
    
    /**
     * Check if this BPEL variable is initialized
     * @return true if this variable is initialized
     */
    boolean isInitialized ();
    
    
}
