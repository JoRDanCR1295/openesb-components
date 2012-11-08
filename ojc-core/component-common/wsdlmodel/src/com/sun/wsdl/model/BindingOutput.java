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
 * @(#)BindingOutput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

/**
 * Describes the binding operation &lt;output&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface BindingOutput extends WSDLExtensibleElement {
    
    /** Tag for this element */
    public static final String TAG = "output";
        
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "name" attribute token */
        public static final String NAME = "name";
        
        /** "message" attribute token */
        public static final String MESSAGE = "message";
    }
    
    /** Ordinal position for name attribute */
    public static final int NAME = 0;
    
    /** Ordinal position for message attribute */
    public static final int MESSAGE = NAME + 1;
    
    /** Getter for the name of the binding input element.
     * @return  Name of binding input element.
     */
    String getName();
    
    /** Setter for the name of the binding output element.
     * @param   name    Value of the name for binding output element.
     */
    void setName(String name);
    
    /** Setter for the name of the binding output element.
     * @param   qName   Qualified name of the binding output element.
     * @param   name    Value of the name for binding output element.
     */
    void setName(String qName, String name);
    
    /** Getter for the message attribute.
     * @return  Value of message attribute.
     */
    String getMessage();
    
    /** Setter for the message attribute.
     * @param   message     Value of message attribute.
     */
    void setMessage(String message);

    /** Setter for the message attribute.
     * @param   qName       Qualified name of the binding fault element.
     * @param   message     Value of message attribute.
     */
    void setMessage(String qName, String message);
}
