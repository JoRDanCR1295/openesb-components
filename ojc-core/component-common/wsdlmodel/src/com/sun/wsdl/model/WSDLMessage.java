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
 * @(#)WSDLMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.Collection;

import com.sun.wsdl.model.common.model.XMLNode;

/**
 * Describes the &lt;wsdl:message&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSDLMessage extends NamedWSDLExtensibleElement {
    /** Tag for this element */
    public static final String TAG = "message";
    
    /** Indexed getter for property part.
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     *
     */
    Part getPart(int index);
    
    /** getter for property part.
     * @param localName name of the property.
     * @return Value of the property with <CODE>localName</CODE>.
     *
     */
    Part getPart(String localName);
    
    /** Indexed setter for property part.
     * @param index Index of the property.
     * @param part New value of the property at <CODE>index</CODE>.
     *
     */
    void setPart(int index, Part part);
    
    /** Adds new property part.
     * @param   part    New part.
     */
    void addPart(Part part);
    
    /** Adds new property part to end of list.
     * @param   part    New part.
     */
    void addPartAtTail(Part part);
    
    /** Removes a part element from the list.
     * @param   p   part element to remove.
     * @return  <tt>true</tt> if removed successfully.
     */
    boolean removePart(Part p);
    
    /** Returns number of part elements in this wsdl:message element.
     * @return  Number of part's
     */
    int getPartSize();
    
    /**
     * TODO: KEEP a map of parts. 
     * @return
     */
    Collection getParts();
}
