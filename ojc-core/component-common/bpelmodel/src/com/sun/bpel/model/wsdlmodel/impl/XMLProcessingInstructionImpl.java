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
 * @(#)XMLProcessingInstructionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.wsdlmodel.impl;

import com.sun.bpel.xml.common.model.XMLProcessingInstruction;
import com.sun.bpel.xml.common.visitor.ProcessingInstructionVisitor;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements a XML processing instruction.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XMLProcessingInstructionImpl extends XMLNodeImpl
    implements XMLProcessingInstruction {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 4247934328948129355L;
    
    /** Creates a new instance of XMLProcessingInstructionImpl.
     */
    public XMLProcessingInstructionImpl() {
        super();
        setTarget("");
        setData("");
    }
    
    /** Creates a new instance of XMLProcessingInstructionImpl.
     * @param   t   Target.
     */
    public XMLProcessingInstructionImpl(String t) {
        super();
        setTarget(t);
        setData("");
    }
    
    /** Creates a new instance of XMLProcessingInstructionImpl.
     * @param   t   Target.
     * @param   d   Data.
     */
    public XMLProcessingInstructionImpl(String t, String d) {
        super();
        setTarget(t);
        setData(d);
    }
    
    /** Getter for the target of the processing instruction.
     * @return  Target of processing instruction.
     */
    public String getTarget() {
        return getLocalName();
    }
    
    /** Setter for the target of the processing instruction.
     * @param   t   Target of processing instruction.
     */
    public void setTarget(String t) {
        setLocalName(t);
        setQualifiedName(t);
    }
    
    /** Getter for the data of the processing instruction.
     * @return  Data of processing instruction.
     */
    public String getData() {
        return getValue();
    }
    
    /** Setter for the data of the processing instruction.
     * @param   d   Data of processing instruction.
     */
    public void setData(String d) {
        setValue(d);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor v) {
        return ((ProcessingInstructionVisitor) v).visit(this);
    }
}
