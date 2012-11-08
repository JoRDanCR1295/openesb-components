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
 * @(#)XMLProcessingInstruction.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;

/**
 * Describes a XML processing instruction.
 *
 * @see ProcessingInstructionVisitor
 * @author Sun Microsystems
 * @version 
 */
public interface XMLProcessingInstruction extends XMLNode {
    
    /** Pseudo tag for XML processing instruction. */
    public static final String TAG = "#processingInstruction";
    
    /** Getter for the target of the processing instruction.
     * @return  Target of processing instruction.
     */
    String getTarget();
    
    /** Setter for the target of the processing instruction.
     * @param   t   Target of processing instruction.
     */
    void setTarget(String t);
    
    /** Getter for the data of the processing instruction.
     * @return  Data of processing instruction.
     */
    String getData();
    
    /** Setter for the data of the processing instruction.
     * @param   d   Data of processing instruction.
     */
    void setData(String d);
}
