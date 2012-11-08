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
 * @(#)Invoke.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;flow&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Invoke extends Activity, 
                                PartnerLinkReference,
                                PortTypeReference,                        
                                OperationReference,
                                CompensationHandlerHolder,
                                FromPartReference, 
                                ToPartReference {
    
    /** Tag for this element. */
    public static final String TAG = Tags.INVOKE;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR, 
                                  PartnerLinkReference.ATTR, 
                                  PortTypeReference.ATTR,                      
                                  OperationReference.ATTR {
        
        /** "inputVariable" attribute token */
        public static final String INPUT_VARIABLE = "inputVariable";
        
        /** "outputVariable" attribute token */
        public static final String OUTPUT_VARIABLE = "outputVariable";
    }
    
    /** Ordinal position of partnerLink attribute. */
    public static final int PARTNERLINK = NUM_STANDARD_ATTRS;
    
    /** Ordinal position of portType attribute. */
    public static final int PORT_TYPE = PARTNERLINK + 1;
    
    /** Ordinal position of operation attribute. */
    public static final int OPERATION = PORT_TYPE + 1;
    
    /** Ordinal position of inputVariable attribute. */
    public static final int INPUT_VARIABLE = OPERATION + 1;
    
    /** Ordinal position of outputVariable attribute. */
    public static final int OUTPUT_VARIABLE = INPUT_VARIABLE + 1;

    /** Total number of attributes */
    public static final int NUM_ATTRS = OUTPUT_VARIABLE + 1;
    
    /** Getter for attribute inputVariable.
     * @return Value of attribute inputVariable.
     *
     */
    String getInputVariable();
    
    /** Setter for attribute inputVariable.
     * @param variable New value of attribute inputVariable.
     *
     */
    void setInputVariable(String variable);
    
    /** Getter for property inputVariable.
     * @return Value of property inputVariable.
     *
     */
    Variable getInputBPELVariable();
    
    /** Setter for property inputVariable.
     * @param variable New value of property inputVariable.
     *
     */
    void setInputBPELVariable(Variable variable);
    
    
    /** Getter for attribute outputVariable.
     * @return Value of attribute outputVariable.
     *
     */
    String getOutputVariable();
    
    /** Setter for attribute outputVariable.
     * @param variable New value of attribute outputVariable.
     *
     */
    void setOutputVariable(String variable);
    
    /** Getter for property outputVariable.
     * @return Value of property outputVariable.
     *
     */
    Variable getOutputBPELVariable();
    
    /** Setter for property outputVariable.
     * @param variable New value of property outputVariable.
     *
     */
    void setOutputBPELVariable(Variable variable);
    
    /** Getter for sub-element correlations.
     * @return sub-element correlations.
     */
    Correlations getCorrelations();
    
    /** Setter for sub-element correlations.
     * @param correlations New value of sub-element correlations.
     *
     */
    void setCorrelations(Correlations correlations);
    
    /** Indexed getter for sub-element catch.
     * @param index Index of the sub-element.
     * @return Value of the sub-element at <CODE>index</CODE>.
     *
     */
    Catch getCatch(int index);
    
    /** Indexed setter for sub-element catch.
     * @param index Index of the sub-element.
     * @param c New value of the sub-element at <CODE>index</CODE>.
     *
     */
    void setCatch(int index, Catch c);
    
    /** Number of catch elements present.
     * @return  Number of catch's.
     */
    int getCatchSize();
    
    /** Add a new catch sub-element to the list.
     * @param   c   New catch.
     */
    void addCatch(Catch c);
    
    /** Inserts catch element at given index within list; pushes all elements after to the right.
     * @param   i   Index to insert catch at.
     * @param   c   catch element to insert.
     */
    void addCatch(int i, Catch c);
    
    /** Removes all the catch elements within the list.
     */
    void clearCatches();
    
    /** Remove a catch sub-element from the list.
     * @param   c   <tt>Catch</tt> object to remove.
     * @return  <tt>true</tt> if the catch sub-element was removed successfully.
     */
    boolean removeCatch(Catch c);
    
    /** Remove a catch sub-element at the indicated position in the list.
     * @param   i   Index of the catch sub-element to remove.
     */
    void removeCatch(int i);
    
    /** Index of catch element within list.
     * @param   c   catch element to index
     * @return  Index (0-based) of element.
     */
    int indexOfCatch(XMLNode c);
    
    /** Gets collection of catch elements.
     * @return Collection of catch elements.
     */
    Collection getCatches();
    
    /** Getter for sub-element catchAll.
     * @return Value of sub-element catchAll.
     *
     */
    CatchAll getCatchAll();
    
    /** Setter for sub-element catchAll.
     * @param catchAll New value of sub-element catchAll.
     *
     */
    void setCatchAll(CatchAll catchAll);
    
}
