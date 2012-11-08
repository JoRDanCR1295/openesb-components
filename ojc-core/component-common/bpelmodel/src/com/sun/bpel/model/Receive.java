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
 * @(#)Receive.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;receive&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Receive extends Activity, 
                                 PartnerLinkReference,
                                 PortTypeReference,
                                 OperationReference, 
                                 VariableReference,
                                 FromPartReference {
    /** Tag for this element. */
    public static final String TAG = Tags.RECEIVE;
    
    public static final String YES = "yes";
    
    public static final String NO = "no";
    
    /**
     * Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR, 
                                  PartnerLinkReference.ATTR,
                                  PortTypeReference.ATTR,
                                  OperationReference.ATTR,
                                  VariableReference.ATTR {
        
        /** "createInstance" attribute token */
        public static final String CREATE_INSTANCE = "createInstance";
        
        /** "messageExchange attribute token */
        public static final String MESSAGE_EXCHANGE = "messageExchange";
    }
    
    /** Ordinal position of the partnerLink attribute */
    public static final int PARTNERLINK = NUM_STANDARD_ATTRS;
    
    /** Ordinal position of the portType attribute */
    public static final int PORT_TYPE = PARTNERLINK + 1;
    
    /** Ordinal position of the operation attribute */
    public static final int OPERATION = PORT_TYPE + 1;
    
    /** Ordinal position of the variable attribute */
    public static final int VARIABLE = OPERATION + 1;
    
    /** Ordinal position of the createInstance attributes */
    public static final int CREATE_INSTANCE = VARIABLE + 1;
    
    /** Ordinal position of the messageExchange attributes */
    public static final int MESSAGE_EXCHANGE = CREATE_INSTANCE + 1;
    
    /** Total number of attributes */
    public static final int NUM_ATTRS = MESSAGE_EXCHANGE + 1;
    
    /** Getter for property createInstance.
     * @return Value of property createInstance.
     *
     */
    String getCreateInstance();
    
    /** Setter for property createInstance.
     * @param createInstance New value of property createInstance.
     *
     */
    void setCreateInstance(String createInstance);
    
    
    /** Getter for messageExchange attribute **/
    String getMessageExchange();
    
    /** Setter for messageExchange attribute **/
    void setMessageExchange(String value);
    
    
    /** Getter for property correlations.
     * @return property correlations.
     */
    Correlations getCorrelations();
    
    /** Setter for property correlations.
     * @param correlations New value of property correlations.
     *
     */
    void setCorrelations(Correlations correlations);
    
    
    
}
