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
 * @(#)OnMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;onMessage&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface OnMessage extends BPELElement, 
                                   SingleActivityHolder, 
                                   PartnerLinkReference,
                                   PortTypeReference,
                                   OperationReference,
                                   VariableReference {
    
    /** Tag for this element. */
    public static final String TAG = "onMessage";       // NOI18N
    
    /** Describes the attributes of this element.
     */
    public interface ATTR extends PartnerLinkReference.ATTR,
                                  PortTypeReference.ATTR,
                                  OperationReference.ATTR,
                                  VariableReference.ATTR {
        
        /** "messageExchange attribute token */
        public static final String MESSAGE_EXCHANGE = "messageExchange";
    }
    
    /** Ordinal position of partnerLink attribute. */
    public static final int PARTNERLINK = 0;
    
    /** Ordinal position of portType attribute. */
    public static final int PORT_TYPE = PARTNERLINK + 1;
    
    /** Ordinal position of operation attribute. */
    public static final int OPERATION = PORT_TYPE + 1;
    
    /** Ordinal position of variable attribute. */
    public static final int VARIABLE = OPERATION + 1;
    
    /** Ordinal position of the messageExchange attributes */
    public static final int MESSAGE_EXCHANGE = VARIABLE + 1;
   
    /** Getter for messageExchange attribute **/
    String getMessageExchange();
    
    /** Setter for messageExchange attribute **/
    void setMessageExchange(String value);
    
    /** Getter for the correlations sub-element.
     * @return  correlations sub-element.
     */
    Correlations getCorrelations();
    
    /** Setter for the correlations sub-element.
     * @param   c   correlations sub-element.
     */
    void setCorrelations(Correlations c);
    
}
