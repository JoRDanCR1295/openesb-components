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
 * @(#)RBPELProcess.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Invoke;


/**
 * Runtime BPEL process
 *
 * @author Sun Microsystems
 */
/**
 * @author pVarghese
 *
 */
public interface RBPELProcess extends BPELProcess, ScopingElement, RActivityHolder {
    /** default process scope ID */
    Long DEFAULT_PROCESS_SCOPE_ID = new Long(-2);

    /** default process branch ID */
    Long DEFAULT_PROCESS_BRANCH_ID = new Long(-1);
    
    Long NULL_FAULTED_SCOPE_ID = new Long(-3);

    /**
     * gets start activity
     * @return Set Set of start activities
     */
    Set getStartElements();
    
    /**
     * Get the Set of all Invoke activities in this bpel process.
     * @return Set Set of Invoke activities.
     */
    Set<Invoke> getInvokeElements();

    /**
     * Returns a StartElement object for the given partnerlink name, operation,
     * and bpel message exchange.
     * @param partnerLink
     * @param oper
     * @param msgExchange
     * @return
     */
    RStartElement getStartElement(String partnerLink, String oper, 
								  String msgExchange);

    /**
     * gets start activities
     * @param isInitiateTrue boolean flag
     * @return Set Set of start activities
     */
    Set getStartElements(boolean isInitiateTrue);

    /**
     * gets BPEL ID
     * @return QName BPEL ID
     */
    QName getBPELId();
    
    
    /**
     * Return the Variable with the given variable unique id from the process list of 
     * variables.
     * TODO: think how this may effect some scope visibility. 
     * @param variableUniqueId
     * @return
     */
    RVariable getVariable(long variableUniqueId);
    
    void setEventHandlersDefined();
    
    boolean isEventHandlersDefined();
}
