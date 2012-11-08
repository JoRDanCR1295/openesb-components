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
 * @(#)BPELProcessRef.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.api;


/**
 * A BPEL that deployed in the BPEL engine.
 * @author Sun Microsystems
 * @version 

 */
public interface BPELProcessRef {
    
    /**
     * Returns the location of the BPEL.
     * @return the locaion.
     */
    String location();
    
    /**
     * Returns the target namespace of the BPEL.
     * @return the target namespace.
     */
    String targetNamespace();
    
    /**
     * Gets global ids of all instances created by this BPEL process.
     * @return the arrary of the global ids of the instance
     */
    String[] allProcessInstanceIDs();
    
    /**
     * Gets the process instance associated with given global unique id
     * @return  the process instance
     */
    BPELProcessInstanceRef getProcessInstance(String guid);
    
    /**
     * Returns the name of the process
     * @return
     */
    String name ();
    
    /**
     * Returns the uri of the process
     * @return
     */
    String uri ();
    
    /**
     * Returns the root XMLElementRef of the tree that represents
     * BPEL Document structure
     */    
    XMLElementRef getXMLElement();
    
    String[] allCorrelationSetsNames();
    
    long getCorrelationSetId(String name);
    
    long[] getWaitingCorrelatedEventIds();
    
    WaitingCorrelatedEvent getWaitingCorrelatedEvent(long eventId);
}
