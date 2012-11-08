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
 * @(#)PropertyChangeNotifier.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;

import java.beans.PropertyChangeListener;

/**
 * Describes the contracts a notifier of <code>PropertyChangeEvent</code> should perform.
 *
 * @see java.beans.PropertyChangeListener
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface PropertyChangeNotifier {
    
    /** Adds a WSDL change listener to the WSDL producer.  The WSDL producer
     * must tolerate multiple registration of the same listener.
     * <ul>
     * <li>The <a href="http://java.sun.com/j2se/1.4.2/docs/api/java/util/EventObject.html#getSource()">getSource()</a>
     *     method must return a <code>WSDLProducer</code> castable object.</li>
     * <li>The <a href="http://java.sun.com/j2se/1.4.2/docs/api/java/beans/PropertyChangeEvent.html#getPropertyName()">getPropertyName()</a>
     *     method must return one of the following:
     *     <ul>
     *     <li><a href="./WSDLConstants.html#WSDL_DOCUMENT_PROP_CHANGE">WSDL_DOCUMENT_PROP_CHANGE</a></li>
     *     <li><a href="./WSDLConstants.html#WSDL_TARGET_NAMESPACE_PROP_CHANGE">WSDL_TARGET_NAMESPACE_PROP_CHANGE</a></li>
     *     <li><a href="./WSDLConstants.html#WSDL_MESSAGE_PROP_CHANGE">WSDL_MESSAGE_PROP_CHANGE</a></li>
     *     <li><a href="./WSDLConstants.html#WSDL_PORT_TYPE_PROP_CHANGE">WSDL_PORT_TYPE_PROP_CHANGE</a></li>
     *     <li><a href="./WSDLConstants.html#WSDL_BINDING_PROP_CHANGE">WSDL_BINDING_PROP_CHANGE</a></li>
     *     <li><a href="./WSDLConstants.html#WSDL_SERVICE_PROP_CHANGE">WSDL_SERVICE_PROP_CHANGE</a></li>
     *     </ul>
     * <li>The <a href="http://java.sun.com/j2se/1.4.2/docs/api/java/beans/PropertyChangeEvent.html#getOldValue()">getOldValue()</a>
     *     method must return the old value of the object corresponding to <code>getPropertyName()</code>.</li>
     * <li>The <a href="http://java.sun.com/j2se/1.4.2/docs/api/java/beans/PropertyChangeEvent.html#getNewValue()">getNewValue()</a>
     *     method must return the new value of the object corresponding to <code>getPropertyName()</code>.</li>
     * </ul>
     *
     * @param   listener    WSDL property change listener to be added.
     */
    void addWSDLChangeListener(PropertyChangeListener listener);
    
    /** Removes a WSDL change listener from the the WSDL producer.
     * @param   listener    WSDL property change listener to be removed.
     */
    void removeWSDLChangeListener(PropertyChangeListener listener);
    
    /** Tests if a WSDL change listener is registered at the WSDL producer.
     * @param   listener    WSDL property change listener to query.
     * @return  <code>true</code> if the WSDL property change listener is registered.
     */
    boolean hasWSDLChangeListener(PropertyChangeListener listener);
    
}
