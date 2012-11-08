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
 * @(#)EventHandlers.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Java class for tEventHandlers complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 *    &lt;complexType name=&quot;tEventHandlers&quot;&gt;
 *      &lt;complexContent&gt;
 *        &lt;extension base=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}tExtensibleElements&quot;&gt;
 *          &lt;sequence&gt;
 *            &lt;element name=&quot;onEvent&quot; type=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}tOnMessage&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *            &lt;element name=&quot;onAlarm&quot; type=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}tOnAlarm&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *          &lt;/sequence&gt;
 *        &lt;/extension&gt;
 *      &lt;/complexContent&gt;
 *    &lt;/complexType&gt;
 * </pre>
 */
public interface EventHandlers extends Activity {

    /** Tag for this element. */
    public static final String TAG = "eventHandlers";

	/**
	 * Returns aray of EventHandlersOnEvent children.
	 * @return array of EventHandlersOnEvent.
	 */
	EventHandlersOnEvent[] getOnEvents();

	/**
	 * Returns ith EventHandlersOnEvent child .
	 * @param i index
	 * @return ith EventHandlersOnEvent object.
	 */
	EventHandlersOnEvent getOnEvent( int i );

	/**
	 * Removes ith EventHandlersOnEvent object.
	 * @param i index
	 */
	void removeOnEvent( int i );

	// OnMessage addNewOnMessage();

	// OnMessage insertNewOnMessage(int i);

	/**
	 * Set new array of EventHandlersOnEvent children.
	 * @param messages new array for set.
	 */
	void setOnEvents( EventHandlersOnEvent[] onEvent );

	/**
	 * Set <code>onEvent</code> to the ith place.
	 * @param onEvent object for set.
	 * @param i index
	 */
	void setOnEvent( EventHandlersOnEvent onEvent, int i );

	/**
	 * Adds onEvent.
	 * @param onEvent object for add.
	 */
	void addOnEvent( EventHandlersOnEvent onEvent );

	/**
	 * Insert <code>onEvent</code> on the ith place.
	 * @param onEvent object for insert.
	 * @param i index
	 */
	void insertOnEvent( EventHandlersOnEvent onEvent, int i );

	/**
	 * Returns array of onAlarm children.
	 * @return array of onAlarm children.
	 */
	EventHandlersOnAlarm[] getOnAlarms();

	/**
	 * Returns ith onAlarm.
	 * @param i index in array.
	 * @return ith onAlarm.
	 */
    EventHandlersOnAlarm getOnAlarm( int i );

	/**
	 * Removes ith onAlarm.
	 * @param i index for remove.
	 */
	void removeOnAlarm( int i );

	// OnAlarm addNewOnAlarm();

	// OnAlarm insertNewOnAlarm(int i );

	/**
	 * Set new array of onAlarm children.
	 * @param alarm array for set.
	 */
	void setOnAlarms( EventHandlersOnAlarm[] alarm );

	/**
	 * Set ith onAlarm to <code>alarm</code> object. 
	 * @param alarm object for set.
	 * @param i index.
	 */
	void setOnAlarm( EventHandlersOnAlarm alarm, int i );

	/**
	 * Add <code>alarm</code>.
	 * @param alarm object for add.
	 */
	void addOnAlarm( EventHandlersOnAlarm alarm );

	/**
	 * Insert <code>alarm</code> to the ith place.
	 * @param alarm object for set.
	 * @param i index
	 */
	void insertOnAlarm( EventHandlersOnAlarm alarm, int i );

	/**
	 * @return size of onEvent children.
	 */
	int sizeOfOnEvent();

	/**
	 * @return size of onAlarm children.
	 */
	int sizeOfOnAlarm();

}
