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
 * @(#)CompensatableActivityHolder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Java class for tActivityOrCompensateContainer complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 *    &lt;complexType name=&quot;tActivityOrCompensateContainer&quot;&gt;
 *      &lt;complexContent&gt;
 *        &lt;extension base=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}tExtensibleElements&quot;&gt;
 *          &lt;choice&gt;
 *            &lt;group ref=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}activity&quot;/&gt;
 *            &lt;element name=&quot;compensate&quot; type=&quot;{http://schemas.xmlsoap.org/ws/2003/03/business-process/}tCompensate&quot;/&gt;
 *          &lt;/choice&gt;
 *        &lt;/extension&gt;
 *      &lt;/complexContent&gt;
 *    &lt;/complexType&gt;
 * </pre>
 */
public interface CompensatableActivityHolder {

	/**
	 * @return activity in this holder.
	 */
	Activity getActivity();

	/**
	 * Set child activity.
	 * @param value object for set.
	 */
	void setActivity( Activity value );

	/**
	 * Removes child activity.
	 */
	void removeActivity();

	// Activity addNewActivity( ActivityDescriptor type );
}
