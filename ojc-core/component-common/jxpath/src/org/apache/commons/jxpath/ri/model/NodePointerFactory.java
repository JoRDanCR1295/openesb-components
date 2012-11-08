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
 * @(#)NodePointerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model;

import java.util.Locale;

import org.apache.commons.jxpath.ri.QName;

/**
 * Creates NodePointers for objects of a certain type.
 * NodePointerFactories are ordered according to the values returned
 * by the "getOrder" method and always queried in that order.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface NodePointerFactory {

    /**
     * The factory order number determines its position between other factories.
     */
    int getOrder();

    /**
     * Create a NodePointer for the supplied object.  The node will represent
     * the "root" object for a path.
     *
     * @return  null if this factory does not recognize objects of the supplied
     * type.
     */
    NodePointer createNodePointer(QName name, Object object, Locale locale);

    /**
     * Create a NodePointer for the supplied child object.
     * <p>
     * @return null if this factory does not recognize objects of the supplied
     * type.
     */
    NodePointer createNodePointer(
        NodePointer parent,
        QName name,
        Object object);
}
