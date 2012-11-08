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
 * @(#)JDOMPointerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.jdom;

import java.util.Locale;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.NodePointerFactory;
import org.jdom.Document;
import org.jdom.Element;

/**
 * Implements NodePointerFactory for DOM elements.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMPointerFactory implements NodePointerFactory {

    public static final int JDOM_POINTER_FACTORY_ORDER = 110;

    public int getOrder() {
        return JDOM_POINTER_FACTORY_ORDER;
    }

    public NodePointer createNodePointer(
            QName name, Object bean, Locale locale)
    {
        if (bean instanceof Document) {
            return new JDOMNodePointer(bean, locale);
        }
        else if (bean instanceof Element) {
            return new JDOMNodePointer(bean, locale);
        }
        return null;
    }

    public NodePointer createNodePointer(
            NodePointer parent, QName name, Object bean)
    {
        if (bean instanceof Document) {
            return new JDOMNodePointer(parent, bean);
        }
        else if (bean instanceof Element) {
            return new JDOMNodePointer(parent, bean);
        }
        return null;
    }
}
