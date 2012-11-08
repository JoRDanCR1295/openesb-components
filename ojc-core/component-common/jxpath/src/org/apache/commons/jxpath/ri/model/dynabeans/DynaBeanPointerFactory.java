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
 * @(#)DynaBeanPointerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dynabeans;

import java.util.Locale;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.NodePointerFactory;

/**
 * Implements NodePointerFactory for DynaBeans.
 * See <a href="http://jakarta.apache.org/commons/beanutils.html">
 * Jakarta Commons BeanUtils
 * </a>
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class DynaBeanPointerFactory implements NodePointerFactory {

    public static final int DYNA_BEAN_POINTER_FACTORY_ORDER = 700;

    public int getOrder() {
        return DYNA_BEAN_POINTER_FACTORY_ORDER;
    }

    public NodePointer createNodePointer(
            QName name, Object bean, Locale locale)
    {
        if (bean instanceof DynaBean) {
            return new DynaBeanPointer(name, (DynaBean) bean, locale);
        }
        return null;
    }

    public NodePointer createNodePointer(
            NodePointer parent, QName name, Object bean)
    {
        if (bean instanceof DynaBean) {
            return new DynaBeanPointer(parent, name, (DynaBean) bean);
        }
        return null;
    }
}
