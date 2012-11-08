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
 * @(#)BeanAttributeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * An iterator of attributes of a JavaBean. Returns bean properties as
 * well as the "xml:lang" attribute.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class BeanAttributeIterator extends PropertyIterator {
    private NodePointer parent;
    private int position = 0;
    private boolean includeXmlLang;

    public BeanAttributeIterator(PropertyOwnerPointer parent, QName name) {
        super(
            parent,
            (name.getPrefix() == null
                && (name.getName() == null || name.getName().equals("*")))
                ? null
                : name.toString(),
            false,
            null);
        this.parent = parent;
        includeXmlLang =
            (name.getPrefix() != null && name.getPrefix().equals("xml"))
                && (name.getName().equals("lang") 
                || name.getName().equals("*"));
    }

    public NodePointer getNodePointer() {
        if (includeXmlLang && position == 1) {
            return new LangAttributePointer(parent);
        }
        else {
            return super.getNodePointer();
        }
    }

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        this.position = position;
        if (includeXmlLang) {
            if (position == 1) {
                return true;
            }
            else {
                return super.setPosition(position - 1);
            }
        }
        else {
            this.position = position;
            return super.setPosition(position);
        }
    }
}
