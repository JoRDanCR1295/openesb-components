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
 * @(#)DynamicAttributeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dynamic;

import org.apache.commons.jxpath.ri.QName;
import org.apache.commons.jxpath.ri.model.beans.BeanAttributeIterator;
import org.apache.commons.jxpath.ri.model.beans.PropertyOwnerPointer;

/**
 * <code>DynamicAttributeIterator</code> is different from a regular
 * <code>BeanAttributeIterator</code> in that given a property name it
 * will always find that property (albeit with a null value).
 *  
 * @author <a href="mailto:dmitri@apache.org">Dmitri Plotnikov</a>
 * @version 
 */
public class DynamicAttributeIterator extends BeanAttributeIterator {

    public DynamicAttributeIterator(PropertyOwnerPointer parent, QName name) {
        super(parent, name);
    }

     protected void prepareForIndividualProperty(String name) {
         ((DynamicPropertyPointer) getPropertyPointer()).setPropertyName(name);
         super.prepareForIndividualProperty(name);
    }
}
