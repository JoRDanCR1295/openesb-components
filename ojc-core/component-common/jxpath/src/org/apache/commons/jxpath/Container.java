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
 * @(#)Container.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * A Container is an object implementing an indirection
 * mechanism transparent to JXPath.  For example, if property
 * "foo" of the context node has a Container as its value,
 * the XPath "foo" will produce the contents of that Container,
 * rather than the container itself.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface Container {

    /**
     * Returns the contained value.
     */
    Object getValue();

    /**
     * Modifies the value contained by this container.  May throw
     * UnsupportedOperationException.
     */
    void setValue(Object value);
}
