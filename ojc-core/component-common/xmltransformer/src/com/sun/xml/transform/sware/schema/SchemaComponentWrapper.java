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
 * @(#)SchemaComponentWrapper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * An interface that all schema component wrappers must implement.  With this
 * interface in place, a concrete wrapper implementation class has a way to
 * extract the original wrapped object and cast it back to something it knows.
 * Any code other than the wrapper implementation code should definitely treat
 * the return value as opaque and NOT try to interprete it.  If this rule is
 * not followed, that piece of code will not guarantee to work if the wrapping
 * implementation is changed. 
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SchemaComponentWrapper {

    /**
     * Gets the original wrapped object.  This method should only be called
     * by the wrapping implementation class.
     * 
     * @return the original wrapped object (treated as opaque) 
     */
    public Object getOpaqueWrappedObject();
}
