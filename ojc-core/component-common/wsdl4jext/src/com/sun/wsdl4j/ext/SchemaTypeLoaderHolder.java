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
 * @(#)SchemaTypeLoaderHolder.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import org.apache.xmlbeans.SchemaTypeLoader;

/**
 * This interface defines a behavior that accepts an instance of
 * <code>SchemaTypeLoader</code> and holds the instance. The instance of
 * the implementation class of this interface shall receive an instance of
 * <code>SchemaTypeLoader</code> and hold the instance through a member
 * variable. 
 *  
 * @author Jun Xu
 */
public interface SchemaTypeLoaderHolder {

    /**
     * Accepts an instance of <code>SchemaTypeLoader</code>.
     * 
     * @param schemaTypeLoader The schema type loader
     */
    void setSchemaTypeLoader(SchemaTypeLoader schemaTypeLoader);
}
