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
 * @(#)SwareSchema.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

import com.sun.xml.transform.sware.schema.impl.ProtectedFactory;

/**
 * Generic wrapping interface for a schema object.  More methods might
 * be added if more information is needed from the wrapper.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareSchema extends ImplSpecificComponent,
        SchemaComponentWrapper, SwareSchemaComponent {

    /**
     * A convenient factory class for getting the wrapper instance
     * from an implementation specific schema object (treated as opaque to
     * the factory methods here).
     */
    public static final class Factory {

        /**
         * Gets the schema wrapper instance (an instance of SwareSchema) from
         * an implementation specific schema object.
         * 
         * @param implSpecificSchemaObject an implementaiton specific schema
         *            object
         * @return the wrapped schema object (an instance of SwareSchema)
         */
        public static SwareSchema getSwareSchema(
                Object implSpecificSchemaObject) {

            return ProtectedFactory.getSwareSchema(
                    implSpecificSchemaObject);
        }
    }
}
