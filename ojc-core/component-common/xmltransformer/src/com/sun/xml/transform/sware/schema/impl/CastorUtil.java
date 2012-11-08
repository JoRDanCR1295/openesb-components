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
 * @(#)CastorUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Group;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.Structure;
import org.exolab.castor.xml.schema.Wildcard;

import com.sun.xml.transform.sware.schema.SwareSchemaComponent;

/**
 * Castor specific utilities.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class CastorUtil {
    
    /**
     * Wraps a Castor schema structure into a Sware schema component.
     * 
     * @param castorStructure the Castor structure
     * @return the wrapped Sware schema component
     * @throws UnsupportedOperationException thrown if the wrapping of the
     *          structure of a specific type is not supported
     */
    public static SwareSchemaComponent wrapStructure(
            Structure castorStructure) throws UnsupportedOperationException {
        if (castorStructure == null) {
            return null;
        }
        switch (castorStructure.getStructureType()) {
        case Structure.SCHEMA:
            return new CastorSwareSchemaImpl((Schema) castorStructure);
        case Structure.GROUP:
        case Structure.MODELGROUP:
            return new CastorSwareGroupImpl((Group) castorStructure);
        case Structure.ELEMENT:
            return new CastorSwareElementImpl((ElementDecl) castorStructure);
        case Structure.WILDCARD:
            return new CastorSwareElementWildcardImpl(
                    (Wildcard) castorStructure);
        default:
            //don't care about other structure types for now
            throw new UnsupportedOperationException(
                    "Wrapping structure of type ["
                    + castorStructure.getStructureType()
                    + "] is not supported.");
        }
    }
}
