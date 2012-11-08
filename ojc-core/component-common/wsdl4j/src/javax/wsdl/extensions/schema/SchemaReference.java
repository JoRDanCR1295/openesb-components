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
 * @(#)SchemaReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package javax.wsdl.extensions.schema;


import java.io.Serializable;

import javax.wsdl.extensions.schema.Schema;

/**
 * Represents an include or a redefine element within a schema element.
 * 
 * @author Jeremy Hughes <hughesj@uk.ibm.com>
 */
public interface SchemaReference extends Serializable
{
    /**
     * Gets the ID attribute of the referenced schema.
     * 
     * @return the id string 
     */
    public abstract String getId();

    /**
     * Sets the ID attribute of the referenced schema.
     * 
     * @param id The id string to set.
     */
    public abstract void setId(String id);

    /**
     * Gets the schemaLocation attribute of the referenced schema.
     * 
     * @return the schemaLocation string.
     */
    public abstract String getSchemaLocationURI();

    /**
     * Sets the schemaLocation attribute of the referenced schema.
     * 
     * @param schemaLocation The schemaLocation string to set.
     */
    public abstract void setSchemaLocationURI(String schemaLocation);

    /**
     * Gets the referenced schema, represented as a LightWeightSchema.
     * 
     * @return the referenced LightWeightSchema.
     */
    public abstract Schema getReferencedSchema();

    /**
     * Sets the referenced schema to a LightWeightSchema.
     * 
     * @param referencedSchema The LightWeightSchema to set.
     */
    public abstract void setReferencedSchema(Schema referencedSchema);
}
