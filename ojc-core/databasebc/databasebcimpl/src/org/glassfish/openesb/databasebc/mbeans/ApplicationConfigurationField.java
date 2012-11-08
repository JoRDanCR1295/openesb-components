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
 */

package org.glassfish.openesb.databasebc.mbeans;

import javax.management.openmbean.SimpleType;

/**
 * Represents an Application Object field.
 */
public abstract class ApplicationConfigurationField
        implements Cloneable, Visitable {

    /**
     * Corresponds to the field's "name" attribute in its Property
     * declaration (see jbi.xml in the packaging directory for this BC).
     * Values for fieldId must match the "name" attribute value.
     */
    public final String fieldId;
    
    /**
     * Non-unique descriptive phrase/word for the field.
     */
    public final String fieldDesc;
    
    /**
     * Data type for the field. 
     */
    public final SimpleType fieldType;
    
    public ApplicationConfigurationField(String id, String desc, SimpleType type) {
        fieldId = id;
        fieldDesc = desc;
        fieldType = type;
    }
    
    public abstract String toString();
    public abstract void fromString(String data);
    public abstract Object clone();
    
    /**
     * Validate the data against the field.
     * 
     * @return If there are no errors, this validation should return null or
     *         an empty array.  If there are correctable errors, this validation
     *         must return an array with one element of type <code>T</code>.
     *         Otherwise, an array of one or more strings describing the
     *         validation failure(s) must be returned, with the first element
     *         in array being <code>null</code>.
     */
    public abstract Object[] validate(Object data);

    /**
     * Assign a value to this field. Implements can assume that the supplied
     * value has already been validated thru a {@link #validate} call.
     * 
     * @throws ClassCastException if the runtime type of the value is invalid
     *         for the field. 
     */
    public abstract void setValue(Object value) throws ClassCastException;

    /**
     * Retrieve the value previously assigned to this field.
     * 
     * @return Value previously assigned to this field, or <code>null</code>
     *         if no value has been
     */
    public abstract Object getValue();

    /**
     * Subject this field to a visitor. Accepts a visitor and "applies" the
     * visitor on itself.  Implementations should override this as necessary.
     * 
     * @param visitor The visitor to which to apply this field.
     */
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
    
    /**
     * Determine whether the value for this field is sensitive in some way
     * (such as passwords).  This serves as a hint for data renderers to
     * treat the value differently in some implementation-specific way, such as
     * to hide or mask it.
     * 
     * @return true if the value represented by the field is sensitive, false
     *         otherwise
     */
    public boolean isSensitive() {
        return false;
    }
}
