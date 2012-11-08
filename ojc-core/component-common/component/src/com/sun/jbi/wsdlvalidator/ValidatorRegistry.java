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
 * @(#)ValidatorRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator;

import javax.xml.namespace.QName;

/**
 * ValidatorRegistry registers and maintains Validator objects.  A
 * Validator is associated with a particular javax.wsdl class (Binding,
 * BindingOperation, BindingInput, BindingOutput, BindingFault, and/or
 * Port) and the QName of the extensibility element to validate.
 * <p>
 * A ValidatingWSDLReader will use a ValidatorRegistry to get Validators
 * to use against the appropriate extensibility element.
 *
 */
public interface ValidatorRegistry {

    /**
     * Registers a Validator with this registry
     *
     * @param parentType the class that encloses the extensibility element.
     * @param elementType the element type of the extensibility element
     * @param validator the Validator
     */
    void registerValidator(Class parentType,
                           QName elementType,
                           Validator validator);

    /**
     * Retrieves a Validator associated with parentType and elementType.
     *
     * @param  parentType the class that encloses the extensibility element.
     * @param  elementType the element type of the extensibility element
     * @return the Validator associated with the parentType and elementType or
     * null if no Validator is associated with parentType and elementType
     */
    Validator queryValidator(Class parentType,
                             QName elementType);
}
