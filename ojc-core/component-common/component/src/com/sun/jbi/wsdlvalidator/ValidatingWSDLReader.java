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
 * @(#)ValidatingWSDLReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator;

import java.io.File;
import java.util.Map;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.WSDLException;

/**
 * A ValidatingWSDLReader extends WSDLReader and provides additional methods
 * to provide validation features.  The first set of methods provides the
 * setting and getting of a ValidatorRegistry.  The second set of methods
 * includes an overloaded readWSDL() method.  It provides the reading and
 * validation of WSDL files from a directory.
 *
 */
public interface ValidatingWSDLReader extends WSDLReader {

    /**
     * Sets a ValidatorRegistry
     *
     * @param        validatorRegistry the registry of validators
     */
    void setValidatorRegistry(ValidatorRegistry validatorRegistry);

    /**
     * Retrieves the ValidatorRegistry associated with this
     * ValidatingWSDLReader
     *
     * @return       the ValidatorRegistry
     */
    ValidatorRegistry getValidatorRegistry();

    /**
     * Reads and parses the WSDL files from directory, dir.
     *
     * @param        dir the directory containing all the WSDLs.  If
     * dir is a WSDL file, then dir is read and parsed
     * @return       a Collection of Definition objects representing the
     * WSDL files in dir.
     * @exception    WSDLException if any of the WSDLs in dir are invalid.
     */
    Map readWSDL(File dir) throws WSDLException;
}
