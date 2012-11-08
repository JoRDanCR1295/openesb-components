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
 * @(#)ValidatingWSDLReaderImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator.impl;

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

import javax.xml.namespace.QName;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;

import org.w3c.dom.Element;
import org.xml.sax.EntityResolver;

import com.ibm.wsdl.util.xml.QNameUtils;
import com.ibm.wsdl.xml.WSDLReaderImpl;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidatorRegistry;


/**
 * ValidatingWSDLReaderImpl is an implementation of ValidatingWSDLReader.
 * This class provides reading, parsing, and validation features of WSDL
 * files.
 */
public class ValidatingWSDLReaderImpl extends WSDLReaderImpl
    implements ValidatingWSDLReader {

    private ValidatorRegistry mValidatorRegistry;

    public ValidatingWSDLReaderImpl() {
        super();
    }

    public ValidatingWSDLReaderImpl(EntityResolver resolver) {
        this();
        setEntityResolver(resolver);
    }

    public void setValidatorRegistry(ValidatorRegistry validatorRegistry) {
        mValidatorRegistry = validatorRegistry;
    }

    public ValidatorRegistry getValidatorRegistry() {
        return mValidatorRegistry;
    }

    public Map readWSDL(File currentDir) throws WSDLException {

        HashMap cumulativeResults = new HashMap();

        if (currentDir.isFile()) {
            if (currentDir.getName().toLowerCase().endsWith(".wsdl")) {
                Definition def = readWSDL(currentDir.getAbsolutePath());
                cumulativeResults.put(currentDir.getName(), def);
                return cumulativeResults;
            }
        } else {
            File[] filesInCurrentDir = currentDir.listFiles();
            for (int ii = 0; ii < filesInCurrentDir.length; ii++) {
                Map source = readWSDL(filesInCurrentDir[ii]);
                if (filesInCurrentDir[ii].isDirectory()) {
                    cumulateResults(cumulativeResults, source, filesInCurrentDir[ii].getName());
                } else {
                    cumulateResults(cumulativeResults, source, null);
                }
            }
        }
        return cumulativeResults;
    }

    public void cumulateResults(Map orig, Map source, String dir) {
        Iterator it = source.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry)it.next();
            String path = (String)entry.getKey();
            if (dir != null)
                path = dir + File.separator + path;
            orig.put(path, entry.getValue());
        }
        
    }
    protected ExtensibilityElement parseSchema( Class parentType,
                                                Element el,
                                                Definition def,
                                                ExtensionRegistry extReg)
        throws WSDLException {

        ExtensibilityElement element =
            super.parseSchema(parentType, el, def, extReg);
        if (mValidatorRegistry != null) {
            try {
                QName elementType = QNameUtils.newQName(el);
                Validator validator =
                    mValidatorRegistry.queryValidator(parentType,
                                                      elementType);
                if (validator != null) {
                    validator.validate(element);
                }
            } catch (Exception ex) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                                        "Validation error",
                                        ex);
            }
        }
        return element;
    }

    protected ExtensibilityElement parseExtensibilityElement(Class parentType,
                                                             Element el,
                                                             Definition def)
        throws WSDLException {

        ExtensibilityElement element =
            super.parseExtensibilityElement(parentType, el, def);
        if (mValidatorRegistry != null) {
            try {
                QName elementType = QNameUtils.newQName(el);
                Validator validator = mValidatorRegistry.queryValidator(parentType,
                                                               elementType);
                if (validator != null) {
                    validator.validate(element);
                }
            } catch (Exception ex) {
                throw new WSDLException(WSDLException.PARSER_ERROR,
                                        "Validation error",
                                        ex);
            }
        }
        return element;
    }

}
