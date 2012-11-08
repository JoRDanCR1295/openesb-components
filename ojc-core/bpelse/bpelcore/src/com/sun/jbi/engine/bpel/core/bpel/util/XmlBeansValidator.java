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
package com.sun.jbi.engine.bpel.core.bpel.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.w3c.dom.Element;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.wsdl4j.ext.WSDL4JExt;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import java.util.concurrent.atomic.AtomicReference;

/**
 *
 * @author Vitaly Bychkov
 */
public class XmlBeansValidator implements Validator<RuntimeVariable> {

    private XmlBeansValidator() {
    }

    public static Validator getInstance() {
        if (INSTANCE_REF.get() == null) {
            INSTANCE_REF.compareAndSet(null, new XmlBeansValidator());
        }
        return INSTANCE_REF.get();
    }

    public ValidationResult<RuntimeVariable> validate(Collection<RuntimeVariable> updatedVariables) {
        List<ValidationResult<RuntimeVariable>> results = validate(updatedVariables, false);
        return results == null || results.size() < 1 ? null : results.get(0);
    }

    public List<ValidationResult<RuntimeVariable>> validate(Collection<RuntimeVariable> updatedVariables, boolean collectAll) {
        Iterator<RuntimeVariable> varValuesItr = updatedVariables.iterator();
        List<ValidationResult<RuntimeVariable>> validationResults = new ArrayList<ValidationResult<RuntimeVariable>>();

        while (varValuesItr.hasNext()) {
            RuntimeVariable rv = varValuesItr.next();
            SchemaTypeLoader typeLoader = null;
            SchemaType xmlType = null;
            Element value = null;
            boolean isXsdTypeVariable = false;

            // check message type variable
            if (rv.getVariableDef().getMessageType() != null) {
                validationResults.addAll(validate(typeLoader, xmlType, rv.getWSMessage(), rv));

            } else {
                if (rv.getVariableDef().getElement() != null) {
                    SchemaGlobalElement elem = rv.getVariableDef().getXSDElement();
                    typeLoader = elem.getTypeSystem();
                } else {
                    xmlType = rv.getVariableDef().getXSDType();
                    isXsdTypeVariable = true;
                    typeLoader = xmlType.getTypeSystem();
                }

                //
                Object objValue = rv.getXSDVariableData();
                if (objValue instanceof Element) {
                    value = (Element)objValue;
                }

                //
                ValidationResult<RuntimeVariable> valRes = validate(typeLoader,
                        xmlType, value, rv, isXsdTypeVariable);
                if (valRes != null) {
                    validationResults.add(valRes);
                }
            }

            if (validationResults.size() > 0 && !collectAll) {
                break;
            }
        }
        return validationResults;
    }

    private List<ValidationResult<RuntimeVariable>> validate(SchemaTypeLoader typeLoader, SchemaType xmlType, WSMessage messageValue, RuntimeVariable rv) {

        boolean isXsdType = false;
        List<ValidationResult<RuntimeVariable>> messageValResults =
                new ArrayList<ValidationResult<RuntimeVariable>>();
        if (rv.getVariableDef().getMessageType() == null || messageValue == null) {
            return messageValResults;
        }

        Message message = rv.getVariableDef().getWSDLMessageType();

        Collection parts = message.getParts().values();
        Iterator partsItr = parts.iterator();
        while (partsItr.hasNext()) {
            Element value = null;

            Part part = (Part) partsItr.next();
            QName xmlTypeName = null;
            typeLoader = WSDL4JExt.getSchemaTypeLoader(part);
            if (part.getElementName() != null) {
                SchemaGlobalElement elem = typeLoader.findElement(part.getElementName());
                if (elem.getType() != null) {
                    xmlTypeName = elem.getType().getName();
                } else {
                    continue;
                }
            } else {
                xmlTypeName = part.getTypeName();
                //SchemaType xmlType = typeLoader.findType(xmlTypeName);
                xmlType = typeLoader.findType(xmlTypeName);
                //when a document is of simple type there is no need to transform
                //this protects from WSDL model bug related to user defined simple type
                //When validate feature is implemented this can not be skipped
                if (xmlType == null) {
                    continue;
                }
                isXsdType = true;
            }

            value = messageValue.getPart(part.getName());
            if (value == null) {
                // wrong part
                continue;
            }
            ValidationResult<RuntimeVariable> valRes = validate(typeLoader, xmlType, value, rv, isXsdType);
            if (valRes != null) {
                messageValResults.add(valRes);
            }
        }
        return messageValResults;
    }

    private ValidationResult<RuntimeVariable> validate(SchemaTypeLoader typeLoader, SchemaType xmlType, Object value, RuntimeVariable rv) {
        return validate(typeLoader, xmlType, value, rv, false);
    }

    private ValidationResult<RuntimeVariable> validate(SchemaTypeLoader typeLoader, SchemaType xmlType, Object value, RuntimeVariable rv, boolean isXsdTypeVariable) {

        ValidationResult<RuntimeVariable> valResult = null;

        XmlObject xobj = null;
        try {
            XmlOptions xmlOptions = new XmlOptions();
            xmlOptions = xmlOptions.setLoadLineNumbers(XmlOptions.LOAD_LINE_NUMBERS_END_ELEMENT);
            if (isXsdTypeVariable) {
                 xmlOptions.setLoadReplaceDocumentElement(null);
            }

            if (xmlType != null && xmlType.isSimpleType()) {
//                LOGGER.log(Level.FINE,"getting xml of simple type: "+xmlType);
                xmlOptions = xmlOptions.setValidateOnSet();
            }

            if (value instanceof Element) {
                xobj = typeLoader.parse((Element)value, xmlType, xmlOptions);
            } else if (value != null && !(value instanceof Element)) {
                xobj = typeLoader.parse(value.toString(), xmlType, xmlOptions);
            }

        } catch (XmlException e) {
            LOGGER.log(Level.SEVERE, value + " not loadable: ", e);
            return new ValidationResultImpl<RuntimeVariable>(rv, value, null);
        }


        if (xobj == null) {
            return new ValidationResultImpl<RuntimeVariable>(rv, value, null);
        }

        Collection errors = new ArrayList();

        XmlOptions vXmlOptions = new XmlOptions();
        vXmlOptions = vXmlOptions.setErrorListener(errors);

        SchemaType schemaType = xobj.schemaType();

        // todo m
        if (schemaType == XmlObject.type) {
            valResult = new ValidationResultImpl<RuntimeVariable>(rv, value, null);
            LOGGER.log(Level.FINEST, value + " is NOT VALID.  ");
            LOGGER.log(Level.FINEST, "  Document type not found.");
            return valResult;
        }

        if (xobj.validate(vXmlOptions)) {
//            LOGGER.log(Level.FINEST, value + " is valid.");
        } else {
            valResult = new ValidationResultImpl<RuntimeVariable>(rv, value, errors);
        }

        return valResult;
    }

    private static AtomicReference<Validator> INSTANCE_REF = new AtomicReference<Validator>();
    private static final Logger LOGGER = Logger.getLogger(XmlBeansValidator.class.getName());
    private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.XmlBeansValidator";
}
