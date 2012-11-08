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
 * @(#)ComponentConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model.web;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.MethodExpression;
import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xmlbeans.SchemaType;
import org.xml.sax.SAXException;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.manager.framework.generic.DisplayConfiguration;
import com.sun.jbi.cam.services.configuration.ConfigurationService;
import com.sun.jbi.cam.xml.configuration.ConfigurationParser;
import com.sun.jbi.cam.xml.configuration.model.Configuration;
import com.sun.jbi.cam.xml.configuration.model.DisplayInformation;
import com.sun.jbi.cam.xml.configuration.schema.SchemaInstanceParser;
import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.component.PasswordField;
import com.sun.webui.jsf.component.Property;
import com.sun.webui.jsf.component.PropertySheet;
import com.sun.webui.jsf.component.PropertySheetSection;
import com.sun.webui.jsf.component.TextField;
import com.sun.webui.jsf.model.Option;

/**
 * @author graj
 * 
 */
public class ComponentConfiguration extends BaseBean implements Serializable {

    final static int INVALID_VALUE = -1;

    final static String SCHEMA_RETRIEVAL_OPERATION = "retrieveConfigurationDisplaySchema";

    final static String DATA_RETRIEVAL_OPERATION = "retrieveConfigurationDisplayData";

    final static String MBEAN_NAME_PREFIX = "com.sun.ebi:ServiceType=Configuration,InstallationType=";

    final static String MBEAN_NAME_SUFFIX = ",IdentificationName=";

    private static final String DOMAIN_NAME_PREFIX = "com.sun.ebi:"; //$NON-NLS-1$

    private static final String SERVICE_TYPE_PREFIX = "ServiceType=Configuration"; //$NON-NLS-1$

    private static final String INSTALLATION_TYPE_PREFIX = "InstallationType="; //$NON-NLS-1$

    private static final String IDENTIFICATION_NAME_PREFIX = "IdentificationName="; //$NON-NLS-1$

    private static final String IDENTIFICATION_SERVICE_UNIT = "ServiceUnitID="; //$NON-NLS-1$

    transient Map<String, UIInput> webUIMap = new HashMap<String, UIInput>();

    transient Map<String, SchemaType> coreTypeMap = new HashMap<String, SchemaType>();

    transient Map<String, MBeanAttributeInfo> attributeNameToInfoMap = new HashMap<String, MBeanAttributeInfo>();

    transient AttributeList attributeList = null;

    transient private Map<String, Object> configData = new LinkedHashMap<String, Object>();

    transient private Map<String, DisplayInformation> displayMap = new HashMap<String, DisplayInformation>();

    transient private Map<String, DisplayInformation> labelDisplayMap = new HashMap<String, DisplayInformation>();

    transient private ConfigurationService configService = null;

    private static String FIELD_VALUE_NAME = "propValue";

    private static String TABULAR_DATA_TYPE = "TabularData";

    private static String COMP_LIST = "compList";

    private static String LABEL_LIST = "labelList";

    transient private PropertySheet propertySheet;

    /** Holds value of property alertDetail. */
    transient private String alertDetail = null;

    /** Holds value of property alertRendered. */
    transient private boolean alertRendered = false;
    
    protected String serviceUnitName;


    /** Get the value of property alertDetail. */
    public String getAlertDetail() {
        return alertDetail;
    }

    /** Get the value of property alertRendered. */
    public boolean getAlertRendered() {
        return alertRendered;
    }

    /**
     * 
     * 
     */
    void resetAlerts() {
        alertDetail = null;
        alertRendered = false;
    }

    /**
     * 
     */
    public ComponentConfiguration() {
        this.setup();
    }

    /**
     * 
     */
    protected void setup() {
        //super.setup();
        this.getRequestParameters();
        super.getServiceManager();
        this.resetAlerts();

        // TODO: Do the initialization here
        // String objectNameString = MBEAN_NAME_PREFIX + this.componentType
        // + MBEAN_NAME_SUFFIX + this.componentName;
        String objectNameString = DOMAIN_NAME_PREFIX + SERVICE_TYPE_PREFIX
                + GenericConstants.COMMA_SEPARATOR + INSTALLATION_TYPE_PREFIX
                + Util.mapType(componentType)
                + GenericConstants.COMMA_SEPARATOR + IDENTIFICATION_NAME_PREFIX
                + componentName;
        //System.out.println("ObjectName string is: "+objectNameString);

        String[] schema = new String[1];
        String xmlData = null;
        ObjectName objectName = null;
        MBeanInfo info = null;
        MBeanAttributeInfo[] attributeInfo = null;
        String[] attributeNames = null;
        try {
            objectName = new ObjectName(objectNameString);
            MBeanServer server = this.findMBeanServer();
            schema[0] = (String) server.invoke(objectName,
                    SCHEMA_RETRIEVAL_OPERATION, null, null);
            xmlData = (String) server.invoke(objectName,
                    DATA_RETRIEVAL_OPERATION, null, null);
            info = server.getMBeanInfo(objectName);
            attributeInfo = info.getAttributes();
            if ((attributeInfo != null) && (attributeInfo.length > 0)) {
                attributeNames = new String[attributeInfo.length];
                for (int index = 0; index < attributeInfo.length; index++) {
                    if (attributeInfo[index] != null) {
                        attributeNames[index] = attributeInfo[index].getName();
                        if (attributeNames[index] != null) {
                            this.attributeNameToInfoMap
                                    .put(attributeNames[index],
                                            attributeInfo[index]);
                        }
                    }
                }
                this.attributeList = server.getAttributes(objectName,
                        attributeNames);
                this.configData = this.getConfigData();
            }
            Application application = FacesContext.getCurrentInstance()
                    .getApplication();
            this.initialize(application, attributeNames, schema, xmlData,
                    this.configData);
        } catch (MalformedObjectNameException e) {
            e.printStackTrace();
        } catch (InstanceNotFoundException e) {
            e.printStackTrace();
        } catch (IntrospectionException e) {
            e.printStackTrace();
        } catch (NullPointerException e) {
            e.printStackTrace();
        } catch (MBeanException e) {
            e.printStackTrace();
        } catch (ReflectionException e) {
            e.printStackTrace();
        }

    }

    /**
     * 
     * @return
     */
    MBeanServer findMBeanServer() {
        MBeanServer server = null;
        server = (MBeanServer) MBeanServerFactory.findMBeanServer(null).get(0);
        return server;
    }

    /**
     * 
     * @param application
     * @param attributeNames
     * @param actualSchemas
     * @param actualXMLConfigData
     * @param configurationData
     */
    void initialize(Application application, String[] attributeNames,
            String[] actualSchemas, String actualXMLConfigData,
            Map<String, Object> configurationData) {
        SchemaInstanceParser parser = null;
        boolean allowNetworkDownloadsFlag = false;
        boolean disableParticleValidRestrictionFlag = false;
        boolean diableUniqueParticleAttributionFlag = false;

        if (actualSchemas != null) {
            parser = new SchemaInstanceParser(actualSchemas,
                    allowNetworkDownloadsFlag,
                    disableParticleValidRestrictionFlag,
                    diableUniqueParticleAttributionFlag);
        }

        SchemaType[] types = parser.getGlobalElements();
        initialize(application, attributeNames, types[0], actualXMLConfigData,
                configurationData);
    }

    /**
     * 
     * @param attributeNames
     * @param actualXMLConfigData
     * @return
     */
    Configuration parseConfigurationXML(String[] attributeNames,
            String actualXMLConfigData) {
        Configuration componentConfiguration = null;
        try {
            ConfigurationParser parser = ConfigurationParser.parseFromString(
                    actualXMLConfigData, attributeNames);
            componentConfiguration = parser.getComponentConfiguration();
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return componentConfiguration;

    }

    /**
     * 
     * @param value
     * @param options
     * @return
     */
    Option getOptionFromValue(String value, Option[] options) {
        Option returnValue = null;
        if ((value != null) && (options != null) && (options.length > 0)) {
            for (int index = 0; index < options.length; index++) {
                if (value.equals(options[index]) == true) {
                    returnValue = options[index];
                    break;
                }
            }
        }
        if ((value == null) && (options != null) && (options.length > 0)) {
            returnValue = options[0];
        }
        return returnValue;
    }

    /**
     * 
     * @param application
     * @param attributeNames
     * @param root
     * @param actualXMLConfigData
     * @param configurationData
     */
    void initialize(Application application, String[] attributeNames,
            SchemaType root, String actualXMLConfigData,
            Map<String, Object> configurationData) {

        Configuration componentConfiguration = parseConfigurationXML(
                attributeNames, actualXMLConfigData);
        DisplayInformation display = null;
        if (componentConfiguration != null) {
            displayMap = componentConfiguration.getDisplayDetailsMap();
            labelDisplayMap = componentConfiguration
                    .getLabelDisplayDetailsMap();
            // TODO: Remove this
            componentConfiguration.dump();
        }

        QName qName = null;
        String prefix = root.getDocumentElementName().getPrefix();
        String namespaceURI = root.getDocumentElementName().getNamespaceURI();

        SchemaType coreType = null;
        SchemaType baseType = null;
        String[] enumValues = null;
        for (int count = 0; count < attributeNames.length; count++) {
            qName = new QName(namespaceURI, attributeNames[count], prefix);
            coreType = SchemaInstanceParser.getCoreType(qName, root);
            if(coreType == null) {
                coreType = root;
            }
            if (coreType.isPrimitiveType() == false) {
                baseType = coreType.getBaseType();
            } else {
                baseType = coreType;
            }
            //System.out.println("AttributeName = " + attributeNames[count]);
            //System.out.println("QName = " + qName);
            //System.out.println("CoreType = " + coreType.getName());
            //System.out.println("BaseType = " + baseType.getName());
            if ((qName != null) && (coreType != null)) {
                this.coreTypeMap.put(attributeNames[count], coreType);
                if (displayMap != null) {
                    display = displayMap.get(attributeNames[count]);
                }
            }
            if (baseType != null) {
                // BTC_STRING
                if (baseType.getBuiltinTypeCode() == SchemaType.BTC_STRING) {
                    // System.out.println("CoreType is =
                    // SchemaType.BTC_STRING");
                    String[] enumeratedValues = SchemaInstanceParser
                            .getEnumeratedStringValues(coreType);
                    if ((enumeratedValues != null)
                            && (enumeratedValues.length > 0)) {
                        Option[] options = new Option[enumeratedValues.length];
                        for (int index = 0; index < enumeratedValues.length; index++) {
                            options[index] = new Option(
                                    enumeratedValues[index],
                                    enumeratedValues[index]);
                        }
                        DropDown input = new DropDown();
                        input.setId(attributeNames[count]);
                        input.setDisabled(false);
                        input.setSelected(options[0]);
                        FacesContext facesContext = FacesContext
                                .getCurrentInstance();
                        ELContext elcontext = facesContext.getELContext();
                        MethodExpression methodExpression = facesContext
                                .getApplication()
                                .getExpressionFactory()
                                .createMethodExpression(
                                        elcontext,
                                        "#{ConfigurationBean.validateEnumeratedString}",
                                        null,
                                        new Class[] { FacesContext.class,
                                                UIComponent.class, Object.class });
                        input.setValidatorExpression(methodExpression);

                        input.setItems(options);
                        if (display != null) {
                            // input.setLabel(display.getDisplayName());
                            input.setToolTip(display.getDisplayDescription());
                            String value = (String) configurationData.get(qName
                                    .getLocalPart());
                            // String value = display.getDefaultValue();
                            input.setValue(this.getOptionFromValue(value,
                                    options));
                        }
                        this.webUIMap.put(attributeNames[count], input);
                        //System.out.println("Adding to webUIMap: key = " + attributeNames[count] + ", value = " + input);
                    } else {
                        // Create Text Field
                        if (display != null) {
                            if (display.isPasswordField() == true) {
                                // Create password Field
                                PasswordField input = new PasswordField();
                                input.setId(attributeNames[count]);
                                input.setDisabled(false);
                                if (display != null) {
                                    // input.setLabel(display.getDisplayName());
                                    input.setToolTip(display
                                            .getDisplayDescription());
                                    String value = (String) configurationData
                                            .get(attributeNames[count]);
                                    // String value = display.getDefaultValue();
                                    input.setText(value);
                                }
                                input.setColumns(60);
                                this.webUIMap.put(attributeNames[count], input);
                                //System.out.println("Adding to webUIMap: key =" + attributeNames[count] + ", value = " + input);

                            } else {
                                // Create regular text field
                                TextField input = new TextField();
                                input.setId(attributeNames[count]);
                                input.setDisabled(false);
                                if (display != null) {
                                    // input.setLabel(display.getDisplayName());
                                    input.setToolTip(display
                                            .getDisplayDescription());
                                    String value = (String) configurationData
                                            .get(attributeNames[count]);
                                    // String value = display.getDefaultValue();
                                    input.setText(value);
                                }
                                input.setColumns(60);
                                this.webUIMap.put(attributeNames[count], input);
                                //System.out.println("Adding to webUIMap: key =" + attributeNames[count] + ", value = " +input);

                            }
                        }

                    }
                }
                // BTC_POSITIVE_INTEGER
                if ((baseType.getBuiltinTypeCode() == SchemaType.BTC_POSITIVE_INTEGER) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_INT) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_NON_NEGATIVE_INTEGER) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_INTEGER)){
                    // System.out.println("CoreType is =
                    // SchemaType.BTC_POSITIVE_INTEGER");
                    int totalDigits = 0, minInclusive = 0, maxInclusive = 0;
                    totalDigits = SchemaInstanceParser.getDecimalFacet(
                            coreType, SchemaType.FACET_TOTAL_DIGITS);
                    minInclusive = SchemaInstanceParser.getDecimalFacet(
                            coreType, SchemaType.FACET_MIN_INCLUSIVE);
                    maxInclusive = SchemaInstanceParser.getDecimalFacet(
                            coreType, SchemaType.FACET_MAX_INCLUSIVE);
                    // Create Text Field
                    if ((totalDigits != INVALID_VALUE)
                            || (minInclusive != INVALID_VALUE)
                            || (maxInclusive != INVALID_VALUE)) {
                        TextField input = new TextField();
                        input.setId(attributeNames[count]);
                        input.setDisabled(false);
                        if (display != null) {
                            // input.setLabel(display.getDisplayName());
                            input.setToolTip(display.getDisplayDescription());
                            Integer integer = null;
                            Object objectValue = configurationData.get(attributeNames[count]);
                            if(objectValue instanceof Integer) {
                                integer = (Integer)objectValue;
                            } else {
                                integer = new Integer(objectValue+"");
                            }
                            String value = integer.toString();
                            input.setText(value);
                        }
                        FacesContext facesContext = FacesContext
                                .getCurrentInstance();
                        ELContext elcontext = facesContext.getELContext();
                        MethodExpression methodExpression = facesContext
                                .getApplication()
                                .getExpressionFactory()
                                .createMethodExpression(
                                        elcontext,
                                        "#{ConfigurationBean.validatePositiveInteger}",
                                        null,
                                        new Class[] { FacesContext.class,
                                                UIComponent.class, Object.class });
                        input.setValidatorExpression(methodExpression);
                        input.setColumns(60);
                        this.webUIMap.put(attributeNames[count], input);
                        //System.out.println("Adding to webUIMap: key = " +attributeNames[count] + ", value = " + input);

                    } else {
                        // Create regular text field
                        TextField input = new TextField();
                        input.setId(attributeNames[count]);
                        input.setDisabled(false);
                        if (display != null) {
                            // input.setLabel(display.getDisplayName());
                            input.setToolTip(display.getDisplayDescription());
                            Integer integer = null;
                            Object objectValue = configurationData.get(qName.getLocalPart());
                            if(objectValue instanceof Integer) {
                                integer = (Integer)objectValue;
                            } else {
                                integer = new Integer(objectValue+"");
                                
                            }
                            String value = integer.toString();
                            input.setText(value);
                        }
                        input.setColumns(60);
                        this.webUIMap.put(attributeNames[count], input);
                        //System.out.println("Adding to webUIMap: key = " +attributeNames[count] + ", value = " + input);
                    }

                }
            }

        }

    }

    /**
     * This method throws validator exception if specified String is invalid.
     * 
     * @param context
     * @param component
     * @param value
     * @throws ValidatorException
     * 
     * @see javax.faces.validator.Validator#validate(javax.faces.context.FacesContext,
     *      javax.faces.component.UIComponent, java.lang.Object)
     */
    public void validatePositiveInteger(FacesContext context,
            UIComponent component, Object value) throws ValidatorException {

        String msgString = null;
        FacesMessage msg = null;
        String localPart = component.getId();
        //System.out.println("Positive Integer Validator key: " + localPart);
        //System.out.println("Positive Integer Validator value: " + value);
        //System.out.println("Positive Integer Validator value class: " + value.getClass().getName());
        if (value != null) {
            SchemaType coreType = this.coreTypeMap.get(localPart);
            SchemaType baseType = null;
            if (coreType.isPrimitiveType() == false) {
                baseType = coreType.getBaseType();
            } else {
                baseType = coreType;
            }
            //System.out.println("Positive Integer baseType is: " + baseType.getName());
            if ((coreType != null)
                    && (baseType != null)
                    && ((baseType.getBuiltinTypeCode() == SchemaType.BTC_POSITIVE_INTEGER)) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_INT) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_NON_NEGATIVE_INTEGER) ||
                    (baseType.getBuiltinTypeCode() == SchemaType.BTC_INTEGER)) {
                int totalDigits = 0, minInclusive = 0, maxInclusive = 0;
                totalDigits = SchemaInstanceParser.getDecimalFacet(coreType,
                        SchemaType.FACET_TOTAL_DIGITS);
                minInclusive = SchemaInstanceParser.getDecimalFacet(coreType,
                        SchemaType.FACET_MIN_INCLUSIVE);
                maxInclusive = SchemaInstanceParser.getDecimalFacet(coreType,
                        SchemaType.FACET_MAX_INCLUSIVE);
                String stringObject = (String) value;
                Integer integer = Integer.valueOf(stringObject);
                if ((totalDigits != INVALID_VALUE)
                        && (integer.toString().length() > totalDigits)) {
                    msgString = integer.toString()
                            + ": Value exceeds maximum limit of " + totalDigits;
                }
                int positiveInteger = integer.intValue();
                if ((minInclusive != INVALID_VALUE)
                        && (positiveInteger < minInclusive)) {
                    msgString = integer.toString()
                            + ": Minimum Value allowed is " + minInclusive;
                }
                if ((maxInclusive != INVALID_VALUE)
                        && (positiveInteger > maxInclusive)) {
                    msgString = integer.toString()
                            + ": Maximum Value allowed is " + maxInclusive;

                }
                if (msgString != null) {
                    msg = new FacesMessage(msgString);
                    //System.out.println(msgString);
                    msg.setSeverity(FacesMessage.SEVERITY_ERROR);
                    alertDetail = msgString;
                    alertRendered = true;
                    throw new ValidatorException(msg);
                }

            }
        } else {
            msgString = "Integer value is null.";
            msg = new FacesMessage(msgString);
            //System.out.println(msgString);
            msg.setSeverity(FacesMessage.SEVERITY_ERROR);
            alertDetail = msgString;
            alertRendered = true;
            throw new ValidatorException(msg);
        }
        
        if ((alertDetail == null) || (alertRendered == false)) {
            this.resetAlerts();
        }
        
    }   

    /**
     * This method throws validator exception if specified String is invalid.
     * 
     * @param context
     * @param component
     * @param value
     * @throws ValidatorException
     * 
     * @see javax.faces.validator.Validator#validate(javax.faces.context.FacesContext,
     *      javax.faces.component.UIComponent, java.lang.Object)
     */
    public void validateEnumeratedString(FacesContext context,
            UIComponent component, Object value) throws ValidatorException {
        //this.resetAlerts();
        FacesMessage msg = null;
        String localPart = component.getId();
        //System.out.println("Enumerated String Validator key: " + localPart);
        //System.out.println("Enumerated String Validator value: " + value);
        //System.out.println("Enumerated String Validator value class: " + value.getClass().getName());
        if (value != null) {
            SchemaType coreType = this.coreTypeMap.get(localPart);
            SchemaType baseType = null;
            if (coreType.isPrimitiveType() == false) {
                baseType = coreType.getBaseType();
            } else {
                baseType = coreType;
            }
            if ((coreType != null) && (baseType != null)
                    && (baseType.getBuiltinTypeCode() == SchemaType.BTC_STRING)) {
                String[] enumeratedValues = SchemaInstanceParser
                        .getEnumeratedStringValues(coreType);
                int index = this.findStringInArray((String) value,
                        enumeratedValues);
                if (index == INVALID_VALUE) {
                    String msgString = "Invalid String.";
                    msg = new FacesMessage(msgString);
                    //System.out.println(msgString);
                    msg.setSeverity(FacesMessage.SEVERITY_ERROR);
                    alertDetail = msgString;
                    alertRendered = true;
                    throw new ValidatorException(msg);

                }
            }
        } else {
            String msgString = "String value is null.";
            msg = new FacesMessage(msgString);
            //System.out.println(msgString);
            msg.setSeverity(FacesMessage.SEVERITY_ERROR);
            alertDetail = msgString;
            alertRendered = true;
            throw new ValidatorException(msg);
        }
        if ((alertDetail == null) || (alertRendered == false)) {
            this.resetAlerts();
        }
    }

    /**
     * 
     * @param stringObject
     * @param stringArray
     * @return
     */
    int findStringInArray(String stringObject, String[] stringArray) {
        int returnValue = INVALID_VALUE;
        if ((stringObject != null) && (stringArray != null)
                && (stringArray.length > 0)) {
            for (int index = 0; index < stringArray.length; index++) {
                if (stringObject.equals(stringArray[index]) == true) {
                    returnValue = index;
                    break;
                }
            }

        }
        return returnValue;
    }

    // ////////////////
    // Config Bean
    // ////////////////

    /**
     * 
     * @return
     */
    public TableDataProvider getProperties() {
        // System.out.println("getProperties() ...");
        List<DisplayConfiguration> list = new ArrayList<DisplayConfiguration>();
        for (Iterator iterator = this.attributeList.iterator(); iterator
                .hasNext();) {
            Attribute attr = (Attribute) iterator.next();
            if (attr != null) {
                String key = attr.getName();
                Object value = attr.getValue();
                // todo - "value" should be of any primitive types
                DisplayConfiguration config = null;
                DisplayInformation display = this.displayMap.get(key);
                if (display != null) {
                    config = new DisplayConfiguration(display.getDisplayName(),
                            Util.convertType(value));
                } else {
                    config = new DisplayConfiguration(key, Util
                            .convertType(value));
                }

                list.add(config);
            }
        }
        provider = new ObjectListDataProvider(list);
        //System.out.println("provider: " + provider);
        return provider;
    }

    /**
     * 
     * @return
     */
    public String actionHandler() {
        // System.out.println("actionHandler called: ");
        return GenericConstants.SUCCESS;
    }

    /**
     * 
     * @return
     */
    public String reset() {
        // System.out.println("reset called...");
        this.resetAlerts();
        // restore previous configuration
        return GenericConstants.SUCCESS;
    }

    /**
     * 
     * @return
     */
    public String getLabel() {
        String label = "";
        if (GenericConstants.BC_TYPE.equals(componentType)) {
            label = Messages.getString("configuration_bc_label");
        } else if (GenericConstants.SE_TYPE.equals(componentType)) {
            label = Messages.getString("configuration_se_label");
        } else if (GenericConstants.SU_TYPE.equals(componentType)) {
            label = Messages.getString("configuration_su_label");
        }
        return label;
    }

    /**
     * 
     * @return
     */
    public String getTitle() {
        return getTitle("configuration_title");
    }

    /**
     * 
     * @param propertySheetSectionName
     * @param id
     * @return
     */
    @SuppressWarnings("unchecked")
    private List createPropertySheetSection(String propertySheetSectionName,
            int id) {
        // Property Section
        // List propertySectionList = propertySheet.getChildren();
        PropertySheetSection propertySheetSection = new PropertySheetSection();
        propertySheetSection.setId(TABULAR_DATA_TYPE + id);
        propertySheetSection.setLabel(propertySheetSectionName); // @todo -
        // localized
        // this

        propertySheet.getChildren().add(propertySheetSection);
        return propertySheetSection.getChildren();
    }

    /**
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    public PropertySheet getPropertySheet() {

        // System.out.println("getPropertySheet...");
        this.setup();

        // Property Sheet
        propertySheet = new PropertySheet();
        propertySheet.setId("propertySheet");

        // Property Section
        List propertySectionList = propertySheet.getChildren();
        PropertySheetSection propertySheetSection = new PropertySheetSection();
        propertySheetSection.setId("propertySheetSection");
        propertySheetSection
                .setLabel(Messages
                        .getString("configuration_propertysection_runtime_configuration"));

        propertySectionList.add(propertySheetSection);
        List propertyList = propertySheetSection.getChildren();

        if (configData != null) {
            createConfigProperties(configData, propertyList);
        }

        return propertySheet;

    }

    /**
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> getConfigData() {
        this.configService = serviceManager.getConfigurationService(tName);
        Map<String, Object> properties = new LinkedHashMap<String, Object>();
        Iterator iterator = this.attributeList.iterator();
        while (iterator.hasNext() == true) {
            Attribute attr = (Attribute) iterator.next();
            if (attr != null) {
                String key = attr.getName();
                Object value = attr.getValue();
                if (key != null && value != null) {
                    properties.put(key, value);
                }
            }
        }
        return properties;
    }

    /**
     * 
     * @param propSheet
     */
    public void setPropertySheet(PropertySheet propSheet) {
        this.propertySheet = propSheet;
    }

    /**
     * 
     * @param props
     * @param propertyList
     * @return
     */
    @SuppressWarnings("unchecked")
    private List createConfigProperties(Map<String, Object> props,
            List propertyList) {

        // System.out.println("createConfigProperties...");

        // add each property from configuration
        int id = 0;
        for (Iterator iterator = props.entrySet().iterator(); iterator
                .hasNext();) {
            Map.Entry entry = (Map.Entry) iterator.next();
            String key = (String) entry.getKey();
            Object value = entry.getValue();
            // System.out.println("{key = " + key + ", value = " + value + "}");
            UIComponent component = null;
            if (value instanceof Boolean) {
                // create Boolean property
                component = createBooleanProperty(id, value.toString());
            } else if (value instanceof TabularData) {
                // open mbean tabular type data
                Map map = createTabularDataProperty(id, (TabularData) value);
                // create new PropertySheetSection
                List propList = createPropertySheetSection(key, id);
                List compList = (List) map.get(COMP_LIST);
                List labelList = (List) map.get(LABEL_LIST);
                if (compList != null) {
                    propList.addAll(createProperties(id, compList, labelList));
                    id += compList.size();
                }
            } else {
                // create String property
                component = this.webUIMap.get(key);
                if (component != null) {
                    DisplayInformation display = this.displayMap.get(key);
                    key = display.getDisplayName();
                }
                //System.out.println("Retrieving from webUIMap: key = " + key +", value = " + component);

                // component = createStringProperty(id,key,value.toString());
            }
            if (component != null) {
                if (key != null) {
                    propertyList.add(createProperty(id, key, component));
                }
                /*
                 * else { propertyList.add(createProperty(id, component)); }
                 */
                id++;
            }
        }

        // System.out.println("propertyList: " + propertyList);
        return propertyList;
    }

    /**
     * 
     * @param id
     * @param value
     * @return
     */
    private UIComponent createBooleanProperty(int id, String value) {
        Checkbox checkbox = new Checkbox();
        checkbox.setId("propb" + id);
        checkbox.setValue(value);
        checkbox
                .setStyle("margin-top:10px;margin-bottom:10px;vertical-align:middle");
        return checkbox;
    }

    /**
     * 
     * @param id
     * @param tabularData
     * @return
     */
    @SuppressWarnings("unchecked")
    private Map createTabularDataProperty(int id, TabularData tabularData) {
        // System.out.println("createTabularDataProperty...");

        Map map = new HashMap();
        List compList = new ArrayList();
        List labelList = new ArrayList();

        for (Iterator dataIter = tabularData.values().iterator(); dataIter
                .hasNext();) {
            CompositeData compData = (CompositeData) dataIter.next();
            CompositeType compType = compData.getCompositeType();
            compType.keySet().iterator();
            //System.out.println("compData:" + compData);
            //System.out.println("compType: " + compType);
            //System.out.println("compType.keySet: " + compType.keySet());

            String name = null;
            String value = null;
            UIComponent component = null;
            for (Iterator itemIt = compType.keySet().iterator(); itemIt
                    .hasNext();) {
                String keyName = (String) itemIt.next();
                String keyValue = (String) itemIt.next();

                name = (String) compData.get(keyName);
                value = (String) compData.get(keyValue);

                // create property
                component = createStringProperty(id, name, value);
                compList.add(component);
                labelList.add(name);
                id++;
            }
        }

        map.put(COMP_LIST, compList);
        map.put(LABEL_LIST, labelList);
        return map;
    }
    
    @Override
    protected void getRequestParameters() {
        
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();           
        
        this.componentName = (String)getParameter(request,GenericConstants.COMPONENT_NAME);
        this.componentType= (String)getParameter(request,GenericConstants.COMPONENT_TYPE);
        this.cType = (String)getParameter(request,GenericConstants.COMPONENT_CTYPE);
        this.cName = (String)getParameter(request,GenericConstants.COMPONENT_CNAME);
        this.pName = (String)getParameter(request,GenericConstants.COMPONENT_PNAME);
        this.tName = (String)getParameter(request,GenericConstants.COMPONENT_TNAME);
        this.serviceUnitName = (String)getParameter(request,"serviceUnitName");
        
    }
  

    /**
     * 
     * @param id
     * @param compList
     * @param labelList
     * @return
     */
    @SuppressWarnings("unchecked")
    private List createProperties(int id, List compList, List labelList) {
        List propList = new ArrayList();
        Iterator iter2 = labelList.iterator();
        for (Iterator iter = compList.iterator(); iter.hasNext();) {
            UIComponent comp = (UIComponent) iter.next();
            String label = (String) iter2.next();
            Property property = createProperty(id, label, comp);
            propList.add(property);
            id++;
        }
        return propList;
    }

    /**
     * 
     * @param id
     * @param label
     * @param comp
     * @return
     */
    @SuppressWarnings("unchecked")
    private Property createProperty(int id, String label, UIComponent comp) {
        Property property = new Property();
        property.setId("property" + id);
        property.setLabel(label);
        property.getChildren().add(comp);
        return property;
    }

    /**
     * 
     * @param id
     * @param label
     * @param comp
     * @return
     */
    @SuppressWarnings("unchecked")
    private Property createProperty(int id, UIComponent comp) {
        Property property = new Property();
        property.setId("property" + id);
        property.getChildren().add(comp);
        return property;
    }

    /**
     * 
     * @param id
     * @param label
     * @param value
     * @return
     */
    private UIComponent createStringProperty(int id, String label, String value) {
        // @todo - this need to be refactored to use field type
        if (label.indexOf("password") != -1 || label.indexOf("Password") != -1) {
            PasswordField password = new PasswordField();
            password.setId("prop" + id);
            password.setText(value);
            password.setColumns(60);
            return password;
        } else {
            TextField text = new TextField();
            text.setId("prop" + id);
            text.setText(value);
            text.setColumns(60);
            return text;
        }
    }

    // ////////
    // save
    // ////////

    /**
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    public String save() {
        // System.out.println("save...");

        if ((alertDetail != null) && (alertRendered == true)) {
            this.resetAlerts();
            return null;
        }

        // 1. first collect UI data
        Map map = new HashMap<String, PropertySheetSection>(); // contains
        // <name,propertySheetSectionMap>

        if (propertySheet != null) {
            String name = null;
            DisplayInformation info = null;
            List propSheetSectionList = propertySheet.getChildren();
            for (Iterator iter = propSheetSectionList.iterator(); iter
                    .hasNext();) {
                PropertySheetSection propSheetSection = (PropertySheetSection) iter
                        .next();
                Map pmap = new LinkedHashMap<String, Object>(); // contains
                // field names
                // and values
                map.put(propSheetSection.getLabel(), pmap);
                List propList = propSheetSection.getChildren();
                for (Iterator iter1 = propList.iterator(); iter1.hasNext();) {
                    Property property = (Property) iter1.next();
                    name = property.getLabel();
                    if (name != null) {
                        info = this.labelDisplayMap.get(name);
                    }
                    if (info != null) {
                        name = info.getAttributeName();
                    }
                    for (Iterator iter2 = property.getChildren().iterator(); iter2
                            .hasNext();) {
                        UIInput comp = (UIInput) iter2.next();
                        Object value = comp.getValue();
                        pmap.put(name, value);
                    }
                }
            }
        }

        // 2. update configData
        if (configData != null) {
            for (Iterator iterator = configData.entrySet().iterator(); iterator
                    .hasNext();) {
                Map.Entry prop = (Map.Entry) iterator.next();
                String propKey = (String) prop.getKey();
                Object propValue = (Object) prop.getValue();
                Object value = null;
                if (propValue instanceof Boolean) {
                    value = findValue(map, propKey);
                } else if (propValue instanceof TabularData) {
                    Map m = findValues(map, propKey);
                    if (m != null) {
                        setTabularDataProperty((TabularData) propValue, m);
                    }
                } else {
                    value = findValue(map, propKey);
                }
                if (value != null) {
                    prop.setValue(value);
                    //System.out.println("Updating ConfigData {key=" + propKey + ", value=" + value + "}");
                }
            }
        }

        // 3. update config service
        if (configData != null) {
            // System.out.println("updating config service...");
            String name = Util.mapComponentValue(cName, componentName);
            String type = Util.mapComponentValue(cType, componentType);
            //System.out.println("name is: " + name);
            //System.out.println("type is: " + type);
            //System.out.println("configData is: " + configData);
            //System.out.println("configService is: " + configService);
            if ((configService != null) && (name != null) && (type != null)
                    && (configData != null)) {
                configService
                        .setConfigurationProperties(name, type, configData);
            }
        }
        this.resetAlerts();

        return GenericConstants.SUCCESS;

    }

    /**
     * 
     * @param map
     * @param key
     * @return
     */
    private Object findValue(Map map, String key) {
        Object value = null;
        for (Iterator iter = map.entrySet().iterator(); iter.hasNext();) {
            Map.Entry entry = (Map.Entry) iter.next();
            String ekey = (String) entry.getKey();
            Map m1 = (Map) entry.getValue();
            value = m1.get(key);
            if (value != null) {
                break;
            }
        }
        return value;
    }

    /**
     * 
     * @param map
     * @param key
     * @return
     */
    private Map findValues(Map map, String key) {
        Map m = null;
        for (Iterator iter = map.entrySet().iterator(); iter.hasNext();) {
            Map.Entry entry = (Map.Entry) iter.next();
            String ekey = (String) entry.getKey();
            Map m1 = (Map) entry.getValue();
            if (ekey.equalsIgnoreCase(key)) {
                m = m1;
                break;
            }
        }
        return m;
    }

    /**
     * 
     * @param tabularData
     * @param propValues
     */
    @SuppressWarnings("unchecked")
    private void setTabularDataProperty(TabularData tabularData, Map propValues) {

        Iterator iter2 = propValues.entrySet().iterator();
        List cList = new ArrayList();
        for (Iterator dataIter = tabularData.values().iterator(); dataIter
                .hasNext();) {
            CompositeData compData = (CompositeData) dataIter.next();
            CompositeType compType = compData.getCompositeType();
            String name = null;
            String value = null;

            for (Iterator itemIt = compType.keySet().iterator(); itemIt
                    .hasNext();) {
                String keyName = (String) itemIt.next();
                String keyValue = (String) itemIt.next();
                name = (String) compData.get(keyName);
                value = (String) compData.get(keyValue);

                Map.Entry entry = (Map.Entry) iter2.next();
                String pKey = (String) entry.getKey();
                Object pVal = entry.getValue();

                String[] itemNames = (String[]) compType.keySet().toArray(
                        new String[1]);
                Object[] itemValues = { name, pVal };
                try {
                    CompositeDataSupport compDataSupport = new CompositeDataSupport(
                            compType, itemNames, itemValues);
                    cList.add(compDataSupport);
                } catch (Exception e) {
                    e.printStackTrace();
                }

            }

        }

        // update tabularData with CompositeDataSupport
        tabularData.clear();
        for (Iterator iter = cList.iterator(); iter.hasNext();) {
            CompositeDataSupport compDataSupport = (CompositeDataSupport) iter
                    .next();
            tabularData.put(compDataSupport);
        }

    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
