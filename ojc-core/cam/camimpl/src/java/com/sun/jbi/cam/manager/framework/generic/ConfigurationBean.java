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
 * @(#)ConfigurationBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.services.configuration.ConfigurationService;
import com.sun.jbi.cam.services.management.*;
import com.sun.jbi.cam.xml.configuration.ConfigurationParser;
import com.sun.jbi.cam.xml.configuration.model.Configuration;
import com.sun.jbi.cam.xml.configuration.schema.SchemaInstanceParser;
import com.sun.webui.jsf.component.Checkbox;
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.component.PasswordField;
import com.sun.webui.jsf.component.Property;
import com.sun.webui.jsf.component.PropertySheetSection;
import com.sun.webui.jsf.model.Option;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.el.ELContext;
import javax.el.MethodExpression;
import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import com.sun.webui.jsf.component.TextField;
import com.sun.webui.jsf.component.PropertySheet;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import com.sun.jbi.cam.xml.configuration.model.DisplayInformation;
import org.apache.xmlbeans.SchemaType;
import javax.xml.namespace.QName;



/**
 * 01/25/2007 - supports XSD/XML data type
 *   - valiidation for enumerated strings, positive integer
 *
 * @author ylee
 * @author graj
 */
public class ConfigurationBean extends BaseBean implements Serializable {
    
    
    protected ConfigurationService configService = null;
    protected ManagementService managementService = null;
    
    protected static String FIELD_VALUE_NAME = "propValue";
    protected static String TABULAR_DATA_TYPE = "TabularData";
    protected static String COMP_LIST = "compList";
    protected static String LABEL_LIST = "labelList";
    protected static int INVALID_VALUE = -1;
    protected static String TARGET_LIST_KEY = "jbiConfigUpdateSelectedInstances";
    private static String INSTANCES_LIST_KEY = "instanceList";
    private static String SELECTED_INSTANCE_KEY = "jbiConfigViewSelectedInstance";
    private Option[] targetOptions = null;
    private String[] selectedTargetOptions = null;
    
    protected PropertySheet propertySheet;
    protected Map<String,Object> configData;
    private static String ALERT_TYPE_ERROR = "error";
    private static String ALERT_TYPE_SUCCESS = "success";
    private static String ALERT_TYPE_WARNING = "warning";
    private static String ALERT_TYPE_INFO = "info";
    
    /** Holds value of property alertDetail. */
    transient protected String alertMessage = null;
    
    /** Holds value of property alertRendered. */
    transient protected boolean renderAlertMessage = false;
    
    transient private String alertType = null;
    transient private String alertSummary;
    protected String serviceUnitName;
    protected String schema = null;
    protected String xmlData = null;
    protected String[] attrNames = null;
    
    transient Map<String, UIInput> webUIMap = new HashMap<String, UIInput>();
    transient protected Map<String, DisplayInformation> displayMap = new HashMap<String, DisplayInformation>();
    transient protected Map<String, DisplayInformation> labelDisplayMap = new HashMap<String, DisplayInformation>();
    transient Map<String, SchemaType> coreTypeMap = new HashMap<String, SchemaType>();
    
    private Logger logger = Logger.getLogger(ConfigurationBean.class.getName());
    
    /** Creates a new instance of ConfigurationBean */
    public ConfigurationBean() {
        //
    }
    
    @SuppressWarnings("unchecked")
    protected void setBooleanProperty(Map.Entry prop, Boolean value) {
        prop.setValue(value);
    }
    
    @SuppressWarnings("unchecked")
    protected void setTextProperty(Map.Entry prop, String value) {
        prop.setValue(value);
    }
    
    @SuppressWarnings("unchecked")
    protected void setProperty(Map.Entry prop, Object value) {
        prop.setValue(value);
    }
    
    
    protected Map findValues(Map map, String key) {
        Map m = null;
        for ( Iterator iter=map.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            String ekey = (String)entry.getKey();
            Map m1 = (Map)entry.getValue();
            if ( ekey.equalsIgnoreCase(key) ) {
                m = m1;
                break;
            }
        }
        return m;
    }
    
    protected Object findValue(Map map,String key) {
        Object value = null;
        for ( Iterator iter=map.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            String ekey = (String)entry.getKey();
            Map m1 = (Map)entry.getValue();
            value = m1.get(key);
            if ( value!=null ) {
                break;
            }
        }
        return value;
    }
    
    
    public String reset() {
        logger.fine("reset called...");
        alertMessage = null;
        renderAlertMessage = false;
        schema = null;
        xmlData = null;
        attrNames = null;
        configData = null;
        targetOptions = null;
        selectedTargetOptions = null;
        // restore previous configuration
        return GenericConstants.SUCCESS;
    }
    
    
    public String getLabel() {
        String label = "";
        if ( GenericConstants.BC_TYPE.equals(componentType) ) {
            label = Messages.getString("configuration_bc_label");
        } else if ( GenericConstants.SE_TYPE.equals(componentType) ) {
            label = Messages.getString("configuration_se_label");
        } else if ( GenericConstants.SU_TYPE.equals(componentType) ) {
            label = Messages.getString("configuration_su_label");
        }
        return label;
    }
    
    public String getTitle() {
        return getTitle("configuration_title");
    }
    
    @SuppressWarnings("unchecked")
    protected List createConfigProperties(Map<String,Object> props,List propertyList ) {
        
        logger.fine("createConfigProperties...");
        
        // add each property from configuration
        int id = 0;
        for ( Iterator iterator = props.entrySet().iterator();  iterator.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iterator.next();
            String key = (String)entry.getKey();
            Object value = entry.getValue();
            UIComponent component = null;
            if ( value instanceof Boolean ) {
                // create Boolean property
                component = createBooleanProperty(id,value.toString());
            } else if ( value instanceof TabularData ) {
                // open mbean tabular type data
                Map map = createTabularDataProperty(id,(TabularData)value);
                // create new PropertySheetSection
                List propList = createPropertySheetSection(key,id);
                List compList = (List)map.get(COMP_LIST);
                List labelList = (List)map.get(LABEL_LIST);
                if ( compList!=null ) {
                    propList.addAll(createProperties(id,compList,labelList));
                    id += compList.size();
                }
            } else {
                // create String property
                component = createStringProperty(id,key,value.toString());
            }
            if ( component!=null ) {
                propertyList.add(createProperty(id,key,component));
                id++;
            }
        }
        
        logger.fine("propertyList: "+propertyList);
        return propertyList;
    }
    
    
    @SuppressWarnings("unchecked")
    protected Property createProperty(int id, String label, UIComponent comp) {
        Property property = new Property();
        property.setId("property"+id);
        property.setLabel(label);
        property.getChildren().add(comp);
        return property;
    }
    
    @SuppressWarnings("unchecked")
    protected List createProperties(int id, List compList, List labelList) {
        List propList = new ArrayList();
        Iterator iter2 = labelList.iterator();
        for ( Iterator iter=compList.iterator(); iter.hasNext(); ) {
            UIComponent comp = (UIComponent)iter.next();
            String label = (String)iter2.next();
            Property property = createProperty(id,label,comp);
            propList.add(property);
            id++;
        }
        return propList;
    }
    
    @SuppressWarnings("unchecked")
    protected void setTabularDataProperty(TabularData tabularData, Map propValues) {
        
        Iterator iter2 = propValues.entrySet().iterator();
        List cList = new ArrayList();
        for (Iterator dataIter=tabularData.values().iterator(); dataIter.hasNext(); ) {
            CompositeData compData = (CompositeData) dataIter.next();
            CompositeType compType = compData.getCompositeType();
            String name = null;
            String value = null;
            
            for (Iterator itemIt = compType.keySet().iterator(); itemIt.hasNext(); ) {
                String keyName  = (String)itemIt.next();
                String keyValue = (String)itemIt.next();
                name = (String)compData.get(keyName);
                value = (String) compData.get(keyValue);
                
                Map.Entry entry = (Map.Entry)iter2.next();
                String pKey = (String)entry.getKey();
                Object pVal = entry.getValue();
                
                String[] itemNames = (String[])compType.keySet().toArray(new String[1]);
                Object[] itemValues= {name,pVal};
                try {
                    CompositeDataSupport compDataSupport = new CompositeDataSupport(compType, itemNames, itemValues);
                    cList.add(compDataSupport);
                } catch(Exception e) {
                    e.printStackTrace();
                }
                
            }
            
        }
        
        // update tabularData with CompositeDataSupport
        tabularData.clear();
        for (Iterator iter=cList.iterator(); iter.hasNext(); ) {
            CompositeDataSupport compDataSupport = (CompositeDataSupport)iter.next();
            tabularData.put(compDataSupport);
        }
        
    }
    
    
    @SuppressWarnings("unchecked")
    protected Map createTabularDataProperty(int id, TabularData tabularData) {
        logger.fine("createTabularDataProperty...");
        
        Map map = new HashMap();
        List compList = new ArrayList();
        List labelList = new ArrayList();
        
        for (Iterator dataIter = tabularData.values().iterator(); dataIter.hasNext(); ) {
            CompositeData compData = (CompositeData) dataIter.next();
            CompositeType compType = compData.getCompositeType();
            compType.keySet().iterator();
            logger.finer("compData:"+compData);
            logger.finer("compType: "+compType);
            logger.finer("compType.keySet: "+compType.keySet());
            
            String name = null;
            String value = null;
            UIComponent component = null;
            for (Iterator itemIt = compType.keySet().iterator(); itemIt.hasNext(); ) {
                String keyName  = (String)itemIt.next();
                String keyValue = (String)itemIt.next();
                
                name = (String)compData.get(keyName);
                value = (String) compData.get(keyValue);
                
                // create property
                component = createStringProperty(id,name,value);
                compList.add(component);
                labelList.add(name);
                id++;
            }
        }
        
        map.put(COMP_LIST,compList);
        map.put(LABEL_LIST,labelList);
        return map;
    }
    
    protected UIComponent createBooleanProperty(int id,String value) {
        Checkbox checkbox = new Checkbox();
        checkbox.setId("propb"+id);
        checkbox.setValue(value);
        checkbox.setStyle("margin-top:10px;margin-bottom:10px;vertical-align:middle");
        return checkbox;
    }
    
    protected UIComponent createStringProperty(int id,String label, String value) {
        // @todo - this need to be refactored to use field type
        if ( label.indexOf("password")!=-1 || label.indexOf("Password")!=-1 ) {
            PasswordField password = new PasswordField();
            password.setId("prop"+id);
            password.setText(value);
            password.setColumns(60);
            return password;
        } else {
            TextField text = new TextField();
            text.setId("prop"+id);
            text.setText(value);
            text.setColumns(60);
            return text;
        }
    }
    
    protected ConfigurationService getConfigurationService(String targetName) {
        ConfigurationService configService = null;
        //  get configuration service
        configService = serviceManager.getConfigurationService(targetName);
        return configService;
    }
    
    protected ManagementService getManagementService(String targetName) {
        ManagementService managementService = null;
        //  get management service
        managementService = serviceManager.getManagementService(targetName);
        return managementService;
    }
    
    
    @SuppressWarnings("unchecked")
    protected Map<String,Object> getConfigData() {
        
        logger.fine("getConfigData...");
        
        configService = getConfigurationService(tName);
        managementService = getManagementService(tName);
        
        String name = Util.mapComponentValue(cName,componentName);
        String type = Util.mapComponentValue(cType,componentType);

        // check component status
        String state = managementService.getState(name,type);
        if ( GenericConstants.SHUTDOWN_STATE.equalsIgnoreCase(state) ) {
        
            setAlertMessage(Messages.getString("configuration_msgComponentDown"));
            setRenderAlertMessage(true);
            displayStatusAlertMessage();            

        } else {

            if ( componentType.equals(GenericConstants.SU_TYPE) ) {
                // get SU configuration
                String suid = pName+ GenericConstants.HYPHEN_SEPARATOR +cName;
                configData = configService.getSUConfigurationProperties(name,type,suid);
            } else {
                // get container configuration (SE/BC)
                configData = configService.getConfigurationProperties(name,type);
            }

            logger.fine("configData="+configData);

            // get Schema
            schema = configService.getSchema(name,type);
            // get xml data
            xmlData = configService.getXmlData(name,type);
            // get Attr Names
            attrNames = configService.getAttributeNames(name,type);
        }
        
        return configData;
    }
    
    
    @SuppressWarnings("unchecked")
    private List createPropertySheetSection(String propertySheetSectionName,int id) {
        // Property Section
        List propertySectionList = propertySheet.getChildren();
        PropertySheetSection propertySheetSection = new PropertySheetSection();
        propertySheetSection.setId(TABULAR_DATA_TYPE+id);
        propertySheetSection.setLabel(propertySheetSectionName);        // @todo - localized this
        
        propertySheet.getChildren().add(propertySheetSection);
        return propertySheetSection.getChildren();
    }
    
    @SuppressWarnings("unchecked")
    public PropertySheet getPropertySheet() {
        
        logger.fine("getPropertySheet...");
        
        setup();
        reset();
        
        // Property Sheet
        propertySheet = new PropertySheet();
        propertySheet.setId("propertySheet");
        
        // Property Section
        List propertySectionList = propertySheet.getChildren();
        PropertySheetSection propertySheetSection = new PropertySheetSection();
        propertySheetSection.setId("propertySheetSection");
        propertySheetSection.setLabel(Messages.getString("configuration_propertysection_runtime_configuration"));
        
        propertySectionList.add(propertySheetSection);
        List propertyList = propertySheetSection.getChildren();
        
        configData = getConfigData();
        if ( configData!=null ) {
            
            if ( schema!=null ) {
                createConfigPropertiesValidation(configData,propertyList);
            } else {
                createConfigProperties(configData,propertyList);
            }
        }
        
        return propertySheet;
        
    }
    
    
    public void setPropertySheet(PropertySheet propSheet) {
        this.propertySheet = propSheet;
    }
    
    
    ///
    ////////////////////////////// support for XSD/XML validation //////////////////////////
    ///
    
    
    /** Get the value of property alertDetail. */
    public String getAlertMessage() {
        return alertMessage;
    }
    
    public void setAlertMessage(String msg) {
        alertMessage = msg;
    }
    
    /** Get the value of property alertRendered. */
    public boolean getAlertMessageRendered() {
        return renderAlertMessage;
    }
    
    public void setRenderAlertMessage(boolean value) {
        renderAlertMessage = value;
    }

    /** return the value of alert summary     */
    public String getAlertSummary() {
        return alertSummary;
    }
    
    public void setAlertSummary(String summary) {
        alertSummary = summary;
    }
    
    /** return the value of alert type */
    public String getAlertType() {
        return alertType;
    }
    
    public void setAlertType(String type) {
        alertType = type;
    }
    /** Get the value of property renderButtons. */
    public boolean getRenderButtons() {
        if ( configData==null || configData.isEmpty() ) {
            return false;
        } else {
            return true;
        }
    }
    
    private void resetAlerts() {
        alertMessage = null;
        renderAlertMessage = false;
        setAlertType(ALERT_TYPE_ERROR);
        setAlertSummary(Messages.getString("configuration_alert"));
    }
    
    
    /**
     *
     * @param props
     * @param propertyList
     * @return
     */
    @SuppressWarnings("unchecked")
    private List createConfigPropertiesValidation(Map<String, Object> props,
            List propertyList) {
        
        Application application = FacesContext.getCurrentInstance().getApplication();
        initialize(application, attrNames, new String[]{schema}, xmlData, configData);
        
        // add each property from configuration
        int id = 0;
        for (Iterator iterator = props.entrySet().iterator(); iterator.hasNext();) {
            Map.Entry entry = (Map.Entry) iterator.next();
            String key = (String) entry.getKey();
            Object value = entry.getValue();
            // System.out.println("{key = " + key + ", value = " + value + "}");
            UIComponent component = null;
            /*
            if (value instanceof Boolean) {
                // create Boolean property
                component = createBooleanProperty(id, value.toString());
            } else
             */
            if (value instanceof TabularData) {
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
            }
            if (component != null) {
                if (key != null) {
                    propertyList.add(createProperty(id, key, component));
                    //System.out.println("id = "+id+", key = " + key +", value = " + component);
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
     * @param value
     * @param options
     * @return
     */
    private Option getOptionFromValue(String value, Option[] options) {
        Option returnValue = null;
        if ((value != null) && (options != null) && (options.length > 0)) {
            for (int index = 0; index < options.length; index++) {
                if (value.equals(options[index].getValue()) == true) {
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
     * @param attributeNames
     * @param actualXMLConfigData
     * @return
     */
    private Configuration parseConfigurationXML(String[] attributeNames,
            String actualXMLConfigData) {
        Configuration componentConfiguration = null;
        try {
            ConfigurationParser parser = ConfigurationParser.parseFromString(
                    actualXMLConfigData, attributeNames);
            componentConfiguration = parser.getComponentConfiguration();
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return componentConfiguration;
        
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
                String[] enumeratedValues = SchemaInstanceParser.getEnumeratedStringValues(coreType);
                int index = this.findStringInArray((String) value,enumeratedValues);
                if (index == INVALID_VALUE) {
                    String msgString = Messages.getString("configuration_msgInvalidString");
                    msg = new FacesMessage(msgString);
                    //System.out.println(msgString);
                    msg.setSeverity(FacesMessage.SEVERITY_ERROR);
                    alertMessage = msgString;
                    renderAlertMessage = true;
                    displayValidationAlertMessage(alertMessage);
                    throw new ValidatorException(msg);
                    
                }
            }
        } else {
            String msgString = Messages.getString("configuration_msgNullString");
            msg = new FacesMessage(msgString);
            //System.out.println(msgString);
            msg.setSeverity(FacesMessage.SEVERITY_ERROR);
            alertMessage = msgString;
            renderAlertMessage = true;
            displayValidationAlertMessage(alertMessage);
            throw new ValidatorException(msg);
        }
        
        if ((alertMessage == null) || ( renderAlertMessage == false)) {
            resetAlerts();
        }
    }
    
    
    /**
     *
     * @param stringObject
     * @param stringArray
     * @return
     */
    private int findStringInArray(String stringObject, String[] stringArray) {
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
            labelDisplayMap = componentConfiguration.getLabelDisplayDetailsMap();
            // TODO: Remove this
            //componentConfiguration.dump();
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
                coreTypeMap.put(attributeNames[count], coreType);
                if (displayMap != null) {
                    display = displayMap.get(attributeNames[count]);
                }
            }
            if (baseType != null) {
                // BTC_BOOLEAN
                if(baseType.getBuiltinTypeCode() == SchemaType.BTC_BOOLEAN) {
                    Checkbox input = new Checkbox();
                    input.setId(attributeNames[count]);
                    if (display != null) {
                        // input.setLabel(display.getDisplayName());
                        input.setToolTip(display.getDisplayDescription());
                        String value = configurationData.get(attributeNames[count])+"";
                        // String value = display.getDefaultValue();
                        input.setValue(value);
                    }
                    input.setStyle("margin-top:10px;margin-bottom:10px;vertical-align:middle");         // $N0N-NLS$
                    this.webUIMap.put(attributeNames[count], input);
                    
                }
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
                                "#{ConfigurationBean.validateEnumeratedString}",                // $NON-NLS$ 
                                null,
                                new Class[] { FacesContext.class,
                                UIComponent.class, Object.class });
                        input.setValidatorExpression(methodExpression);
                        
                        input.setItems(options);
                        if (display != null) {
                            input.setToolTip(display.getDisplayDescription());
                            String value = (String) configurationData.get(attributeNames[count]);
                            Option option = getOptionFromValue(value,options);
                            input.resetValue();
                            input.setSelected(option);
                            input.setValue(value);
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
                                    input.setToolTip(display.getDisplayDescription());
                                    String value = (String) configurationData.get(attributeNames[count]);
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
                                    input.setToolTip(display.getDisplayDescription());
                                    String value = (String) configurationData.get(attributeNames[count]);
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
                            Object objectValue = null;
                            try {
                                objectValue = configurationData.get(attributeNames[count]);
                                if(objectValue instanceof Integer) {
                                    integer = (Integer)objectValue;
                                } else {
                                    integer = new Integer(objectValue+"");
                                }
                            } catch(NumberFormatException e) {
                                logger.warning(Messages.getString("configuration_msgInvalidXmlData")+attributeNames[count]+
                                       " " + Messages.getString("configuration_msgInvalidXmlData2")+objectValue);
                                integer = new Integer(0);
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
                            Object objectValue = null;
                            try {
                                objectValue = configurationData.get(qName.getLocalPart());
                                if(objectValue instanceof Integer) {
                                    integer = (Integer)objectValue;
                                } else {
                                    integer = new Integer(objectValue+"");
                                }
                            } catch(NumberFormatException e) {
                                logger.warning(Messages.getString("configuration_msgInvalidXmlData")+attributeNames[count]+
                                        " " + Messages.getString("configuration_msgInvalidXmlData2")+objectValue);
                                integer = new Integer(0);
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
                Integer integer = null;
                try {
                    integer = Integer.valueOf(stringObject);
                } catch(NumberFormatException e) {
                    msgString = value+ Messages.getString("configuration_msgNotNumber");
                    msg = new FacesMessage(msgString);
                    msg.setSeverity(FacesMessage.SEVERITY_ERROR);
                    alertMessage = msgString;
                    renderAlertMessage = true;
                    displayValidationAlertMessage(alertMessage);
                    throw new ValidatorException(msg);
                }
                if ((totalDigits != INVALID_VALUE)
                && (integer.toString().length() > totalDigits)) {
                    msgString = integer.toString()
                    + Messages.getString("configuration_msgValueExceedMax") + totalDigits;
                }
                int positiveInteger = integer.intValue();
                if ((minInclusive != INVALID_VALUE)
                && (positiveInteger < minInclusive)) {
                    msgString = integer.toString()
                    + Messages.getString("configuration_msgMinValueAllowed") + minInclusive;
                }
                if ((maxInclusive != INVALID_VALUE)
                && (positiveInteger > maxInclusive)) {
                    msgString = integer.toString()
                    + Messages.getString("configuration_msgMaxValueAllowed") + maxInclusive;
                    
                }
                if (msgString != null) {
                    msg = new FacesMessage(msgString);
                    //System.out.println(msgString);
                    msg.setSeverity(FacesMessage.SEVERITY_ERROR);
                    alertMessage = msgString;
                    renderAlertMessage = true;
                    displayValidationAlertMessage(alertMessage);
                    throw new ValidatorException(msg);
                }
            }
        } else {
            msgString = Messages.getString("configuration_msgIntegerNull");
            msg = new FacesMessage(msgString);
            //System.out.println(msgString);
            msg.setSeverity(FacesMessage.SEVERITY_ERROR);
            alertMessage = msgString;
            renderAlertMessage = true;
            displayValidationAlertMessage(alertMessage);
            throw new ValidatorException(msg);
        }
        
        if ((alertMessage == null) || ( renderAlertMessage == false)) {
            resetAlerts();
        }
        
    }
    
    @SuppressWarnings("unchecked")
    public String save(String[] targets) {
        logger.fine("save to targets...");
        if ( alertMessage!=null && renderAlertMessage==true ) {
            resetAlerts();
            return null;
        }
        if ( targets==null || targets.length==0 ) {
            logger.fine("No targets specified.");
            return null;
        }
        saveConfigData();
        // iterate thru each target
        for (int i=0; i<targets.length; i++) {
            logger.info("saving to target: "+targets[i]);
            ConfigurationService service = getConfigurationService(targets[i]);
            saveConfigService(service);
        }
        
        // display success alert
        displaySavedAlertMessage();
        
        //resetAlerts();
        return GenericConstants.SUCCESS;
    
    }    
    
    private void displayAlertMessage(String type, String summary, String msg) {
        setAlertType(type);
        setAlertSummary(summary);
        setRenderAlertMessage(true);
        setAlertMessage(msg);
    }
    
    private void displayValidationAlertMessage(String msg) {
        displayAlertMessage(ALERT_TYPE_ERROR,Messages.getString("configuration_alert_validation"),msg);
    }    

    private void displayStatusAlertMessage() {
        displayAlertMessage(ALERT_TYPE_ERROR,Messages.getString("configuration_alert"),
                Messages.getString("configuration_msgComponentDown"));
    }    
    
    private void displaySavedAlertMessage() {
        displayAlertMessage(ALERT_TYPE_SUCCESS,Messages.getString("configuration_alert_save_success"),"");
    }
    
    
    @SuppressWarnings("unchecked")
    public String save() {
        logger.fine("save...");
        if ( alertMessage!=null && renderAlertMessage==true ) {
            resetAlerts();
            return null;
        }
        saveConfigData();
        saveConfigService(configService);
        displaySavedAlertMessage();
        //resetAlerts();
        return GenericConstants.SUCCESS;
    }

    
    @SuppressWarnings("unchecked")
    public void saveConfigData() {
        
        // 1. first collect UI data
        Map map = new HashMap<String,PropertySheetSection>();    // contains <name,propertySheetSectionMap>
        
        if ( propertySheet!=null ) {
            List propSheetSectionList = propertySheet.getChildren();
            for ( Iterator iter=propSheetSectionList.iterator(); iter.hasNext(); ) {
                PropertySheetSection propSheetSection = (PropertySheetSection)iter.next();
                Map pmap = new LinkedHashMap<String,Object>();       // contains field names and values
                map.put(propSheetSection.getLabel(),pmap);
                List propList = propSheetSection.getChildren();
                for ( Iterator iter1=propList.iterator(); iter1.hasNext(); ) {
                    Property property = (Property)iter1.next();
                    String name = property.getLabel();
                    DisplayInformation info = null;
                    if (name != null) {
                        info = this.labelDisplayMap.get(name);
                    }
                    if (info != null) {
                        name = info.getAttributeName();
                    }
                    for (Iterator iter2=property.getChildren().iterator(); iter2.hasNext(); ) {
                        UIInput comp = (UIInput)iter2.next();
                        Object value = comp.getValue();
                        pmap.put(name,value);
                    }
                }
            }
        }
        
        // 2. update configData
        if ( configData !=null ) {
            for ( Iterator iterator = configData.entrySet().iterator();  iterator.hasNext(); ) {
                Map.Entry prop = (Map.Entry)iterator.next();
                String propKey = (String)prop.getKey();
                Object propValue = (Object)prop.getValue();
                Object value = null;
                if ( propValue instanceof Boolean ) {
                    value = findValue(map,propKey);
                }  else if ( propValue instanceof TabularData ) {
                    Map m = findValues(map,propKey);
                    if ( m!=null ) {
                        setTabularDataProperty((TabularData)propValue,m);
                    }
                } else {
                    value = findValue(map,propKey);
                }
                if ( value!=null ) {
                    prop.setValue(value);
                }
            }
        }
        
    }
    
    
    public void saveConfigService(ConfigurationService configService) {
        // 3. update config service
        if ( configData!=null ) {
            logger.fine("updating config service...");
            String name = Util.mapComponentValue(cName,componentName);
            String type = Util.mapComponentValue(cType,componentType);
            configService.setConfigurationProperties(name,type,configData);
        }
    }

    
    /**
     * save configuration changes to list of targets which is stored in the session variable
     *  
     *
     */
    public String saveConfigs() {
        logger.info("saveConfigs....");
        // retrieve list of targets from session variable
        //String[] targets = (String[])getParameter(TARGET_LIST_KEY);        // $NON-NLS-1$
        // get targets from listbox
        String[] targets = selectedTargetOptions;
        save(targets);
        return "";
    }

    
    ///////////////////////////// 
    //      unit tests  
    /////////////////////////////
    
    public String testSaveConfigs() {
        System.out.println("testSaveConfigs....");
        String[] targets = { "server", "domain" };
        // store list of targets to session varaible
        setParameter(TARGET_LIST_KEY,targets);
        saveConfigs();
        return "";
    }
    /**

     * save configuration changes to list of targets which is stored in the session variable

     */


    /**
     * return a list of target instances for the listbox
     */
    public Option[] getTargetOptions() {
        // get list from session
        Option[] targetOptions = (Option[])getParameter(INSTANCES_LIST_KEY);
        logger.info("targetOptions: "+targetOptions);
        return targetOptions;
    }
    

    /**
     * set the list of target instances for the listbox
     */
    public void setTargetOptions(Option[] options) {
        logger.info("setting target options: "+options);
        targetOptions = options;
    }    
    

    /**
     * set the selected target instances options from the listbox
     */
    public void setSelectedTargetOptions(String[] selections) {
        logger.info("selections: "+selections);
        selectedTargetOptions = selections;
    }


    /**
     * get the selected target instances options from the listbox
     */
    public String[] getSelectedTargetOptions() {
        if ( selectedTargetOptions==null ) {
            selectedTargetOptions = new String[]{ (String)getParameter(SELECTED_INSTANCE_KEY) } ;
        }
        logger.info("get selections: "+selectedTargetOptions);
        return selectedTargetOptions;
    }
    

}
