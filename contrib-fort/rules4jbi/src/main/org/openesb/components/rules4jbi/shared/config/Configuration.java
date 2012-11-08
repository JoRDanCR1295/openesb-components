/*
 * @(#)Configuration.java        $Revision: 1.4 $ $Date: 2008/11/11 00:25:29 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.config;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

/**
 * This class is used to create, save, and load a service unit's configuration data.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/11 00:25:29 $
 * 
 * @since 0.1
 */
@XmlRootElement(name="config", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
public class Configuration implements Saveable {
    
    @XmlElement(name="rule-service-provider", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    private String ruleServiceProvider;
    
    @XmlElement(name="rule-service-provider-class", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    private String ruleServiceProviderClass;
    
    @XmlElement(name="ruleset-file", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    private String rulesetFile;

    @XmlElement(name="wsdl-file", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    private String wsdlFile;
    
    //TODO: should be a Set rather than a List
    @XmlElementWrapper(name="classes", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    @XmlElement(name="class-name", namespace="http://www.milanfort.com/xml/ns/jbi/rules/configuration")
    private List<String> classes;
    
    @XmlTransient
    private final PropertyChangeSupport propertyChangeSupport;
    
    public Configuration() {
        ruleServiceProvider = "";
        ruleServiceProviderClass = "";
        rulesetFile = "";
        wsdlFile = "";
        classes = new ArrayList<String>();
        
        propertyChangeSupport = new PropertyChangeSupport(this);
    }
    
    public static Configuration load(InputStream inputStream) throws InvalidConfigurationException {
        if (inputStream == null) {
            throw new NullPointerException("Input stream cannot be null");
        }

        try {
            JAXBContext context = JAXBContext.newInstance(Configuration.class);

            Unmarshaller unmarshaller = context.createUnmarshaller();

            Configuration result = (Configuration) unmarshaller.unmarshal(inputStream);

            return result;
            
//        } catch (JAXBException e) {
//            throw new InvalidConfigurationException(e);
            
        } catch (Exception e) {
            throw new InvalidConfigurationException(e);
        }
    }

    public void save(OutputStream outputStream) throws SaveFailedException {
        if (outputStream == null) {
            throw new NullPointerException("Output stream cannot be null");
        }
        
        try {
            JAXBContext context = JAXBContext.newInstance(this.getClass());
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty("jaxb.formatted.output", Boolean.TRUE);

            marshaller.marshal(this, outputStream);

        } catch (JAXBException e) {
            throw new SaveFailedException(e);
        }
    }
    
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }
    
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }
    
    public String getRuleServiceProvider() {
        return ruleServiceProvider;
    }

    public void setRuleServiceProvider(final String ruleServiceProvider) {
        if (ruleServiceProvider == null) {
            throw new NullPointerException("Rule service provider must not be null");
        }
        
        /* No event is fired if old and new values are equal */
        propertyChangeSupport.firePropertyChange(
                "ruleServiceProvider", this.ruleServiceProvider, ruleServiceProvider);

        this.ruleServiceProvider = ruleServiceProvider;
    }

    public String getRuleServiceProviderClass() {
        return ruleServiceProviderClass;
    }

    public void setRuleServiceProviderClass(final String ruleServiceProviderClass) {
        if (ruleServiceProviderClass == null) {
            throw new NullPointerException("Rule service provider class must not be null");
        }

        propertyChangeSupport.firePropertyChange(
                "ruleServiceProviderClass", this.ruleServiceProviderClass, ruleServiceProviderClass);

        this.ruleServiceProviderClass = ruleServiceProviderClass;
    }

    public String getRulesetFile() {
        return rulesetFile;
    }

    public void setRulesetFile(final String rulesetFile) {
        if (rulesetFile == null) {
            throw new NullPointerException("Ruleset file must not be null");
        }

        propertyChangeSupport.firePropertyChange("rulesetFile", this.rulesetFile, rulesetFile);

        this.rulesetFile = rulesetFile;
    }

    public String getWSDLFile() {
        return wsdlFile;
    }

    public void setWSDLFile(final String wsdlFile) {
        if (wsdlFile == null) {
            throw new NullPointerException("WSDL file must not be null");
        }
        
        propertyChangeSupport.firePropertyChange("wsdlFile", this.wsdlFile, wsdlFile);

        this.wsdlFile = wsdlFile;
    }

    public List<String> getClasses() {
        return classes;
    }

    public void setClasses(final List<String> classes) {
        if (classes == null) {
            throw new NullPointerException("Classes must not be null");
        }
        
        propertyChangeSupport.firePropertyChange("classes", this.classes, classes);
        
        this.classes = classes;
    }
    
    public String toXML() {
        return toXML(true);
    }

    public String toXML(boolean formattedOutput) {
        try {
            StringWriter writer = new StringWriter();
            
            JAXBContext context = JAXBContext.newInstance(this.getClass());
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty("jaxb.formatted.output", formattedOutput);

            marshaller.marshal(this, writer);
            
            return writer.toString();

        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Configuration\n");
        String indent = "    ";
        
        sb.append(indent);
        sb.append("RuleServiceProvider: ");
        sb.append(ruleServiceProvider);
        sb.append("\n");

        sb.append(indent);
        sb.append("RuleServiceProviderClass: ");
        sb.append(ruleServiceProviderClass);
        sb.append("\n");

        sb.append(indent);
        sb.append("RulesetFile: ");
        sb.append(rulesetFile);
        sb.append("\n");

        sb.append(indent);
        sb.append("WSDLFile: ");
        sb.append(wsdlFile);
        sb.append("\n");

        sb.append(indent);
        sb.append("Classes: ");
        sb.append(classes);
        sb.append("\n");
        
        return sb.toString();
    }
    
//    public static void main(String[] args) throws FileNotFoundException, SaveFailedException, InvalidConfigurationException {
//        Configuration config = new Configuration();
//        
//        config.setRuleServiceProvider("org.jcp.jsr94.jess");
//        config.setRuleServiceProviderClass("org.jcp.jsr94.jess.RuleServiceProviderImpl");
//        config.setRulesetFile("data/ruleset.xml");
//        config.setWSDLFile("data/rules.wsdl");
//        
//        List<String> classes = new ArrayList<String>();
//        classes.add(Integer.class.getName());
//        classes.add(Double.class.getName());
//        config.setClasses(classes);
//        
//        System.out.println(config);
//        System.out.print(config.toXML());
//        System.out.println();
//        
//        File file = new File("/tmp/config.xml");
//        config.save(new FileOutputStream(file));
//        
//        System.out.println("Loading configuration...");
//        Configuration config2 = Configuration.load(new FileInputStream(file));
//        System.out.println(config2);
//        System.out.println(config2.toXML());
//    }
}
