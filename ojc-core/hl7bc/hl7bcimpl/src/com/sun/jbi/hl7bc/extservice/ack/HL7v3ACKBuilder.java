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
 * @(#)HL7v3ACKBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.extservice.ack;

import java.net.URL;
import java.net.URLClassLoader;
import java.lang.reflect.Method;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extservice.support.hl7v3.HL7V3TransmissionWrapperContext;

import static com.sun.jbi.hl7bc.util.JarUtil.*;

import org.w3c.dom.Document;


/*
 * This class builds and return HL7 v3 compliant ACK
 * @author Raghunadh Teegavarapu
 *
 */
public class HL7v3ACKBuilder implements HL7Constants {

    private static DocumentBuilder db = null;

    private static JAXBContext jaxbContext = null;

    private static Unmarshaller unmarshaller = null;

    private static Marshaller marshaller = null;
    
    private static URLClassLoader mUrlClassLoader = null;
    
    private String mAcceptACKXSDName;
    
    private String mAppACKXSDName;
    
    private Object mrootACKObj = null;
    
    private String mJarName = null;
    
    private HL7V3TransmissionWrapperContext mTransmissionWrapperContext;
    
    
    
    static {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            db = dbf.newDocumentBuilder();
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "true");
            } catch (Exception exc) {
            exc.printStackTrace();
        }
    }
    
    public HL7v3ACKBuilder(String xsdName, String jarName) {
        try{
            mAcceptACKXSDName =  getXSDNameWithoutSpecChar(xsdName);
            mJarName = jarName;
            mUrlClassLoader   =  getURLClassLoader(new URL("file", null, jarName));
            mrootACKObj       =  mUrlClassLoader.loadClass(getJarEntryName(jarName,mAcceptACKXSDName)).newInstance();
            jaxbContext       =  JAXBContext.newInstance( mrootACKObj.getClass());
            // create an Unmarshaller
            unmarshaller      =  jaxbContext.createUnmarshaller();
            // create a Marshaller
            marshaller        =  jaxbContext.createMarshaller();
        } catch(Exception ex){
            ex.printStackTrace();
        }
    }
    
    public void setTransmissionContext(HL7V3TransmissionWrapperContext context) {
        mTransmissionWrapperContext = context;
    }
    
    private Object getClassObject(String className)throws Exception {
       return mUrlClassLoader.loadClass(getJarEntryName(mJarName,className)).newInstance();
    }
    
    private void invokeMethod(Object classObject, String methodName, Class[] methodSignature, Object value) throws Exception {
        Method method = classObject.getClass().getMethod(methodName, methodSignature);
        if(method != null){
            method.invoke(classObject, new Object[]{value});
        }
    }
    
    private String getXSDNameWithoutSpecChar(String xsdName) {
        try {
            if (!nonEmptyString(xsdName))
                return null;
            if (xsdName.endsWith(".xsd")) {
                xsdName.substring(0, xsdName.length() - 4);
                xsdName.replace("_", "");
            }
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        }
        return xsdName;
    }
    
    private boolean nonEmptyString(String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }
    
    public Document buildACK() throws Exception {
        //set CS value for setProcessingCode
        // get CS object
        Object classObj = getClassObject("CS");
        // construct setCode method signature
        Class[] methodSignature = new Class[1];
        methodSignature[0] = (new String()).getClass();
        // invoke setCode method
        invokeMethod(classObj, "setCode", methodSignature, mTransmissionWrapperContext.getProcessingCode());
        // Invoke setProcessingCode method on the root class and set CS object.
        methodSignature[0] = classObj.getClass();
        invokeMethod(mrootACKObj, "setProcessingCode", methodSignature, classObj);
        
        //marshall
//      for invoking marshall on root class using jaxb context
        QName ackQName = new QName("urn:hl7-org:v3", "Message");
        JAXBElement ackElement = new JAXBElement(ackQName, mrootACKObj.getClass(), mrootACKObj);
        Document doc = db.newDocument();
        marshaller.marshal(ackElement, doc);
        return doc;
        
        
    }

}
