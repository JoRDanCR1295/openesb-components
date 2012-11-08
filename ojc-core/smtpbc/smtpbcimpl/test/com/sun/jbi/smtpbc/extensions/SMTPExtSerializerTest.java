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
 * @(#)SMTPExtSerializerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.StringWriter;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import junit.framework.*;
import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import org.xml.sax.EntityResolver;
import com.sun.jbi.smtpbc.packaging.WSDLConfigurations;
import com.sun.jbi.smtpbc.extensions.SMTPExtSerializer;
import org.xml.sax.InputSource;

/**
 *
 * @author sainath
 */
public class SMTPExtSerializerTest extends TestCase {
    
        ExtensibilityElement extension = null;
        PrintWriter pw = null;   
        SMTPExtSerializer instance = new SMTPExtSerializer();
        Definition def = null;
        QName elementType = null;
        Element el = null;
        QName bindingElementType = null;
        QName bindingOperationElementType = null;
        QName addressElementType = null;
        QName inputElementType = null;  
        Document doc = null;
        
    public SMTPExtSerializerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        
        Class parentType = null;
        ExtensionRegistry extReg = null;
        WSDLFactory wsdlFactory = null;
        WSDLReader reader = null;
        extReg = new ExtensionRegistry();
        QName elementType = null;
        Definition def = null;
        
        BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(new File("test/com/sun/jbi/smtpbc/packaging/wsdls/TestSmtp.wsdl")), "UTF-8"));
    	wsdlFactory = WSDLFactory.newInstance();
        reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(new CatalogResolver(new CatalogManager()));
        def = reader.readWSDL(new File("test/com/sun/jbi/smtpbc/packaging/wsdls/TestSmtp.wsdl").getAbsolutePath());
         
        bindingElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/smtp/", Constants.ELEM_BINDING);
        bindingOperationElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/smtp/", Constants.ELEM_OPERATION);
        inputElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/smtp/",Constants.ELEM_INPUT );
        addressElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/smtp/", "address");
                     
        try {
            InputSource is = new InputSource(br);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db;
            synchronized(dbf) {
                dbf.setNamespaceAware(true);
                db = dbf.newDocumentBuilder();
            }
            doc = db.parse(is);
            
        } catch(Exception e) {
            fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
        
        if (doc == null) {
           fail("Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of registerSerializer method, of class com.sun.jbi.smtpbc.extensions.SMTPExtSerializer.
     */
    /*public void testRegisterSerializer() {
        System.out.println("registerSerializer");
        
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        instance.registerSerializer(registry);
        
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    } */
    

    /**
     * Test of marshall method, of class com.sun.jbi.smtpbc.extensions.SMTPExtSerializer.
     */
    public void testMarshallForSMTPBinding() throws Exception {
        
        System.out.println("Testing marshall method for binding ");
        String expectedOutput = null;
        // 1. testing smtp:binding extensibility element
        extension = new SMTPBinding();
         
         expectedOutput = "<smtp:binding/>";
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
    
     public void testMarshallForSMTPOperation() throws Exception {
        
        System.out.println("Testing marshall method for operation ");
        String expectedOutput = null;
        
        // 1. testing smtp:binding extensibility element
        extension = new SMTPOperation();
         
         expectedOutput = "<smtp:operation/>";
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
     
     public void testMarshallForSMTPInput() throws Exception {
        
        System.out.println("Testing marshall method for <smtp:input/> ");
        String expectedOutput = null;
        // 1. testing smtp:binding extensibility element
        extension = new SMTPOperationInput();
         
         expectedOutput = "";
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         System.out.println("output is "+strwr.toString().trim());
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
     
     public void testMarshallForSMTPAddressWithLocation() throws Exception {
        
        System.out.println("Testing marshall method for smtp:address ");
        String expectedOutput = null;
        // 1. testing smtp:binding extensibility element
        extension = new SMTPAddress();
         
         expectedOutput = "<smtp:address location="+"\""+"mailto:someuser@localhost.com"+"\""+"/>";
         System.out.println("expectedoutput"+expectedOutput);
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         ((SMTPAddress)extension).setLocation(new  MailTo("mailto:someuser@localhost.com"));
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
     
     public void testMarshallForSMTPAddressWithServer() throws Exception {
        
        System.out.println("Testing marshall method for smtp:address with server  ");
        String expectedOutput = null;
        // 1. testing smtp:address extensibility element
         extension = new SMTPAddress();
         
         expectedOutput = "<smtp:address smtpserver="+"\""+"localhost"+"\""+"/>";
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         ((SMTPAddress)extension).setSMTPServer("localhost");
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
     
      public void testMarshallForSMTPAddress() throws Exception {
        
        System.out.println("Testing marshall method for smtp:address  ");
        String expectedOutput = null;
        // 1. testing smtp:address extensibility element
        extension = new SMTPAddress();
         
         expectedOutput = "<smtp:address location="+"\""+"mailto:someuser@localhost.com"+"\""+" smtpserver="+"\""+"localhost"+"\""+"/>";
         System.out.println("output file name "+expectedOutput);
         StringWriter strwr = new StringWriter();
         pw = new PrintWriter(strwr);
         ((SMTPAddress)extension).setLocation(new  MailTo("mailto:someuser@localhost.com"));
         ((SMTPAddress)extension).setSMTPServer("localhost");
         instance.marshall(null, null , extension, pw, def, null);
         pw.flush();
         System.out.println("output file name "+ strwr.toString().trim());
         assertEquals(expectedOutput , strwr.toString().trim());     

    }
    
   
    /**
     * Test of unmarshall method, of class com.sun.jbi.smtpbc.extensions.SMTPExtSerializer.
     */
      
    public void testUnmarshallForBinding() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:binding");
        result = instance.unmarshall(null, bindingElementType, xelem, null, null);
        assertTrue(result instanceof SMTPBinding);
        
       }
    
   public void testUnmarshallForOperation() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:operation");
        result = instance.unmarshall(null, bindingOperationElementType, xelem, null, null);
        assertTrue(result instanceof SMTPOperation);
        
       }    
   
   public void testUnmarshallForOperationInputWithAllAttributes() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:input");
        result = instance.unmarshall(null, inputElementType, xelem, null, null);
        assertTrue(result instanceof SMTPOperationInput);
        
       }    
   
   public void testUnmarshallForSMTPAddressWithAllAttributes() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:address");
        result = instance.unmarshall(null, addressElementType, xelem, null, null);
        assertTrue(result instanceof SMTPAddress);
        SMTPAddress smtpaddress  = (SMTPAddress)result;
        assertEquals("localhost", smtpaddress.getSMTPServer());
        assertTrue(smtpaddress.getLocation() instanceof MailTo);
        
       }    
   
   public void testUnmarshallForSMTPAddressWithSMTPServer() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:address");
        result = instance.unmarshall(null, addressElementType, xelem, null, null);
        assertTrue(result instanceof SMTPAddress);
        SMTPAddress smtpaddress  = (SMTPAddress)result;
        assertEquals("localhost", smtpaddress.getSMTPServer());
                
       }    
   
    public void testUnmarshallForSMTPAddressWithLocation() throws Exception {
        System.out.println("unmarshall");
        Element xelem = null;
        ExtensibilityElement result = null;
        SMTPExtSerializer instance = new SMTPExtSerializer();
        
        xelem = getElement(doc, "smtp:address");
        result = instance.unmarshall(null, addressElementType, xelem, null, null);
        assertTrue(result instanceof SMTPAddress);
        SMTPAddress smtpaddress  = (SMTPAddress)result;
        assertTrue(smtpaddress.getLocation() instanceof MailTo);
                
       }   
   
   private Element getElement(Node aNode, String elementName) {
       Element theOne = null;
       if (aNode.getNodeName().equalsIgnoreCase(elementName)) {
           return (Element) aNode;
       }
       
       NodeList children = aNode.getChildNodes();
       for (int ii = 0; ii < children.getLength(); ii++) {
           Node child = children.item(ii);
           if (child.getNodeName().equalsIgnoreCase(elementName)) {
               theOne = (Element) child;
               break;
           } else {
               theOne = getElement(child, elementName);
               if (theOne != null) {
                   break;
               }
           }
       }
       return theOne;
    }
}
