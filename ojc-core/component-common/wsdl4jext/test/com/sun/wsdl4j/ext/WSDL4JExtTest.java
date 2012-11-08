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
 * @(#)WSDLReader2Impl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.util.Collection;
import java.util.List;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;
import com.sun.wsdl4j.ext.impl.DefinitionEx;
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;

/**
 * JUnit test class for <code>WSDLReadeEx</code> class.
 *  
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public class WSDL4JExtTest {

    static void ttestWSDL4J() {
        try {
            WSDLFactory wsdlFactory = WSDLFactory.newInstance();
            WSDLReader wsdlReader = wsdlFactory.newWSDLReader();
            Definition wsdlDef =
                wsdlReader.readWSDL(
                        "c:/Downloads/test/AlarmIRP/AlarmIRPProxy.wsdl");
            Message message =
                wsdlDef.getMessage(
                        new QName(
                                "http://services/fm", "getAlarmsBySeverity10x1k"));
            List<Part> partList = message.getOrderedParts(null);
            System.out.println("part count = " + partList.size());
        } catch (WSDLException e) {
            e.printStackTrace();
        }
    }
    
    static void ttestWSDL4JEXT() {
        try {
            WSDLReader wsdlReader = WSDL4JExt.newWSDLReader(null);
            Definition wsdlDef = wsdlReader.readWSDL(
                        "C:/Downloads/test/AlarmIRP/Partners/AlarmIRP/AlarmIRPPortTypeWrapper.wsdl");
            
            Collection<MessageProperty> properties = 
                WSDL4JExt.getMessageProperties(wsdlDef);
            
            Collection<MessagePropertyAlias> aliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef);
            
            Collection<PartnerLinkType> plts =
                WSDL4JExt.getPartnerLinkTypes(wsdlDef);
            
            SchemaTypeLoader loader = WSDL4JExt.getSchemaTypeLoader(wsdlDef);
            
            Message msg =
                wsdlDef.getMessage(
                        new QName("http://services/fm", "getAlarmsBySeverity10x1k"));
            
            SchemaGlobalElement elem = loader.findElement(msg.getPart("parameters").getElementName());
            
            System.out.println("good");
        } catch (WSDLException e) {
            e.printStackTrace();
        }
    }
    
    public static void main(String[] argv) {
        ttestWSDL4JEXT();
    }
}