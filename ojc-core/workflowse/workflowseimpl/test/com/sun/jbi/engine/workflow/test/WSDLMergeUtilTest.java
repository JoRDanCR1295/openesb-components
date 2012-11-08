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
 * @(#)$Id: WSDLMergeUtilTest.java,v 1.2 2010/02/15 19:25:02 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.test;

import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;

import org.w3c.dom.Document;

import com.sun.jbi.engine.workflow.util.WSDLMergeUtil;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.engine.workflow.xpath.test.XpathTestUtil;

import junit.framework.TestCase;

public class WSDLMergeUtilTest extends TestCase{ 
    public void testMergeWSDL () throws Exception {
         final String TASK_WSDL_FILE_NAME = "data/ApprovePurchase_TM_BPEL_Import.wsdl";
         Definition def = XpathTestUtil.loadDefinition(TASK_WSDL_FILE_NAME);
         WSDLFactory factory = WSDLFactory.newInstance();
         Definition newDef = factory.newDefinition();
         
         WSDLMergeUtil.mergeWSDL(newDef, def);
         WSDLWriter writer =factory.newWSDLWriter();
         Document  wsdlDefDoc = writer.getDocument(newDef);
         
         String xml = XmlUtil.toXmlPretty(wsdlDefDoc.getDocumentElement(),"UTF-8", true);      
         System.out.println (xml);         
    }
}
