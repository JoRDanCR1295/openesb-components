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
 * @(#)DOMJCOTransformerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sap.mw.jco.IFunctionTemplate;
import com.sap.mw.jco.JCO;
import com.sun.jbi.sapbc.testutils.DataBlob;
import com.sun.jbi.sapbc.testutils.JUnitSAPRepository;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLDefinitions;
import java.io.File;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.jmock.Mock;
import org.jmock.MockObjectTestCase;
import org.xml.sax.InputSource;

/**
 *
 * @author jknight
 */
public class DOMJCOTransformerTest extends MockObjectTestCase {
    
    DOMJCOTransformer instance = null;
    static String DATADIR = "test/com/sun/jbi/sapbc/data";
    static String DATAEXT = "domjco";
    
    public DOMJCOTransformerTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        Mock wsdlDefinition = mock(WSDLDefinitions.class);
        
        instance = new DOMJCOTransformer((WSDLDefinitions)wsdlDefinition.proxy());
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(DOMJCOTransformerTest.class);
        
        return suite;
    }
    
    /**
     * Test of transform method of class com.sun.jbi.sapbc.DOMJCOTransformer.
     */
    public void testTransform() {
        System.out.println("Testing transform");
        final List<File> dataFiles = SAPWSDLUtilities.listResourceFiles(new File(DATADIR), DATAEXT);
        List<DataBlob> blobs = new ArrayList<DataBlob>();
        DataBlob blob = null;
        Iterator blobIt = null;
        
        for (Iterator<File> data = dataFiles.iterator(); data.hasNext(); ) {
            try {
                blobs.add(new DataBlob((File) data.next()));
                blobIt = blobs.listIterator();
            } catch (Exception ex) {
                ex.printStackTrace();
                String errMsg = "testTransform: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
                fail(errMsg);
            }
        }
        
        System.out.println("Found ["+blobs.size()+"] data blobs");
        while (blobIt.hasNext()) {
            try {
                blob = (DataBlob) blobIt.next();
                JCO.Request req = createJCORequest(blob.getBapiName());
                String expected = blob.getJCOData().replaceAll("\r","").replaceAll("\n","").replaceAll(" ","");
                Node nmrData = createNMRNode(blob.getXMLData());
                instance.transform(req, nmrData);
                String actual = req.toXML();
                assertEquals(expected,actual);
           } catch (Exception ex) {
                ex.printStackTrace();
                String errMsg = "testTransform: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
                fail(errMsg);
            }
        }
        
        
        System.out.println("Successfully tested transform");
    }
    
    /**
     * Create the JCO.Request Object
     */
    public JCO.Request createJCORequest(String rfmName) {
        JCO.Request retRec = null;
        JUnitSAPRepository sapRepository = new JUnitSAPRepository("JUnitSAPRepository");
        IFunctionTemplate ft = sapRepository.getFunctionTemplate(rfmName);
        
        if (ft == null) {
            System.out.println("Unable to find SAP RFM " + rfmName);
        } else {
            retRec = ft.getRequest();
            if (retRec == null) {
                String errMsg = "createJCORequest: Unable to instantiate SAP RFM " + rfmName;
                fail(errMsg);
            }
        }        
        return retRec; 
    }
    
    /**
     * Create the Node Object
     */
    public Node createNMRNode(String data) {
        Node retNode = null;
        
        WSDLDefinitions def = null;
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            
            StringReader objRd = new StringReader(data);
            
            InputSource objIs = new InputSource(objRd);
            
            Document doc = builder.parse(objIs);
            retNode = doc.getDocumentElement();
            
            //SAPWSDLUtilities.nodetostring(retNode);
            SAPWSDLUtilities.doctostring(retNode.getOwnerDocument());
            System.out.println("Node type ["+retNode.getNodeType()+"] local name ["+retNode.getLocalName()+"]");
            System.out.println("Node string ["+retNode.toString()+"]");
            
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "getNMRNode: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        
        return retNode;
        
    }
    
}
