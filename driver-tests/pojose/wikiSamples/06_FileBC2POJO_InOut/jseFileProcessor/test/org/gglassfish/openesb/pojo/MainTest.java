package org.gglassfish.openesb.pojo;

import org.netbeans.modules.soa.tedmet.core.DomDiff;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.StringWriter;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.w3c.dom.Document;


public class MainTest  {

     public MainTest() {
     }

     @BeforeClass
     public static void setUpClass() throws Exception {
     }

     @AfterClass
     public static void tearDownClass() throws Exception {
     }

     @Before
     public void setUp() {
     }

     @After
     public void tearDown() {
     }

     @Test
     public void testMain() throws Exception {
         	System.out.println("main");
    // Variables
        boolean bResult;
        DomDiff differ;
        String masterXMLFileName;
        BufferedWriter output;
        Document sDoc;
        StringWriter sw;
        Document tDoc;
        File tempFile;
        TransformerFactory tff;
        Transformer transformer;
        FileProcessor mapperClass = new FileProcessor();

    // Exception Handling
        try {
            // Initialize
                File srcFile = new File("test/org/gglassfish/openesb/pojo/NativeWarehouseOrderSample.xml");
                File targetFile = new File("test/org/gglassfish/openesb/pojo/PartnerWarehouseOrderSample.xml");

            // Load original XML file
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                factory.setValidating(false);
                sDoc = factory.newDocumentBuilder().parse(srcFile);
            // Call mapper to generate generatedXMLDoc
                tDoc = factory.newDocumentBuilder().newDocument();
                mapperClass.NativeWarehouseOrderSampleToPartnerWarehouseOrderSample(sDoc, tDoc);

            // Create a Transformer Factory to marshal the documents to Strings
                tff = TransformerFactory.newInstance();
                tff.setAttribute("indent-number", 4);
                transformer = tff.newTransformer();
                transformer.setOutputProperty(OutputKeys.INDENT, "yes");

            // Write generated XML to a Temp File
                sw = new StringWriter();
                transformer.transform(new DOMSource(tDoc), new StreamResult(sw));
                tempFile = File.createTempFile("generated", ".xml");
                output = new BufferedWriter(new FileWriter(tempFile));
                output.write(sw.toString());
                output.close();

            // Setup Visual Diff Information
                org.netbeans.modules.soa.tedmet.diff.ui.VisualDiffUtil.setup(targetFile.getAbsolutePath(), tempFile.getAbsolutePath());

            // Compare the two XML files
                differ = new DomDiff();
                differ.mOut = System.err;
                bResult = differ.diff("Source", targetFile, "Target", sw.toString());
                if (!bResult) {
                    fail("The source and target XML files do not match!  The target XML file has been saved to " + tempFile.getPath() + "Please go to Tools->Show Unit Test Diff." );
                }
        } catch (Exception E) {
            throw E;
        }
     }

 }
