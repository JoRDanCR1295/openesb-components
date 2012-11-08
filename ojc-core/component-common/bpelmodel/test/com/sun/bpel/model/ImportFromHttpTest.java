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
 * @(#)ImportFromHttpTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.extensions.schema.Schema;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

import junit.framework.TestCase;

public class ImportFromHttpTest extends TestCase {

    private MockUpHttpServer _mockUpHttpServer;
    private Thread _mockUpHttpServerThread;
    
    protected void setUp() throws Exception {
        super.setUp();
        _mockUpHttpServer = new MockUpHttpServer();
        _mockUpHttpServerThread = new Thread(_mockUpHttpServer);
        _mockUpHttpServerThread.start();
        //Wait for server socket to be ready
        Thread.sleep(1000);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        _mockUpHttpServerThread.interrupt();
        if (_mockUpHttpServer.getException() != null) {
            //Re-throw the exception
            throw _mockUpHttpServer.getException();
        }
    }

    public void testImportFromHttp() {
        try {
            String fileName =
                "/com/sun/bpel/model/importFromHttpTest/newProcess.bpel";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
             
            String baseURI = uri.toString();
            
            BPELParseContext parseContext =
                new BPELParseContext.DefaultParseContext();
            ProjectBasedWSDLResolver wsdlLoader =
                ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(
                        baseURI, parseContext);
            parseContext.setWSDLResolver(wsdlLoader);
            
            ProjectBasedXSDResolver xsdResolver =
                ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(
                        baseURI, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            Reader reader = new FileReader(new File(uri));
            
            ParsingCaches caches = new ParsingCaches();
            parseContext.setCaches(caches);
            DeferredActionRegistry registry = new DeferredActionRegistry();
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document =
                BPELDocumentParseFactory.getInstance().load(
                        reader, parseContext);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
            assertNotNull(document);
            
            BPELProcess process = document.getDocumentProcess();
            assertNotNull(process);
            
            //check if all imports have valid model loaded
            List imports = process.getImports();
            Iterator it = imports.iterator();
            while(it.hasNext()) {
                Import imp = (Import) it.next();
                String location = imp.getLocation();
                Object importedObject = imp.getImportedObject();
                assertNotNull(importedObject);
                if(location.endsWith("xsd") || location.endsWith("xsd=1")
                        || location.endsWith("xsd=2")) {
                    assertTrue(importedObject instanceof XMLSchema);
                } else if(location.endsWith("wsdl")) {
                    assertTrue(importedObject instanceof WSDLDocument);
                }
            }
            
            //Assert XML schema component lookup
            SchemaTypeLoader loader = process.getSchemaTypeLoader();
            SchemaGlobalElement elem = loader.findElement(
                    new QName("http://webservice/", "echoOperation"));
            assertNotNull(elem);
            SchemaType xmlType = loader.findType(
                    new QName("http://myothernamespace", "yetAnotherComplexClass"));
            assertNotNull(xmlType);
            
        } catch(Exception ex) {
            fail(ex.getMessage());
        }
    }
    
    
    
    private static class MockUpHttpServer implements Runnable {

        private ServerSocket _serverSocket;
        private Exception _exception;
        private final String _xsd1;
        private final String _xsd2;
        
        public MockUpHttpServer() throws IOException, URISyntaxException {
            String fileName =
                "/com/sun/bpel/model/importFromHttpTest/NewWebServiceService.xsd_1.xsd";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
            File xsdFile1 = new File(uri);
            _xsd1 = getText(xsdFile1);
            fileName =
                "/com/sun/bpel/model/importFromHttpTest/NewWebServiceService.xsd_2.xsd";
            url = getClass().getResource(fileName);
            uri = url.toURI();
            File xsdFile2 = new File(uri);
            _xsd2 = getText(xsdFile2);
            
            _serverSocket = new ServerSocket(13283);
        }
        
        public void run() {
            try {
                while (!Thread.currentThread().isInterrupted()) {
                    Socket clientSocket = _serverSocket.accept();
                    PrintWriter out = new PrintWriter(
                            clientSocket.getOutputStream(), true);
                    BufferedReader in = new BufferedReader(
                            new InputStreamReader(
                                    clientSocket.getInputStream()));

                    String inputLine;
                    readLoop: while ((inputLine = in.readLine()) != null) {
                        if (inputLine.indexOf("?xsd=1") >= 0) {
                            clientSocket.shutdownInput();
                            sendResponse(out, _xsd1);
                            out.flush();
                            out.close();
                            break readLoop;
                        } else if (inputLine.indexOf("?xsd=2") >= 0) {
                            clientSocket.shutdownInput();
                            sendResponse(out, _xsd2);
                            out.flush();
                            out.close();
                            break readLoop;
                        }
                    }
                    clientSocket.close();
                }
            } catch (InterruptedIOException e) {
                //Ignore.  Interrupted purposely
            } catch (IOException e) {
                _exception = e;
            } finally {
                try {
                    _serverSocket.close();
                } catch (IOException e) {
                    //ignore
                }
            }
        }
        
        public Exception getException() {
            return _exception;
        }
        
        private void sendResponse(PrintWriter out, String response) {
            out.print("HTTP/1.1 200 OK");
            out.print("\r\n");
            out.print("Server: MockUp Server");
            out.print("\r\n");
            out.print("Last-Modified: Wed, 08 Jan 2007 23:11:55 GMT");
            out.print("\r\n");
            out.print("Content-Length: ");
            out.print(response.length());
            out.print("\r\n");
            out.print("Connection: close");
            out.print("\r\n");
            out.print("Content-Type: text/html; charset=UTF-8");
            out.print("\r\n");
            out.print("\r\n");
            out.print(response);
        }
    }

    /**
     * Reads the contents of the given UTF-8 encoded file as a string.
     *
     * @param file  the input file
     * @return the contents
     */
    public static String getText (File file)
        throws FileNotFoundException, IOException {
        if (file == null) {
            throw new NullPointerException("no file");
        }
        StringBuilder sb = new StringBuilder((int) file.length());
        Reader in;
        try {
            in = new InputStreamReader(
                new BufferedInputStream(new FileInputStream(file)),
                "UTF-8");
        } catch (UnsupportedEncodingException ue) {
            throw new RuntimeException("broken JRE, no UTF-8");
        }
        int count;
        char[] buf = new char[1024];
        while ((count = in.read(buf)) > 0) {
            sb.append(buf, 0, count);
        }
        in.close();
        return sb.toString();
    }
}
